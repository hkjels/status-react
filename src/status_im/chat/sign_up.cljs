(ns status-im.chat.sign-up
  (:require [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [status-im.components.styles :refer [default-chat-color]]
            [status-im.utils.utils :refer [http-post]]
            [status-im.utils.random :as random]
            [status-im.utils.sms-listener :refer [add-sms-listener
                                                  remove-sms-listener]]
            [status-im.constants :as const]
            [status-im.chat.constants :as chat-const]
            [status-im.i18n :refer [label]]
            [clojure.string :as s]))

(defn send-console-message [text]
  {:message-id   (random/id)
   :from         "me"
   :to           const/console-chat-id
   :content      text
   :content-type const/text-content-type
   :outgoing     true})

;; todo fn name is not too smart, but...
(defn command-content
  [command content]
  {:command (name command)
   :content content})

;; -- Send phone number ----------------------------------------
(defn on-sign-up-response [& [message]]
  (let [message-id (random/id)]
    (dispatch [:received-message
               {:message-id   message-id
                :content      (command-content
                               :confirmation-code
                               (or message (label :t/confirmation-code)))
                :content-type const/content-type-command-request
                :outgoing     false
                :chat-id      const/console-chat-id
                :from         const/console-chat-id
                :to           "me"}])))

(defn start-listening-confirmation-code-sms []
  (dispatch [:request-permissions
             [:receive-sms]
             (fn []
               (let [listener (add-sms-listener
                               (fn [{body :body}]
                                 (when-let [matches (re-matches #"(\d{4})" body)]
                                   (dispatch [:sign-up-confirm (second matches)]))))]
                 (dispatch [:start-listening-confirmation-code-sms listener])))]))

(defn stop-listening-confirmation-code-sms [db]
  (when-let [listener (:confirmation-code-sms-listener db)]
    (remove-sms-listener listener)
    (dissoc db :confirmation-code-sms-listener)))

;; -- Send confirmation code and synchronize contacts---------------------------
(defn on-sync-contacts []
  (dispatch [:received-message
             {:message-id   (random/id)
              :content      (label :t/contacts-syncronized)
              :content-type const/text-content-type
              :outgoing     false
              :chat-id      const/console-chat-id
              :from         const/console-chat-id
              :to           "me"}])
  (dispatch [:set-signed-up true]))

(defn sync-contacts []
  ;; TODO 'on-sync-contacts' is never called
  (dispatch [:request-permissions
             [:read-contacts]
             #(dispatch [:sync-contacts on-sync-contacts])]))

(defn on-send-code-response [body]
  (dispatch [:received-message
             {:message-id   (random/id)
              :content      (:message body)
              :content-type const/text-content-type
              :outgoing     false
              :chat-id      const/console-chat-id
              :from         const/console-chat-id
              :to           "me"}])
  (let [status (keyword (:status body))]
    (when (= :confirmed status)
      (dispatch [:stop-listening-confirmation-code-sms])
      (sync-contacts)
      ;; TODO should be called after sync-contacts?
      (dispatch [:set-signed-up true]))
    (when (= :failed status)
      (on-sign-up-response (label :t/incorrect-code)))))

(def start-signup-events
  [[:received-message
    {:message-id   (random/id)
     :content      (command-content
                    :phone
                    (label :t/phone-number-required))
     :content-type const/content-type-command-request
     :outgoing     false
     :chat-id      const/console-chat-id
     :from         const/console-chat-id
     :to           "me"}]
   [:received-message
    {:message-id   (random/id)
     :content      (label :t/shake-your-phone)
     :content-type const/text-content-type
     :outgoing     false
     :chat-id      const/console-chat-id
     :from         const/console-chat-id
     :to           "me"}]])

;; -- Saving password ----------------------------------------
(defn account-generation-message []
  (dispatch [:received-message
             {:message-id   chat-const/crazy-math-message-id
              :content      (label :t/account-generation-message)
              :content-type const/text-content-type
              :outgoing     false
              :chat-id      const/console-chat-id
              :from         const/console-chat-id
              :to           "me"}]))

(defn move-to-internal-failure-message []
  (dispatch [:received-message
             {:message-id   chat-const/move-to-internal-failure-message-id
              :content      (command-content
                             :grant-permissions
                             (label :t/move-to-internal-failure-message))
              :content-type const/content-type-command-request
              :outgoing     false
              :chat-id      const/console-chat-id
              :from         const/console-chat-id
              :to           "me"}]))

(defn passphrase-messages-events [mnemonic signing-phrase crazy-math-message?]
  (into [[:received-message
          {:message-id   chat-const/passphrase-message-id
           :content      (if crazy-math-message?
                           (label :t/phew-here-is-your-passphrase)
                           (label :t/here-is-your-passphrase))
           :content-type const/text-content-type
           :outgoing     false
           :chat-id      const/console-chat-id
           :from         const/console-chat-id
           :to           "me"}]
         [:received-message
          {:message-id   (random/id)
           :content      mnemonic
           :content-type const/text-content-type
           :outgoing     false
           :chat-id      const/console-chat-id
           :from         const/console-chat-id
           :to           "me"}]
         [:received-message
          {:message-id   chat-const/signing-phrase-message-id
           :content      (label :t/here-is-your-signing-phrase)
           :content-type const/text-content-type
           :outgoing     false
           :chat-id      const/console-chat-id
           :from         const/console-chat-id
           :to           "me"}]
         [:received-message
          {:message-id   (random/id)
           :content      signing-phrase
           :content-type const/text-content-type
           :outgoing     false
           :chat-id      const/console-chat-id
           :from         const/console-chat-id
           :to           "me"}]]
        start-signup-events))

(def intro-status
  {:message-id   chat-const/intro-status-message-id
   :content      (label :t/intro-status)
   :from         const/console-chat-id
   :chat-id      const/console-chat-id
   :content-type const/content-type-status
   :outgoing     false
   :to           "me"})

(def intro-events
  [[:received-message intro-status]
   [:received-message-when-commands-loaded
    const/console-chat-id
    {:chat-id      const/console-chat-id
     :message-id   chat-const/intro-message1-id
     :content      (command-content
                    :password
                    (label :t/intro-message1))
     :content-type const/content-type-command-request
     :outgoing     false
     :from         const/console-chat-id
     :to           "me"}]])

(def console-chat
  {:chat-id      const/console-chat-id
   :name         (s/capitalize const/console-chat-id)
   :color        default-chat-color
   :group-chat   false
   :is-active    true
   :unremovable? true
   :timestamp    (.getTime (js/Date.))
   :photo-path   const/console-chat-id
   :contacts     [{:identity         const/console-chat-id
                   :text-color       "#FFFFFF"
                   :background-color "#AB7967"}]})

(def console-contact
  {:whisper-identity  const/console-chat-id
   :name              (s/capitalize const/console-chat-id)
   :photo-path        const/console-chat-id
   :dapp?             true
   :unremovable?      true
   :bot-url           "local://console-bot"
   :dapp-hash         858845357})
