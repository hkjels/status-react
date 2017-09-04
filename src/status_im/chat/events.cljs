(ns status-im.chat.events
  (:require [re-frame.core :refer [reg-fx reg-cofx inject-cofx trim-v]]
            [tailrecursion.priority-map :refer [priority-map-by]]
            [taoensso.timbre :as log]
            [status-im.utils.handlers :refer [register-handler-db register-handler-fx]]
            [status-im.utils.random :as random]
            [status-im.chat.models :as model]
            [status-im.chat.models.unviewed-messages :as unviewed-messages-model]
            [status-im.chat.sign-up :as sign-up]
            [status-im.chat.constants :as chat-const]
            [status-im.data-store.messages :as msg-store]
            [status-im.data-store.contacts :as contacts-store]
            [status-im.data-store.chats :as chats-store]
            [status-im.protocol.core :as protocol]
            [status-im.constants :as const]
            status-im.chat.events.input
            status-im.chat.events.commands
            status-im.chat.events.animation
            status-im.chat.events.receive-message))

;;;; Coeffects

(reg-cofx
 :random-id
 (fn [coeffects _]
   (assoc coeffects :random-id (random/id))))

(reg-cofx
 :stored-unviewed-messages
 (fn [cofx _]
   (assoc cofx :stored-unviewed-messages (msg-store/get-unviewed))))

(reg-cofx
 :get-stored-message
 (fn [cofx _]
   (assoc cofx :get-stored-message msg-store/get-by-id)))

(reg-cofx
 :get-stored-messages
 (fn [cofx _]
   (assoc cofx :get-stored-messages msg-store/get-by-chat-id)))

(reg-cofx
 :get-last-stored-message
 (fn [cofx _]
   (assoc cofx :get-last-stored-message msg-store/get-last-message)))

(reg-cofx
 :all-stored-chats
 (fn [cofx _]
   (assoc cofx :all-stored-chats (chats-store/get-all))))

;;;; Effects

(reg-fx
  :update-message
  (fn [message]
    (msg-store/update message)))

(reg-fx
  :save-message
  (fn [{:keys [chat-id] :as message}]
    (msg-store/save chat-id message)))

(reg-fx
  :save-chat
  (fn [chat]
    (chats-store/save chat)))

(reg-fx
  :save-all-contacts
  (fn [contacts]
    (contacts-store/save-all contacts)))

(reg-fx
  :protocol-send-seen
  (fn [params]
    (protocol/send-seen! params)))

;;;; Helper fns

(defn init-console-chat
  ([{:keys [chats] :accounts/keys [current-account-id] :as db} existing-account?]
   (let [new-chat sign-up/console-chat]
     (if (chats const/console-chat-id)
       {:db db}
       (cond-> {:db (-> db
                        (assoc :new-chat new-chat)
                        (update :chats assoc const/console-chat-id new-chat)
                        (assoc :current-chat-id const/console-chat-id))
                :dispatch-n [[:add-contacts [sign-up/console-contact]]]
                :save-chat new-chat
                :save-all-contacts [sign-up/console-contact]}

         (not current-account-id)
         (update :dispatch-n concat sign-up/intro-events)

         existing-account?
         (update :dispatch-n concat sign-up/start-signup-events))))))

;;;; Handlers

(register-handler-db
  :set-layout-height
  [trim-v]
  (fn [db [height]]
    (assoc db :layout-height height)))

(register-handler-db
  :set-chat-ui-props
  [trim-v]
  (fn [db [kvs]]
    (model/set-chat-ui-props db kvs)))

(register-handler-db
  :toggle-chat-ui-props
  [trim-v]
  (fn [db [ui-element]]
    (model/toggle-chat-ui-prop db ui-element)))

(register-handler-db
  :show-message-details
  [trim-v]
  (fn [db [details]]
    (model/set-chat-ui-props db {:show-bottom-info? true
                                 :show-emoji?       false
                                 :bottom-info       details})))

(register-handler-fx
  :load-more-messages
  [(inject-cofx :get-stored-messages)]
  (fn [{{:keys [current-chat-id loading-allowed] :as db} :db
        get-stored-messages :get-stored-messages} _]
    (let [all-loaded? (get-in db [:chats current-chat-id :all-loaded?])]
      (if loading-allowed
        (if all-loaded?
          {:db db}
          (let [messages-path [:chats current-chat-id :messages]
                messages      (get-in db messages-path)
                chat-messages (filter #(= current-chat-id (:chat-id %)) messages)
                new-messages  (get-stored-messages current-chat-id (count chat-messages))
                all-loaded?   (> const/default-number-of-messages (count new-messages))]
            {:db (-> db
                     (assoc :loading-allowed false)
                     (update-in messages-path concat new-messages)
                     (assoc-in [:chats current-chat-id :all-loaded?] all-loaded?))
             ;; we permit loading more messages again after 400ms
             :dispatch-later [{:ms 400 :dispatch [:set :loading-allowed true]}]}))
        {:db db}))))

(register-handler-db
  :set-message-shown
  [trim-v]
  (fn [db [{:keys [chat-id message-id]}]]
    (update-in db
               [:chats chat-id :messages]
               (fn [messages]
                 (map (fn [message]
                        (if (= message-id (:message-id message))
                          (assoc message :new? false)
                          message))
                      messages)))))

(register-handler-fx
  :init-console-chat
  (fn [{:keys [db]} _]
    (init-console-chat db false)))

(defn- compare-chats
  [{timesatmp1 :timestamp} {timestamp2 :timestamp}]
  (compare timestamp2 timesatmp1))

(register-handler-fx
  :initialize-chats
  [(inject-cofx :all-stored-chats) (inject-cofx :stored-unviewed-messages) (inject-cofx :get-last-stored-message)]
  (fn [{:keys [db all-stored-chats stored-unviewed-messages get-last-stored-message]} _]
    (let [{:accounts/keys [account-creation?]} db
          new-db (unviewed-messages-model/load-unviewed-messages db stored-unviewed-messages)
          event  [:load-default-contacts!]]
      (if account-creation?
        {:db new-db
         :dispatch-n [event]}
        (let [chats (->> all-stored-chats
                         (map (fn [{:keys [chat-id] :as chat}]
                                [chat-id (assoc chat :last-message (get-last-stored-message chat-id))]))
                         (into (priority-map-by compare-chats)))]
          (-> new-db
              (assoc :chats chats)
              (init-console-chat true)
              (update :dispatch-n conj event)))))))

(register-handler-fx
  :reload-chats
  [(inject-cofx :all-stored-chats) (inject-cofx :get-last-stored-message)]
  (fn [{:keys [db all-stored-chats get-last-stored-message]} _]
    (let [updated-chats (->> all-stored-chats
                             (map (fn [{:keys [chat-id] :as chat}]
                                    (let [prev-chat    (get (:chats db) chat-id)
                                          updated-chat     (assoc chat :last-message (get-last-stored-message chat-id))]
                                      [chat-id (merge prev-chat updated-chat)])))
                             (into (priority-map-by compare-chats)))]
      (-> (assoc db :chats updated-chats)
          (init-console-chat true)))))

(register-handler-fx
  :send-seen!
  [trim-v]
  (fn [{:keys [db]} [{:keys [message-id chat-id from]}]]
    (let [{:keys [web3 current-public-key chats]
           :contacts/keys [contacts]} db
          {:keys [group-chat public?]} (get chats chat-id)]
      (cond-> {:db (unviewed-messages-model/remove-unviewed-messages db chat-id)
               :update-message {:message-id      message-id
                                :message-status :seen}}
        (and (not (get-in contacts [chat-id] :dapp?))
             (not public?))
        (assoc :protocol-send-seen
               {:web3    web3
                :message (cond-> {:from       current-public-key
                                  :to         from
                                  :message-id message-id}
                           group-chat (assoc :group-id chat-id))})))))

(register-handler-fx
  :show-mnemonic
  [(inject-cofx :get-stored-message) trim-v]
  (fn [{:keys [get-stored-message]} [mnemonic signing-phrase]]
    (let [crazy-math-message? (get-stored-message chat-const/crazy-math-message-id)]
      {:dispatch-n (sign-up/passphrase-messages-events mnemonic
                                                       signing-phrase
                                                       crazy-math-message?)})))
