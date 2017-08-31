(ns status-im.chat.events
  (:require [re-frame.core :refer [reg-fx reg-cofx inject-cofx trim-v]]
            [status-im.utils.handlers :refer [register-handler-db register-handler-fx]]
            [status-im.chat.models :as model]
            [status-im.data-store.messages :as msg-store]
            [status-im.constants :as const]
            status-im.chat.events.input
            status-im.chat.events.commands
            status-im.chat.events.animation))

;;;; Coeffects

(reg-cofx
 :get-persisted-message
 (fn [coeffects _]
   (assoc coeffects :get-persisted-message msg-store/get-by-id)))

(reg-cofx
 :get-persisted-messages
 (fn [coeffects _]
   (assoc coeffects :get-persisted-messages msg-store/get-by-chat-id)))

;;;; Effects

(reg-fx
  :update-persisted-message
  (fn [message]
    (msg-store/update message)))

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
  [(inject-cofx :get-persisted-messages)]
  (fn [{{:keys [current-chat-id loading-allowed] :as db} :db
        get-persisted-messages :get-persisted-messages} _]
    (let [all-loaded? (get-in db [:chats current-chat-id :all-loaded?])]
      (if loading-allowed
        (if all-loaded?
          {:db db}
          (let [messages-path [:chats current-chat-id :messages]
                messages      (get-in db messages-path)
                chat-messages (filter #(= current-chat-id (:chat-id %)) messages)
                new-messages  (get-persisted-messages current-chat-id (count chat-messages))
                all-loaded?   (> const/default-number-of-messages (count new-messages))]
            {:db (-> db
                     (assoc :loading-allowed false)
                     (update-in messages-path concat new-messages)
                     (assoc-in [:chats current-chat-id :all-loaded?] all-loaded?))
             ;; we permit loading more messages again after 400ms
             :dispatch-later [{:ms 400 :dispatch [:set :loading-allowed true]}]}))
        {:db db}))))
