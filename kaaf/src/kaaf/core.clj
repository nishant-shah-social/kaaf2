(ns kaaf.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.spec.alpha :as s]
            [kaaf.consumers.core :as consumer]
            kaaf.contracts.core
            [kaaf.data.mocks :as mock]
            [kaaf.dsl.core :as dsl]
            [mint.validation.mappings :as mvm]
            [kaaf.contracts.sequence-validators :as kcs]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-memory persistent stores for various contexts and actions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defonce profiles
  (atom {}))

(defonce actions-taken-atom
  (atom []))

(defonce events-collected-atom
  (atom []))

(defonce actions-events-join-atom
  (atom []))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Business action runners and helpers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def create-user-profile-dsl
  {:action [:create :sdk :profiles]
   :description "creates a new user profile via sdk"
   :ctx {:op {:env :localshiva}
         :client (dsl/get-client-config)
         :entity  (assoc (mock/mock-new-user-params (mock/gen-uuid-str))
                         :platform-id (dsl/get-test-app-id)
                         :timestamp (mock/gen-hsft-timestamp)
                         :headers {:x-hs-request-id (mock/gen-uuid-str)
                                   :content-type "application/x-www-form-urlencoded"
                                   })
         :contract {:exactly-ordered [:create_issue_by_customer :first_reply_to_issue_by_customer]}}})


(defn init-user-profile []
  (let [user-profile (when-not (get @profiles [:app-id (dsl/get-test-app-id)])
                       (dsl/action create-user-profile-dsl))]
    (when user-profile
      (swap! profiles
             assoc
             [:app-id (dsl/get-test-app-id)]
             user-profile))))


(comment
  (init-user-profile)
  (reset! profiles {})
  (reset! actions-taken-atom [])
  (deref events-collected-atom)
  (reset! events-collected-atom [])
  (dsl/action create-user-profile-dsl)
  (dsl/get-client-config :localshiva :app-id (dsl/get-test-app-id))
  (dsl/get-client-config :localshiva :client-type :xhr))


;; sample wait-dsl action which will be used to perform wait between 2 actions
(def wait-dsl
  {:action [:wait :any :any]
   :description "waits for specified amount of milliseconds"
   :ctx {:op {:env :localshiva
              :ms 10000}}})


;; fetching all the events generated by an action
(defn matching-event?
  [{{action-id "x-hs-request-id"} :headers
    :as action-ctx-map}
   {{event-id "request_id"} :value
    :as event-map}]
  (= action-id event-id))


(defn get-event-type
  [event-map]
  (get-in event-map [:value "type"]))

;; joining relevant events to the corresponding action payload
;; Joined action-event payload will be further passed on for validations
(defn join-action-to-events
  [action-ctx events]
  (let [events-matched
        (->> events
             (filterv (partial matching-event? action-ctx))
             (map (fn [event-map]
                    (assoc event-map
                           :kaaf.contracts.core/dashboard-event
                           (get-event-type event-map)))))]
    (assoc action-ctx :events events-matched)))


;; Emulating the flow :- create issue via sdk -> user replying to the issue via sdk ->
;; Agent login via dashboard -> Agent replying to the issue -> Agent resolving issue
;; For each of the action, we are defining the DSL as the consumer of Kaaf will do in
;; .edn file
(comment
  (let [zmap [] ;;payloads(request as well as response) of all the actions will be punched into this zmap
        _ (reset! events-collected-atom [] ;;clearing events-collected-atom
                  )
        create-sdk-issue-dsl
        {:action [:create :sdk :issue]
         :description "User creates an issue via sdk"
         :ctx {:op {:env :localshiva}
               :client (dsl/get-client-config :localshiva :app-id (dsl/get-test-app-id))
               :entity {:message-text "Message from user"
                        :type "txt"
                        :platform-id (dsl/get-test-app-id)
                        :profile-id "nishanttesting_profile_20200203143823731-749c12230be1bf4"
                        :timestamp (mock/gen-hsft-timestamp)}}
         :contract {:exactly-ordered [:create_issue_by_customer :first_reply_to_issue_by_customer]}}
        response (dsl/action create-sdk-issue-dsl)
        issue-id (get (json/read-str (get-in response [:response :body])) "id")
        create-sdk-issue-dsl (merge create-sdk-issue-dsl response)
        zmap (conj zmap create-sdk-issue-dsl) ;;punch the whole action payload into the zmap

        _ (dsl/action wait-dsl)

        message-sdk-issue-dsl
        {:action [:create :sdk :message]
         :description "User sends a message to an issue"
         :ctx {:op {:env :localshiva}
               :client (dsl/get-client-config :localshiva :app-id (dsl/get-test-app-id))
               :entity {:message-text "Another message from user"
                        :type "txt"
                        :platform-id (dsl/get-test-app-id)
                        :profile-id "nishanttesting_profile_20200203143823731-749c12230be1bf4"
                        :timestamp (mock/gen-hsft-timestamp)
                        :issue-id issue-id}}
         :contract {:exactly-ordered [:reply_to_issue_by_customer]}}
        response (dsl/action message-sdk-issue-dsl)
        message-sdk-issue-dsl (merge message-sdk-issue-dsl response)
        zmap (conj zmap message-sdk-issue-dsl)

        _ (dsl/action wait-dsl)

        dashboard-login-dsl
        {:action [:create :anon :login]
         :description "agent logs in to the dashboard"
         :ctx {:op {:env :localshiva}
               :client (dsl/get-client-config :localshiva :client-type :webui)
               :entity {:username (dsl/get-agent-username :localshiva)
                        :password (dsl/get-agent-password :localshiva)}}
         :contract {:exactly-ordered [:login_agent]}}
        response (dsl/action dashboard-login-dsl)
        cookie-store (get response :cookies)
        response (dissoc response :cookie)
        dashboard-login-dsl (merge dashboard-login-dsl response)
        zmap (conj zmap dashboard-login-dsl)

        _ (dsl/action wait-dsl)

        xhr-message-client (dsl/get-client-config :localshiva :client-type :xhr)
        xhr-message-client (assoc xhr-message-client :cookie-store cookie-store)
        xhr-message-dsl
        {:action [:create :xhr :message]
         :description "agent sends message to an issue"
         :ctx {:op {:env :localshiva}
               :client xhr-message-client
               :entity {:body "Test message by agent"
                        :issue_id issue-id}}
         :contract {:each-once [:assign_issue_to_agent :reply_to_issue_by_agent]}} ;;order does not matter because this action results into two events at the same time
        response (dsl/action xhr-message-dsl)
        xhr-message-dsl (update-in xhr-message-dsl [:ctx :client] dissoc :cookie-store) ;;avoiding punching of cookie-store in the zmap below as it causes issue while reading the zmap
        xhr-message-dsl (merge xhr-message-dsl response)
        zmap (conj zmap xhr-message-dsl)

        _ (dsl/action wait-dsl)

        resolve-issue-client (dsl/get-client-config :localshiva :client-type :xhr)
        resolve-issue-client (assoc resolve-issue-client :cookie-store cookie-store)
        resolve-issue-dsl
        {:action [:update :xhr :issue]
         :description "agent updates the status of the issue"
         :ctx {:op {:env :localshiva}
               :client resolve-issue-client
               :entity {:status 2 ;;status 2 stands for resolving issue
                        :id issue-id}}
         :contract {:exactly-ordered [:resolve_issue]}}
        response (dsl/action resolve-issue-dsl)
        resolve-issue-dsl (update-in resolve-issue-dsl [:ctx :client] dissoc :cookie-store)
        resolve-issue-dsl (merge resolve-issue-dsl response)
        zmap (conj zmap resolve-issue-dsl)

        _ (dsl/action wait-dsl)

        ;; joining the corresponding actions and events
        action-events-join (for [action zmap]
                             (join-action-to-events action
                                                    @events-collected-atom))]

    ;; verifying the events sequence in the joined action-events payload
    (apply kcs/validate-kafka-events-for-actions action-events-join)
    ;;(dsl/action dashboard-login-dsl)
    ))


;;emulating a business scenario where
;;user creates issue via sdk -> agent logs in to the dashboard -> creates a new tag ->
;;agent opens that issue to view -> agent adds new tag to the issue
(comment
  (let [zmap [] ;;payloads(request as well as response) of all the actions will be punched into this zmap
        _ (reset! events-collected-atom [] ;;clearing events-collected-atom
                  )
        create-sdk-issue-dsl
        {:action [:create :sdk :issue]
         :description "User creates an issue via sdk"
         :ctx {:op {:env :localshiva}
               :client (dsl/get-client-config :localshiva :app-id (dsl/get-test-app-id))
               :entity {:message-text "Message from user"
                        :type "txt"
                        :platform-id (dsl/get-test-app-id)
                        :profile-id "nishanttesting_profile_20200203143823731-749c12230be1bf4"
                        :timestamp (mock/gen-hsft-timestamp)}}
         :contract {:exactly-ordered [:create_issue_by_customer :first_reply_to_issue_by_customer]}}
        response (dsl/action create-sdk-issue-dsl)
        issue-id (get (json/read-str (get-in response [:response :body])) "id")
        publish-id (get (json/read-str (get-in response [:response :body])) "publish_id")
        create-sdk-issue-dsl (merge create-sdk-issue-dsl response)
        zmap (conj zmap create-sdk-issue-dsl) ;;punch the whole action payload into the zmap

        _ (dsl/action wait-dsl)

        dashboard-login-dsl
        {:action [:create :anon :login]
         :description "agent logs in to the dashboard"
         :ctx {:op {:env :localshiva}
               :client (dsl/get-client-config :localshiva :client-type :webui)
               :entity {:username (dsl/get-agent-username :localshiva)
                        :password (dsl/get-agent-password :localshiva)}}
         :contract {:exactly-ordered [:login_agent]}}
        response (dsl/action dashboard-login-dsl)
        cookie-store (get response :cookies)
        response (dissoc response :cookie)
        dashboard-login-dsl (merge dashboard-login-dsl response)
        zmap (conj zmap dashboard-login-dsl)

        _ (dsl/action wait-dsl)

        xhr-client (dsl/get-client-config :localshiva :client-type :xhr)
        xhr-client (assoc xhr-client :cookie-store cookie-store)

        tag (mock/mock-tag-name)
        create-tag-dsl
        {:action [:create :xhr :tags]
         :description "agent/admin creates new tag via xhr"
         :ctx {:op {:env :localshiva}
               :client xhr-client
               :entity {:tags "[\"sampletagishere10\"]"}}}
        response (dsl/action create-tag-dsl)
        create-tag-dsl (update-in create-tag-dsl [:ctx :client] dissoc :cookie-store)
        create-tag-dsl (merge create-tag-dsl response)
        zmap (conj zmap create-tag-dsl)

        open-issue-dsl
        {:action [:read :xhr :issue-details]
         :description "agent reads/opens the issue via xhr"
         :ctx {:op {:env :localshiva}
               :client xhr-client
               :entity {:publish_id publish-id
                        :viewing true
                        :get_tmpl false
                        :show_faq_suggestion true
                        :issue_id issue-id
                        :issue_author_id "nishanttesting_profile_20200203143823731-749c12230be1bf4"
                        :user_email "groot+e748ee95-ca1f-4aa3-8f78-32abc27136cc@gmail.com"
                        :user_id "undefined"}}
         :contract {:exactly-ordered [:view_issue_by_agent]}}
        response (dsl/action open-issue-dsl)
        open-issue-dsl (update-in open-issue-dsl [:ctx :client] dissoc :cookie-store)
        open-issue-dsl (merge open-issue-dsl response)
        zmap (conj zmap open-issue-dsl)

        _ (dsl/action wait-dsl)

        insert-tag-dsl
        {:action [:update :xhr :issue]
         :description "agent adds a tag to the issue"
         :ctx {:op {:env :localshiva}
               :client xhr-client
               :entity {:tags "[\"sampletagishere10\"]" ;;tag to be added to the issue
                        :id issue-id}}
         :contract {:exactly-ordered [:add_tags_to_issue]
                    :never [:assign_issue_to_agent]}}
        response (dsl/action insert-tag-dsl)
        insert-tag-dsl (update-in insert-tag-dsl [:ctx :client] dissoc :cookie-store)
        insert-tag-dsl (merge insert-tag-dsl response)
        zmap (conj zmap insert-tag-dsl)

        _ (dsl/action wait-dsl)

        ;;joining the corresponding actions and events
        action-events-join (for [action zmap]
                             (join-action-to-events action
                                                    @events-collected-atom))]

    ;; verifying the events sequence in the joined action-events payload
    (apply kcs/validate-kafka-events-for-actions action-events-join)))


;; Create issues using the registered profile
(defn create-issues! [num]
  (dotimes [_ num]
    (let [payload-ctx
          (mock/mock-payload-ctx
           (dsl/get-app-config :localshiva (dsl/get-test-app-id))
           (mock/mock-user-profile-params (-> @profiles
                                              (get [:app-id (dsl/get-test-app-id)])
                                              :body
                                              json/read-str
                                              (get "id")))
           (mock/mock-message-params)
           (mock/mock-client-params (dsl/get-test-app-id))
           (mock/mock-time-params))
          create-issue!
          (dsl/action :create :sdk :issue
                      payload-ctx)
          response-body (json/read-str
                         (:body create-issue!))]
      (swap! actions-taken-atom
             conj
             {:kaaf.contracts.core/action [:create :sdk :issue]
              :payload-ctx payload-ctx
              :response response-body
              :contract {:repo :kaaf
                         :ids [:kaaf.contracts.core/create_issue_by_customer
                               :kaaf.contracts.core/first_reply_to_issue_by_customer]}}))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions and Event post-processors (filter, join, validate etc.)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn join-action-to-events!
  [action-ctx events]
  (let [events-matched
        (->> events
             (filterv (partial matching-event? action-ctx))
             (map (fn [event-map]
                    (assoc event-map
                           :kaaf.contracts.core/dashboard-event
                           (get-event-type event-map)))))]
    (swap! actions-events-join-atom
           conj
           (assoc action-ctx
                  :events events-matched))))


(defn validate-events
  [{events :events
    :as action-events-map}]
  (map (fn [event-map]
         (or (s/valid? :kaaf.contracts.core/event event-map)
             (s/explain-data :kaaf.contracts.core/event event-map)))
       events))


(defn validate-action-event-aggregate
  [actions-events]
  (map validate-events actions-events))


(comment
  mvm/codes->origins
  mvm/codes->types

  (consumer/restart-consumer! :dashboard-events
                              (consumer/make-message-copier
                               events-collected-atom))

  (init-user-profile) ;; TODO: return appropriate errors if create fails

  (create-issues! 1) ;; TODO: 403 on action + no events is a valid case,
  ;; and we should be able to specify a contract for that


  (doseq [action @actions-taken-atom]
    (join-action-to-events! action
                            @events-collected-atom))

  (map count [@actions-taken-atom
              @events-collected-atom
              @actions-events-join-atom])

  (every? true?
          (validate-action-event-aggregate
           @actions-events-join-atom))

  #_(reset! actions-taken-atom [])
  #_(reset! events-collected-atom [])
  #_(reset! actions-events-join-atom [])
  #_(consumer/stop-consumer! :dashboard-events)

  (map (fn [m] ((juxt identity mvm/codes->types)
                (get-in m [:value "type"])))
       @events-collected-atom)

  (map (fn [m] (get-in m [:value "request_id"]))
       @events-collected-atom)

  (let [{k "kafka_meta"
         r "request_id"
         o "origin"
         v "v"
         i "id"
         domain "domain"
         t "type"
         at "action_taken_at"
         da "data"
         g "generated_at"
         :as event} (map :value ((juxt (comp last butlast) last)
                                 @events-collected-atom))]
    event)

  (dsl/dsl-implemented? dsl/action
                        [[:create :sdk :issue]
                         [:create :sdk :profiles]
                         [:reply :sdk :issue]
                         [:delete :dashboard :issue]])

  (dsl/dsl-implemented? kaaf.contracts.core/action
                        [[:create :sdk :issue]
                         [:create :sdk :profile]
                         [:reply :sdk :issue]
                         [:delete :dashboard :issue]])

  ((juxt (comp count :implemented)
         (comp count :not-implemented))
   (dsl/dsl-implemented? dsl/action
                         kaaf.dsl.state-machine/all-possible-action-permutations)))


;;sample content of actions-taken-atom. just for reference
;;to be removed later
(comment
  {:kaaf.contracts.core/action [:create :sdk :issue],
   :payload-ctx
   {:client-type :android,
    :app-name "MyFirstTestApp",
    :app-id
    "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2",
    :api-key "8ea83b7703b51c0f8e42a87f0da40bbc",
    :domain "nishanttesting.helpshift.mobi",
    :env :localshiva,
    :request-params
    {"message-text"
     "Are you kaafing, 437dbd60-ef69-48e4-b72b-9ea69c8373b1, my friend?",
     "platform-id"
     "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2",
     "profile-id"
     "nishanttesting_profile_20200121102622300-2d57529a410d5d3",
     "timestamp" "1579708399.143"},
    :headers
    {"x-hs-request-id" "6274d822-e4ac-4ecd-8485-5f45e1b8b6d0"}},
   :response
   {"chat?" false,
    "body" "",
    "publish_id" "171",
    "id" "nishanttesting_issue_20200122155322161-1c2b7746d2b9b28",
    "updated_at" "2020-01-22T15:53:22.165Z",
    "status" 0,
    "title"
    "Are you kaafing, 437dbd60-ef69-48e4-b72b-9ea69c8373b1, my friend?",
    "in_bzhrs" true,
    "created_at" "2020-01-22T15:53:22.165Z",
    "messages"
    [{"origin" "mobile",
      "body"
      "Are you kaafing, 437dbd60-ef69-48e4-b72b-9ea69c8373b1, my friend?",
      "seen_event" {},
      "md_state" "sent",
      "author"
      {"id"
       "nishanttesting_profile_20200121102622300-2d57529a410d5d3",
       "name" "IAmGroot a27ec11a-c39e-4a38-8c71-df3b439b3bd4"},
      "id" "nishanttesting_message_20200122155322167-014e1c71cb9c9df",
      "issue_id"
      "nishanttesting_issue_20200122155322161-1c2b7746d2b9b28",
      "type" "txt",
      "meta" {},
      "created_at" "2020-01-22T15:53:22.165Z"}]},
   :contract
   {:repo :kaaf,
    :ids
    [:kaaf.contracts.core/create_issue_by_customer
     :kaaf.contracts.core/first_reply_to_issue_by_customer]}})
