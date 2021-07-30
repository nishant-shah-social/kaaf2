(ns kaaf.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.spec.alpha :as s]
            [kaaf.consumers.core :as consumer]
            kaaf.contracts.core
            [kaaf.data.mocks :as mock]
            [kaaf.dsl.core :as dsl]
            [mint.validation.mappings :as mvm]))

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

(defn init-user-profile []
  (let [user-profile (when-not (get @profiles [:app-id mock/test-app-id])
                       (dsl/action :create :sdk :profiles
                                   (mock/mock-payload-ctx
                                    (mock/make-app-config :localshiva mock/test-app-id)
                                    (mock/mock-new-user-params (mock/gen-uuid-str))
                                    (mock/mock-client-params mock/test-app-id)
                                    (mock/mock-time-params))))]
    (when user-profile
      (swap! profiles
             assoc
             [:app-id mock/test-app-id]
             user-profile))))


(comment
  (init-user-profile)
  (reset! profiles {})
  (reset! actions-taken-atom []))

(def create-issue-dsl
  {:action [:create :sdk :issue]
   :description "User creates an issue via sdk"
   :ctx {:op {:env :localshiva}
         :client {:client-type :android
                  :app-name "MyFirstTestApp"
                  :app-id "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2"
                  :api-key "8ea83b7703b51c0f8e42a87f0da40bbc"
                  :domain "nishanttesting.helpshift.mobi"}
         :entity {:message-text "Message from user"
                  :type "txt"
                  :platform-id mock/test-app-id
                  :profile-id "nishanttesting_profile_20200203143823731-749c12230be1bf4"
                  :timestamp (mock/gen-hsft-timestamp)
                  :headers {:x-hs-request-id (mock/gen-uuid-str)
                            :content-type "application/x-www-form-urlencoded"
                            }}
         :contract {:create_issue_by_customer :first_reply_to_issue_by_customer}}
   })


(comment
  (dsl/action create-issue-dsl)

  (let [ctx (:ctx create-issue-dsl)]
    #_(assoc (:entity ctx) :signature "signature is here")
    (dissoc (:entity ctx) :headers)))

;; Create issues using the registered profile
(defn create-issues! [num]
  (dotimes [_ num]
    (let [payload-ctx
          (mock/mock-payload-ctx
           (mock/make-app-config :localshiva mock/test-app-id)
           (mock/mock-user-profile-params (-> @profiles
                                              (get [:app-id mock/test-app-id])
                                              :body
                                              json/read-str
                                              (get "id")))
           (mock/mock-message-params)
           (mock/mock-client-params mock/test-app-id)
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


(defn get-event-type
  [event-map]
  (get-in event-map [:value "type"]))


(defn matching-event?
  [{{{action-id "x-hs-request-id"} :headers} :payload-ctx
    :as action-ctx-map}
   {{event-id "request_id"} :value
    :as event-map}]
  (= action-id event-id))


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


  ;; Emulate SDK creates issue, and then replies to it
  (let [{:keys [payload-ctx response]} (or (last @actions-taken-atom)
                                           (do (create-issues! 1)
                                               (last @actions-taken-atom)))
        zmap (assoc payload-ctx
                    :issue-id (get-in response ["id"]))
        reply-ctx (mock/mock-payload-ctx
                   zmap
                   (:request-params zmap)
                   (mock/mock-message-params)
                   (mock/mock-time-params))]
    (dsl/action :create :sdk :message
                reply-ctx))


  ;; Emulate SDK Create Issue -> SDK Reply to issue -> Agent Replies via xhr
  ;; -> Agent resolves the issue via xhr
  (let [{:keys [payload-ctx response]} (or (last @actions-taken-atom)
                                           (do (create-issues! 1)
                                               (last @actions-taken-atom)))
        ;;constructing zmap for [create sdk message]
        zmap (assoc payload-ctx
                    :issue-id (get-in response ["id"]))
        zmap (mock/mock-payload-ctx
              zmap
              (:request-params zmap)
              (mock/mock-message-params)
              (mock/mock-time-params))
        reply-response (dsl/action :create :sdk :message
                                   zmap)

        ;;creating zmap for [create anon login]
        zmap (assoc zmap
                    :request-params {:username "nishant.shah@helpshift.com"
                                     :password "helpshift"})
        zmap (assoc zmap
                    :headers {"user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0"})
        login-cookie-store (dsl/action :create :anon :login zmap)

        ;;creating zmap for [create xhr message]
        zmap (merge zmap login-cookie-store)
        zmap (assoc zmap
                    :request-params {:body "Epic Reply by the agent"
                                     :issue_id (get zmap :issue-id)})
        zmap (mock/append-headers zmap "content-type" "application/x-www-form-urlencoded")
        zmap (mock/append-headers zmap "X-Requested-With" "XMLHttpRequest")
        reply-agent-response (dsl/action :create :xhr :message zmap)

        ;;creating zmap for [update xhr issue]
        zmap (assoc zmap
                    :request-params {:id (get zmap :issue-id)
                                     :status 2})] ;;status 2 is for resolved
    (dsl/action :update :xhr :issue zmap))


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


(comment
  ;;chaining creation of issue and reply to an issue by customer
  (let [issue-ctx (mock/mock-payload-ctx
                   (mock/make-app-config :localshiva mock/test-app-id)
                   (mock/mock-user-profile-params (-> @profiles
                                                      (get [:app-id mock/test-app-id])
                                                      :body
                                                      json/read-str
                                                      (get "id")))
                   (mock/mock-message-params)
                   (mock/mock-client-params mock/test-app-id)
                   (mock/mock-time-params))

        issue (dsl/action :create :sdk :issue
                          issue-ctx)

        response-body (json/read-str
                       (:body issue))

        _ (swap! actions-taken-atom
                 conj
                 {:kaaf.contracts.core/action [:create :sdk :issue]
                  :payload-ctx issue-ctx
                  :response response-body
                  :contract {:repo :kaaf
                             :ids [:kaaf.contracts.core/create_issue_by_customer
                                   :kaaf.contracts.core/first_reply_to_issue_by_customer]}})

        message-params (mock/mock-message-params)
        message-ctx (mock/mock-payload-ctx
                     (mock/make-app-config :localshiva mock/test-app-id)

                     (mock/mock-user-profile-params (-> @profiles
                                                        (get [:app-id mock/test-app-id])
                                                        :body
                                                        json/read-str
                                                        (get "id")))
                     {"message-text" "I want to see this appear."}
                     (mock/mock-client-params mock/test-app-id)
                     (mock/mock-time-params)
                     {"type" "txt"}
                     {"refers" ""})
        message-ctx (assoc message-ctx :issue-id (get response-body "id"))
        message-response (dsl/action :create :sdk :message
                                     message-ctx)
        message-response-body (json/read-str
                               (:body message-response))]

    (swap! actions-taken-atom
           conj
           {:kaaf.contracts.core/action [:create :sdk :message]
            :payload-ctx message-ctx
            :response message-response-body
            :contract {:repo :kaaf
                       :ids [:kaaf.contracts.core/reply_to_issue_by_customer]}})))
