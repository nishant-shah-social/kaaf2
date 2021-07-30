(ns kaaf.contracts.core
  (:require [clojure.spec.alpha :as s]
            [kaaf.dsl.core :as dsl]
            [mint.validation.mappings :as mvm]))

;;
;; All known event types, as defined centrally in Mint
;;

(def legal-event-type-code?
  (into #{} (vals mvm/types->codes)))


(def legal-event-origin-code?
  (into #{} (vals mvm/origins->codes)))


;;
;; Generic contract for standard Kafka event structure and critical fields
;;

(defn p99-SLA-met?
  "Event should appear in Kafka within 'delta-ms' of the parent action taken.
  Default to 10000 ms (10 sec). Assumes negligible clock drift."
  ([event-map]
   (p99-SLA-met? event-map
                 10000))
  ([{:strs [kafka_meta action_taken_at]
     :as event-map}
    delta-ms]
   (let [kafka-msg-ts-ms (get kafka_meta "msg_ts")]
     (-> (- kafka-msg-ts-ms action_taken_at)
         (<= delta-ms)))))


(defn mandatory-keys-present?
  [event-map]
  (let [must-have-keys ["kafka_meta" "action_taken_at" "generated_at" ; timestamps
                        "v", "type", "data",                          ; versioned event + payload
                        "id", "request_id", "origin", "domain"]       ; provenance tracers
        mandatory-key-found? (into #{} (keys event-map))]
    (every? mandatory-key-found?
            must-have-keys)))


(defn legal-event-origin?
  [event-map]
  (legal-event-origin-code?
   (get event-map "origin")))


(defn legal-event-type?
  [event-map]
  (legal-event-type-code?
   (get event-map "type")))


(s/def ::value
  (s/and mandatory-keys-present?
         legal-event-origin?
         legal-event-type?
         p99-SLA-met?))


(s/def ::kafka-event-base
  (s/keys :req-un [::value]))

;;
;; Per-event contracts
;;

(s/def ::dashboard-event
  mvm/codes->types)


(defmulti event
  "Dispatch validation to the multispec for the given type of contract."
  ::dashboard-event)


(s/def ::event
  (s/multi-spec event ::dashboard-event))


(defmethod event :default
  [_]
  ;; TODO: Decide if this should be (constantly false)
  ;; meaning, force the kaaf maintainer to explicitly add support
  ;; for event types that we expect to validate.
  ::kafka-event-base)


(defmethod event :create_model
  [_]
  ::kafka-event-base)


(defmethod event :create_issue_by_customer
  [_]
  ::kafka-event-base)


(letfn [(type-check-example-for-this-event
          [{value :value}]
          (constantly true))
        (semantic-check-example-within-this-event
          [{value :value}]
          #_(and (= (get-in value ["data" "issue" "state"]) 0) ; open
                 (nil? (get-in value ["data" "issue" "resolved_time"])))
          (constantly true))]

  (defmethod event :first_reply_to_issue_by_customer
    [_]
    (s/and semantic-check-example-within-this-event
           type-check-example-for-this-event
           ::kafka-event-base)))


(comment
  (def example-kafka-payload
    {:kaaf.contracts.core/dashboard-event 2 ;; assume a pre-pass massged this in
     :value
     {"kafka_meta" {"msg_ts" 1580393827936},
      "request_id" "28ce832b-4963-4c42-82b1-2c896cbfdd14",
      "origin" 1,
      "v" 1,
      "id" "c19f6a0a-8298-46cd-ba93-3f2031133ae4",
      "domain" "test",
      "type" 2,
      "action_taken_at" 1580393827819,
      "data"
      {"v" 6,
       "issue"
       {"has_duplicates" false,
        "id" "test_issue_20200130141702793-996fe597afb6eff",
        "tt" "i",
        "pid" 899,
        "added_tags" [],
        "is_custom_meta" false,
        "title" "7ab4271f2acaacc39afe911fa7172bdd04a7d6a6",
        "state" 0,
        "meta" {},
        "created_at" 1580393827819,
        "current_tags" [],
        "removed_tags" [],
        "is_duplicate" false},
       "queue"
       {"current"
        {"id" "test_queue_20191218090443489-0a218760a6e2efc"}},
       "customer"
       {"id" "test_profile_20200130141414794-03c3f3fc0f08cf1"},
       "author"
       {"id" "test_profile_20200130141414794-03c3f3fc0f08cf1",
        "roles" ["user"]},
       "app"
       {"id" "test_app_20191218090459037-d67e51d5c3b41e2",
        "title" "Facebook-EN Only",
        "created_at" 1576659899019},
       "platform"
       {"id" "test_platform_20191218090459400-1ad69de62c5a805",
        "type" "ios",
        "created_at" 1576659899060},
       "message"
       {"id" "test_message_20200130141707823-422d24f98ba60de",
        "type" "txt"}},
      "generated_at" 1580393827935},
     :key "test_issue_20200130141702793-996fe597afb6eff",
     :partition 0,
     :topic "dashboard_events",
     :offset 4818})


  (s/valid? ;; good payload
   ::value (:value example-kafka-payload))


  (s/explain-data
   ;; FAIL: Bad data type
   ::value (:value (assoc-in example-kafka-payload
                             [:value "type"]
                             "2")))

  (s/explain-data
   ;; FAIL: Stale event --- fails p99 SLA requirements
   ::value (:value (assoc-in example-kafka-payload
                             [:value "action_taken_at"]
                             1000000000000)))

  (s/explain-data
   ;; FAIL: Right data type, but illegal value
   ::kafka-event-base (assoc-in example-kafka-payload
                                [:value "type"]
                                12345))

  (s/valid? ;; good payload
   ::event example-kafka-payload)


  (s/explain-data
   ;; FAIL: stale event
   ::event (assoc-in example-kafka-payload
                     [:value "action_taken_at"]
                     1000000000000))
  )
