(ns kaaf.consumers.core
  (:require [clojure.data.json :as cdj]
            [helpshift-kafka.consumer :as hkc]
            [kaaf.dsl.core :as kdc])
  (:import [org.apache.kafka.clients.consumer KafkaConsumer]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consumer Definitions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce live-consumers
  ^{:doc "Associate topic name with the consumer object (a promise).
Ensure we def only once, in a clean REPL."}
  (atom {:dashboard-events
         {:topic "dashboard_events"
          :kafka-config {"bootstrap.servers" ((kdc/properties-file) "test.kafka.server")
                         "auto.offset.reset" "latest"
                         "enable.auto.commit" "false"
                         "group.id" "dashboard-event-consumers"
                         "client.id" "kaaf-dashboard-events-consumer-01"
                         "key.deserializer" :string
                         "value.deserializer" :string}
          :monitoring-config {:team-key "test"
                              :prefix "test.kafka.consumer"}
          :kafka-msg-handler (constantly false) ;; update at the time of initialising
          :consumer-obj nil}}))


(defn- get-ctx-for
  "Given a consumer key, and a mapping of consumer configurations, return
  context only if it is well-formed. Default lookup the live-consumers atom."
  ([consumer-k]
   (get-ctx-for consumer-k
                @live-consumers))
  ([consumer-k consumer-ctx-mappings]
   {:pre [(every? (partial contains? (get consumer-ctx-mappings
                                          consumer-k))
                  [:topic :kafka-config :monitoring-config
                   :kafka-msg-handler :consumer-obj])
          (fn? (:kafka-msg-handler (get consumer-ctx-mappings
                                        consumer-k)))]}
   (get consumer-ctx-mappings
        consumer-k)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consumer Message Handlers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-message-copier
  "Given an atom where messages should be read off the topic and stored,
  create a message handler needed to initialise the consumer."
  [message-collector-atom]
  (fn [{mval :value :as msg}]
    (let [mval (cdj/read-str mval)]
      (swap! message-collector-atom
             conj
             (assoc msg :value mval)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consumer Session Management Utils
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-consumer!
  [consumer-k kafka-msg-handler]
  (let [{:keys [topic
                kafka-config
                monitoring-config]
         :as consumer-ctx} (get-ctx-for consumer-k)
        consumer-obj (hkc/init-consumer kafka-config
                                         topic
                                         monitoring-config
                                         kafka-msg-handler)
        live-consumer-ctx (assoc consumer-ctx
                                 :consumer-obj consumer-obj
                                 :kafka-msg-handler kafka-msg-handler)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (println "Running shutdown hook. ========")
                                 (hkc/stop-consumer consumer-obj)
                                 (println "Shutdown hook complete. ========"))))
    (swap! live-consumers
           assoc
           consumer-k
           live-consumer-ctx)))


(defn stop-consumer!
  "Given a consumer key, look it up in the atom containing context of
  live consumers, and stop it."
  [consumer-k]
  (let [consumer-obj (:consumer-obj
                       (get-ctx-for consumer-k))]
    (when consumer-obj
      (hkc/stop-consumer
       consumer-obj))))


(defn restart-consumer!
  "Support clean restarts for two scenarios:
  1. When we want to re-do a consumer pass with existing message handlers.
  2. When we want to re-consume, with a _different_ message handler, e.g.
  when refactoring an handler, or experimenting with different handlers."
  ([consumer-k]
   (restart-consumer! consumer-k
                      (:kafka-msg-handler
                       (get-ctx-for consumer-k))))
  ([consumer-k kafka-msg-handler]
   (let [{:keys [consumer-obj]
          :as consumer-ctx} (get-ctx-for consumer-k)]
     (println "\n" (. ^java.time.LocalDateTime (java.time.LocalDateTime/now) toString)
              "==================================================\n")
     (when (and (map? consumer-obj)
                (instance? KafkaConsumer (:kafka-consumer consumer-obj)))
       (do (println "Stopping topic consumer associated with topic key " consumer-k)
           (stop-consumer! consumer-k)))
     (println "Starting topic consumer for topic key " consumer-k)
     (init-consumer! consumer-k
                     kafka-msg-handler)
     (println "Topic consumer context is ")
     (clojure.pprint/pprint (get-ctx-for consumer-k)))))
