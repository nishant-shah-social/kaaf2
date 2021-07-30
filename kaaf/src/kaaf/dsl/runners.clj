(ns kaaf.dsl.runners
  "Facilitates running of test scenarios specified in Scenarios.edn.
  Also performs some pre and post requisites before and after execution
  of tests/actions."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as cpp]
            [clojure.string :as cs]
            [kaaf.dsl.core :as kdc]
            [clojure.tools.cli :refer [parse-opts]]
            [kaaf.dsl.aggregator :as kda]
            [kaaf.contracts.sequence-validators :as kcs]
            [kaaf.consumers.core :as consumer]
            [vulcan.seq-util :as vsu]
            clj-http.core))


(defonce events-collection-atom
  ^{:doc "Stores kafka events triggered by the actions performed."}
  (atom []))


(defn start-consumer
  "Starts kafka consumer for dashboard-events topic."
  []
  (consumer/restart-consumer! :dashboard-events
                              (consumer/make-message-copier events-collection-atom)))


(defn stop-consumer
  "Stops kafka consumer for dashboard-events topic."
  []
  (consumer/stop-consumer! :dashboard-events))


(defn fetch-actual-param-val
  "Fetches the actual value of the request parameter to be
  substituted."
  [scenario-actions place-holder]
  (println place-holder)
  (let [split-place-holder (clojure.string/split place-holder
                                                 #"\.")
        action-id (first split-place-holder)
        json-path (mapv keyword
                        (rest split-place-holder))
        prev-action (vsu/ffilter #(= (Integer/parseInt action-id)
                                     (int (:id %)))
                                 scenario-actions)
        prev-action-resp (:response prev-action)
        prev-action-response-body (:body prev-action-resp)]
    (get-in prev-action-response-body
            json-path)))


(defn keys-in
  "Returns a sequence of all key paths in a given map using DFS walk."
  [m]
  (letfn [(children
            [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch?
            [node] (-> (children node)
                       seq
                       boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch?
                            children %)))))


(defn substitute-params-in-action
  "In the scenarios.edn file if the user wants to leverage
  some of the response fields of any previous actions, then he/she
  would specify something like :lookup-response 1.id.
  Presence of :val-from-above-actions key indicate that the value of
  this parameter needs to be fetched from the response of one of the
  previous actions. 1.id indicates that the value needs to be fetched
  from the response field named id of the first action.
  Substitution of such placeholder keys in the action takes here."
  [scenario-actions entity-params]
  (let [all-key-paths (keys-in entity-params)
        path-requiring-sub (filter #(:lookup-response (set %))
                                   all-key-paths)
        sub-params-with-new-vals (reduce (fn [sub key-path]
                                           (let [new-value (fetch-actual-param-val (:action-response scenario-actions)
                                                                                   (get-in entity-params
                                                                                           key-path))]
                                             (assoc sub
                                                    (vec (drop-last key-path))
                                                    new-value)))
                                         {}
                                         path-requiring-sub)
        new-entity-params (reduce-kv (fn [n-entity-params sub-key sub-value]
                                       (assoc-in n-entity-params
                                                 sub-key
                                                 sub-value))
                                     entity-params
                                     sub-params-with-new-vals)]
    (if (empty? path-requiring-sub)
      entity-params
      new-entity-params) ))


(defn run-actions
  "parse and executes all the actions present in a dsl scenario."
  [{:keys [scenario actions]}]
  (let [scenario-outcome (reduce (fn [scenario-details action]
                                   ;; If :ctx -> :entity is present in the action, then check
                                   ;; and perform any param substitutions if needed
                                   (let [action-with-sub (if-let [ctx-entity (get-in action
                                                                                     [:ctx :entity])]
                                                           (let [entity-params-with-sub (substitute-params-in-action scenario-details
                                                                                                                     ctx-entity)]
                                                             (assoc action
                                                                    :entity
                                                                    entity-params-with-sub))
                                                           action)
                                         resp (kdc/action action-with-sub
                                                          (:data-collector scenario-details))
                                         action-response (merge action
                                                                (dissoc resp
                                                                        :data))
                                         scenario-details (-> scenario-details
                                                              (update :action-response
                                                                      conj action-response)
                                                              (update :data-collector
                                                                      merge (:data resp)))]
                                     ;; Breather for events to generate
                                     (Thread/sleep 2000)
                                     ;; Break execution if action response
                                     ;; returns failure/false
                                     (if (:success? resp)
                                       scenario-details
                                       (reduced (assoc scenario-details
                                                       :scenario-passed? false)))))
                                 {:data-collector {}
                                  :action-response []
                                  :scenario-passed? true}
                                 actions)]
    {:scenario scenario
     :actions (:action-response scenario-outcome)
     :passed? (:scenario-passed? scenario-outcome)}))


(defn read-dsl
  "Reads the full dsl from the .edn file passed in as a parameter."
  [file-name scenarios-to-run]
  (let [full-dsl (edn/read-string (slurp (io/resource file-name)))
        scenarios-to-run (set scenarios-to-run)]
    (filterv (fn [scenario]
               (or (empty? scenarios-to-run)
                   (scenarios-to-run (scenario :scenario))))
             full-dsl)))


(defn parse-and-run-dsl-scenarios
  "Parses each scenario of dsl and executes actions in each scenario."
  [dsl]
  (mapv run-actions dsl))


(defn aggregate-actions-events
  "Aggregates the kafka events generated by the actions.
  The events are aggregated into the respective action map."
  [{:keys [scenario actions passed?]}]
  (let [action-events (mapv (partial kda/action+events
                                     @events-collection-atom)
                            actions)]
    {:scenario scenario
     :actions action-events
     :passed? passed?}))


(defn aggregate-actions-events-summarize
  "Aggregates the kafka events generated by the actions.
  The events are aggregated into the respective action map.
  Summarize - Only specific fields of the event payload are filtered out."
  [{:keys [scenario actions passed?]}]
  (let [action-event-summary (mapv (fn [action]
                                     (let [action-events (kda/action+events->summarize @events-collection-atom
                                                                                       action)]
                                       (select-keys action-events
                                                    [:action :headers :response :contract :events])))
                                   actions)]
    {:scenario scenario
     :actions action-event-summary
     :passed? passed?}))


(defn validate-events-for-actions
  "Validates the events and the contracts of each action in the
  Scenarios.edn file. Only sequence of the events is validated."
  [verbose-result? {:keys [scenario actions]}]
  (let [actions-events-validations (apply (partial kcs/validate-kafka-events-for-actions-with-results
                                                   verbose-result?)
                                          actions)
        scenario-passed? (every? :valid-events?
                                 actions-events-validations)]
    {:scenario scenario
     :actions actions-events-validations
     :passed? scenario-passed?}))


(defn execute-tests
  "Executes the test scenarios which are intersection between
  the ones specified in Scenarios.edn file and scenarios passed in
  as parameters.
  Returns Detailed outcome, i.e. detailed action response and event payload
  is punched into the outcome"
  [edn-file & scenarios]
  (let [dsl (read-dsl edn-file scenarios)
        ;; Flush out all the existing events
        _ (reset! events-collection-atom [])
        ;; Punch-in the response & headers with the respective actions
        dsl-with-response (parse-and-run-dsl-scenarios dsl)
        ;; Punch-in the events generated by respective actions
        ;; for the whole scenario
        actions-with-events (mapv aggregate-actions-events
                                  dsl-with-response)]
    (mapv validate-events-for-actions
          actions-with-events)))


(defn exit
  "Exits the program"
  [status msg]
  (println msg)
  (System/exit status))


(defn execute-tests-summarize
  "Executes the test scenarios which are intersection between
  the ones specified in Scenarios.edn file and scenarios passed in
  as parameters.
  Returns only the summary of the outcome, i.e. Summarized action
  response and event payload is NOT punched into the outcome."
  [verbose-result? edn-file scenarios]
  (try (let [dsl (read-dsl edn-file scenarios)
             ;; Flush out all the existing events
             _ (reset! events-collection-atom [])
             _ (start-consumer)
             ;; Punch-in the response & headers with the respective actions
             dsl-with-response (parse-and-run-dsl-scenarios dsl)
             ;; Punch-in the events generated by respective actions
             actions-with-events (mapv aggregate-actions-events-summarize
                                       dsl-with-response)]
         (mapv (partial validate-events-for-actions verbose-result?)
               actions-with-events))
       (catch Exception ex
         (exit 1
               (str "Error occurred while execution :- "
                    (.getMessage ex))))
       (finally (stop-consumer))))


(defn is-boolean
  "Verifies whether given input is boolean or not"
  [x]
  (or (true? x) (false? x)))


(def cli-options
  "Command line options needed for executing kaaf tests and their definition"
  [["-v" "--verbose? false" "Whether output needs to be verbose or not"
    :default false
    :parse-fn #(= % "true")
    :validate-fn #(is-boolean %)]
   ["-f" "--file scenarios.edn" "Name of the scenarios.edn file"
    :default "scenarios.edn"
    :validate-fn #(cs/includes? % ".edn")]
   ["-s" "--scenarios scenario1, scenario2" "Name of the scenarios to be executed"
    :default []
    :parse-fn #(cs/split % #",")]])


(defn perform-execution
  "Performs the kaaf test execution"
  [verbose-result? scenario-file scenarios]
  (let [output (execute-tests-summarize verbose-result?
                                        scenario-file
                                        scenarios)]
    (println "********************** Here's Your Report ***********************")
    (cpp/pprint output)
    (println "********************** Here's Your Report ***********************")))


(defn -main
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args
                                                               cli-options)]
    (if errors
      (exit 1 (str "Error occurred while parsing the command line params:- " errors))
      (perform-execution (:verbose? options)
                         (:file options)
                         (:scenarios options)))))


(comment
  (start-consumer)
  (stop-consumer)

  (reset! events-collection-atom '[])

  ;; Execute all/specified scenarios from
  ;; scenarios.edn file
  (execute-tests-summarize false "scenarios.edn"
                           ["Basic issue life cycle"
                            "Agent inserts tag to an issue"])
  (execute-tests-summarize false "scenarios_without_wait.edn"
                           ["Basic issue life cycle"]))
