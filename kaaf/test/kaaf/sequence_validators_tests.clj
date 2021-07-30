(ns kaaf.sequence-validators-tests
  (:require  [clojure.test :refer [deftest is]]
             [kaaf.contracts.sequence-validators :as kcsv]
             [mint.validation.mappings :as mvm]))


(defn sample-dsl-declaration-for-a-business-action
  [exactly-ordered-events]
  {:action [:create :sdk :issue]
   :ctx {}
   :contract {:exactly-ordered exactly-ordered-events}})


(deftest positive-non-empty-exactly-ordered-test
  (let [contract {:exactly-ordered [:eventa :eventb :eventc]}
        events [:eventa :eventb :eventc]]
    (is (kcsv/exactly-ordered? contract
                               events)
        "Returning false even if :exactly-ordered events match the actual events")))


(deftest positive-empty-contract-and-events-exactly-ordered-test
  (let [contract {}
        events []]
    (is (kcsv/exactly-ordered? contract
                               events)
        "Returning false even if contract is empty and actual events is empty")))


(deftest positive-empty-exactly-ordered-and-events-exactly-ordered-test
  (let [contract {:exactly-ordered []}
        events []]
    (is (kcsv/exactly-ordered? contract
                               events)
        "Returning false even if contract->:exactly-ordred is empty and actual events is empty")))


(deftest positive-nil-contract-and-empty-events-exactly-ordered-test
  (let [contract nil
        events []]
    (is (kcsv/exactly-ordered? contract
                               events)
        "Returning false even if contract->:exactly-ordred is empty and actual events is empty")))


(deftest negative-mistmatch-contract-events-exactly-ordered-test
  (let [contract {:exactly-ordered [:eventa :eventb :eventc]}
        events [:eventb :eventa :eventc]]
    (is (not (kcsv/exactly-ordered? contract
                                    events))
        "Returning true even if :exactly-ordered events do not match the actual events")))


(deftest negative-empty-contract-non-empty-events-exactly-ordered-test
  (let [contract {}
        events [:eventb :eventa :eventc]]
    (is (not (kcsv/exactly-ordered? contract
                                    events))
        "Returning true even if contract is empty and actual events is non-empty")))


(deftest negative-empty-exactly-ordered-non-empty-events-exactly-ordered-test
  (let [contract {:exactly-ordered []}
        events [:eventb :eventa :eventc]]
    (is (not (kcsv/exactly-ordered? contract
                                    events))
        "Returning true even if contract->:exactly-ordered is empty and actual events is non-empty")))


(deftest negative-duplicate-and-mismatch-events-exactly-ordered-test
  (let [contract {:exactly-ordered [:eventa :eventb :eventc]}
        events [:eventa :eventb :eventc :eventc]]
    (is (not (kcsv/exactly-ordered? contract
                                    events))
        "Returning true even if contract->:exactly-ordered is empty and actual events is non-empty")))


(deftest positive-validate-kafka-events-for-actions-with-results-verbose-test
  (let [actions-events-ctx '({:action [:create :sdk :issue],
                              :headers
                              {"header1" "value1",
                               "header2" "value2"},
                              :response "response",
                              :contract
                              {:exactly-ordered
                               [:eventa :eventb]},
                              :events
                              [:eventa :eventb]},
                             {:action [:wait :any :any],
                              :headers
                              {"header1" "value1"}
                              :response "response"
                              :events []}
                             {:action [:create :sdk :message],
                              :headers
                              {"header1" "value1"},
                              :response "response",
                              :contract {:exactly-ordered [:eventc :eventd :evente]},
                              :events [:eventc :eventd :evente]})
        action-events-result (apply kcsv/validate-kafka-events-for-actions-with-results true
                                    actions-events-ctx)]
    (is (every? :valid-events? action-events-result)
        "Some action did not have similar contract->:exactly-ordered and events fields")
    (is (every? (fn [action]
                  (and (:response action)
                       (:headers action)))
                action-events-result)
        "Some action did not contain :headers or :response field even if the verbose flag is true")))


(deftest positive-validate-kafka-events-for-actions-with-results-non-verbose-test
  (let [actions-events-ctx '({:action [:create :sdk :issue],
                              :headers
                              {"header1" "value1",
                               "header2" "value2"},
                              :response "response",
                              :contract
                              {:exactly-ordered
                               [:eventa :eventb]},
                              :events
                              [:eventa :eventb]},
                             {:action [:wait :any :any],
                              :headers
                              {"header1" "value1"}
                              :response "response"
                              :events []}
                             {:action [:create :sdk :message],
                              :headers
                              {"header1" "value1"},
                              :response "response",
                              :contract {:exactly-ordered [:eventc :eventd :evente]},
                              :events [:eventc :eventd :evente]})
        action-events-result (apply kcsv/validate-kafka-events-for-actions-with-results false
                                    actions-events-ctx)]
    (is (every? :valid-events? action-events-result)
        "Some action did not have similar contract->:exactly-ordered and events fields")
    (is (every? (fn [action]
                  (and (not (:response action))
                       (not (:headers action))))
                action-events-result)
        "Some action contained :headers or :response field even if the verbose flag is false and these actions passed")))


(deftest negative-validate-kafka-events-for-actions-with-results-non-verbose-some-action-fail-test
  (let [actions-events-ctx '({:action [:create :sdk :issue],
                              :headers
                              {"header1" "value1",
                               "header2" "value2"},
                              :response "response",
                              :contract
                              {:exactly-ordered
                               [:eventa :eventb]},
                              :events
                              [:eventa :eventb :eventc]},
                             {:action [:wait :any :any],
                              :headers
                              {"header1" "value1"}
                              :response "response"
                              :events []}
                             {:action [:create :sdk :message],
                              :headers
                              {"header1" "value1"},
                              :response "response",
                              :contract {:exactly-ordered [:eventc :eventd :evente]},
                              :events [:eventc :eventd :evente]})
        action-events-result (apply kcsv/validate-kafka-events-for-actions-with-results false
                                    actions-events-ctx)
        action-with-verbose (first action-events-result)
        actions-without-verbose (rest action-events-result)]
    (is (not (action-with-verbose :valid-events?))
        "action :- [:create :sdk :issue] not failing even though the contract and events are mismatching")
    (is (and (action-with-verbose :headers)
             (action-with-verbose :response))
        "action :- [:create :sdk :issue] not having headers and response fields even though its failing")
    (is (every? (fn [action]
                  (and (not (:response action))
                       (not (:headers action))))
                actions-without-verbose)
        "Some action contained :headers or :response field even if the verbose flag is false and these actions passed")))
