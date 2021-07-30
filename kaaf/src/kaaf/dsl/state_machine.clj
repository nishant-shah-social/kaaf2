(ns kaaf.dsl.state-machine
  "Experimental sketch...
  Turn our DSL-as-data to arbitrary actions as data, via the lens
  of a State Machine, which could also be described as data."
  (:require [kaaf.data.mocks :as mock]
            [kaaf.dsl.core :as dsl]))

(def all-possible-action-permutations
  (for [a dsl/allowed-operation?
        p dsl/valid-client?
        x dsl/valid-entity-k?]
    [a p x]))


(def action-state-machine
  "Basic mapping of action pairs that are illogical by construction.
  e.g. after we :delete a domain entity that, we cannot do anything with it.
  Separately, define illegal actions for specific client<->entity pairs, e.g.
  {[:sdk :issue] {:delete (constantly false), :create #{:update, :fetch}, :fetch #{:update, :fetch}}
   [:hs-api :issue] {:delete #{}, :create : }"
  {:delete #{}
   :create #{:delete :update :fetch :read}
   :fetch #{:delete :update :fetch :read}
   :read #{:delete :update :fetch :read}
   :update #{:delete :update :fetch :read}})


(defn rand-sample-actions
  ([]
   (rand-sample-actions 0.1))
  ([probability]
   (random-sample probability
                  all-possible-action-permutations))
  ([probability list-of-dsl-actions ]
   (random-sample probability
                  list-of-dsl-actions)))


(defn legal-action-sequence?
  "An action sequence is [action-k actor-k domain-entity-k]"
  [actions]
  (let [legal-next-step? (fn [[fst-k nxt-k]]
                           ((fst-k action-state-machine)
                            nxt-k))]
    (->> actions
         (partition-all 2 1)
         butlast
         (every? legal-next-step?))))


(comment
  (legal-action-sequence?
   (map first [[:update :hs-api :customer-survey]
               [:fetch :hs-api :customer-survey]
               #_[:delete :hs-api :customer-survey]
               [:read :hs-api :customer-survey]
               [:update :hs-api :customer-survey]]))


  (let [client-entity-tuple (juxt second last)
        massage-plat-entity-records (fn [[p-e-tuple v]]
                                      [p-e-tuple (map first v)])
        issue-related? (comp #{:issue :my-issues}
                             last)
        sdk-action? (comp #{:sdk} second)]
    (->> all-possible-action-permutations
         (filter issue-related?)
         (filter sdk-action?)
         (rand-sample-actions 0.5)
         (group-by client-entity-tuple)
         (map massage-plat-entity-records)
         (filter (fn [[_ v]]
                   (legal-action-sequence? v)))
         (assoc {:runner dsl/action
                 :start-ctx
                 (mock/mock-payload-ctx
                  (mock/mock-client-config mock/test-app-id)
                  (mock/mock-new-user-params mock/mock-unique-user-id)
                  (mock/mock-client-params (:app-id mock/mock-client-config))
                  (mock/mock-time-params))}
                :actions))))
