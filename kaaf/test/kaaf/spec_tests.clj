(ns kaaf.spec-tests
  (:require  [clojure.test :refer [deftest is]]
             [kaaf.dsl.spec :as sut]
             [clojure.spec.alpha :as s]))

;; sample dsl which will be used as a base for each test below
;; assoc/dissoc/update on this dsl to get desired dsl for your tests
(def sample-dsl
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
                  :platform-id "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2"
                  :profile-id "nishanttesting_profile_20200203143823731-749c12230be1bf4"
                  :timestamp "123123333.678"
                  :headers {:x-hs-request-id "8ea83b7703b51c0f8e42a87f0da40bbc"
                            :content-type "application/x-www-form-urlencoded"
                            }}}
   :contract {:exactly-ordered [:create_issue_by_customer :first_reply_to_issue_by_customer]
              :each-once [:create_issue_by_customer :first_reply_to_issue_by_customer]
              :each-atleast-once [:create_issue_by_customer]
              :never [:resolve_issue]}})


(comment
  (assoc-in sample-dsl [:ctx :op :env] :sandbox))


(deftest test-dsl-positive-all-fields-present
  (is (= true (s/valid? :kaaf.dsl.spec/dsl sample-dsl))))


(deftest test-dsl-positive-ctx-contract-permeutations
  (let [dsl-with-only-exactly-ordered (update-in sample-dsl
                                                 [:contract]
                                                 dissoc :each-once :each-atleast-once :never)
        dsl-with-only-each-once (update-in sample-dsl
                                           [:contract]
                                           dissoc :exactly-ordered :each-atleast-once :never)
        dsl-with-only-each-atleast-once (update-in sample-dsl
                                                   [:contract]
                                                   dissoc :each-once :exactly-ordered :never)
        dsl-with-only-never (update-in sample-dsl
                                       [:contract]
                                       dissoc :each-once :each-atleast-once :exactly-ordered)
        dsl-with-empty-contracts (update-in sample-dsl
                                            [:contract]
                                            dissoc :each-once :each-atleast-once :exactly-ordered :never)
        dsl-without-contracts (update-in sample-dsl
                                         [:ctx]
                                         dissoc :contract)]
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-only-exactly-ordered))
        "only :exactly-ordered key is present")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-only-each-atleast-once))
        "only :each-atleast-once key is present")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-only-each-once))
        "only :each-once key is present")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-only-never))
        "only :never key is present")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-empty-contracts))
        "contracts is an empty map")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-without-contracts))
        "contracts key is not present")))


(deftest test-dsl-negative-ctx-contract-permeutations
  (let [dsl-with-non-existing-event-in-exactly-ordered (assoc-in sample-dsl
                                                                 [:contract :exactly-ordered]
                                                                 [:create_issue_by_customer :non-existing-event])
        dsl-with-non-existing-event-in-each-once (assoc-in sample-dsl
                                                           [:contract :each-once]
                                                           [:create_issue_by_customer :non-existing-event])
        dsl-with-non-existing-event-in-each-atleast-once (assoc-in sample-dsl
                                                                   [:contract :each-atleast-once]
                                                                   [:non-existing-event])
        dsl-with-non-existing-event-in-never (assoc-in sample-dsl
                                                       [:contract :never]
                                                       [:non-existing-event :resolve_issue])
        dsl-with-empty-exactly-ordered (assoc-in sample-dsl
                                                 [:contract :exactly-ordered] [])
        dsl-with-empty-each-once (assoc-in sample-dsl
                                           [:contract :each-once] [])
        dsl-with-empty-each-atleast-once (assoc-in sample-dsl
                                                   [:contract :each-atleast-once] [])
        dsl-with-empty-never (assoc-in sample-dsl
                                       [:contract :never] [])]
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-non-existing-event-in-exactly-ordered))
        "non-existing event in :exactly-ordered key")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-non-existing-event-in-each-once))
        "non-existing event in :each-once")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-non-existing-event-in-each-atleast-once))
        "non-existing event in :each-atleast-once")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-non-existing-event-in-never))
        "non-existing event in :never")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-empty-exactly-ordered))
        "empty events vector in :exactly-ordered")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-empty-each-once))
        "empty events vector in :each-once")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-empty-each-atleast-once))
        "empty events vector in :each-atleast-once")
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-empty-never))
        "empty events vector in :never")))


(deftest test-dsl-positive-ctx-not-present
  (let [dsl-without-ctx
        (dissoc sample-dsl :ctx)]
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-without-ctx))
        "ctx key is not present")))


(deftest test-dsl-positive-ctx-permeutations
  (let [dsl-ctx-with-only-op
        (update-in sample-dsl [:ctx] dissoc :client :entity)]
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-ctx-with-only-op))
        "ctx->entity and ctx->client not present")))


(deftest test-dsl-negative-action-permeutations
  (let [dsl-action-without-entity (assoc sample-dsl
                                         :action [:create :sdk])
        dsl-action-without-operation (assoc sample-dsl
                                            :action [:sdk :issue])
        dsl-action-without-client (assoc sample-dsl
                                         :action [:create :issue])
        dsl-action-non-existing-operation (assoc sample-dsl
                                                 :action [:make :sdk :issue])
        dsl-action-non-existing-client (assoc sample-dsl
                                              :action [:create :mobile :issue])
        dsl-action-non-existing-entity (assoc sample-dsl
                                              :action [:create :sdk :ticket])
        dsl-without-action (dissoc sample-dsl :action)
        dsl-action-with-string-values (assoc sample-dsl
                                             :action ["create" "sdk" "issue"])]

    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-without-entity))
        "entity is missing in the action part of the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-without-operation))
        "operation is missing in the action part of the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-without-client))
        "client is missing in the action part of the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-non-existing-operation))
        "non-existing operation is present in the action part of the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-non-existing-client))
        "non-existing client is present in the action part of the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-non-existing-entity))
        "non-existing entity is present in the action part of the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-without-action))
        "action key not present in the dsl")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-with-string-values))
        "action with string values instead of keywords")))


(deftest test-dsl-negative-description-permeutations
  (let [dsl-without-description (dissoc sample-dsl
                                        :description)
        dsl-description-not-string (assoc sample-dsl
                                          :description 45678)]
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-without-description))
        "description key is not present")
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-description-not-string))
        "description is not string")))


(deftest test-dsl-positive-sandbox-as-environment
  (let [dsl-with-sandbox-environment (assoc-in sample-dsl
                                               [:ctx :op :env] :sandbox)]
    (is (= true
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-sandbox-environment))
        "environment is sandbox")))


(deftest test-dsl-negative-non-existing-environment
  (let [dsl-with-sandbox-environment (assoc-in sample-dsl
                                               [:ctx :op :env] :non-existing-env)]
    (is (= false
           (s/valid? :kaaf.dsl.spec/dsl
                     dsl-with-sandbox-environment))
        "non-existing environment in ctx->op->env")))
