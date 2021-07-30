(ns kaaf.dsl.spec
  "A spec for the DSL itself, so that we may validate it on
  read/write from/to edn.


  PARTS OF THE DSL

  {:action [:op-key :client-key :entity-key] ;; must
   :description \"Some string to explain the purpose. Usable in error messages & logs.\"
   :ctx {:op     {:env :localshiva} ;; and other context specific to the operation
         :client {... map of client config data ...}
         :entity {... map of entity data ...}}
   :contract {'events-contract 'declaration}}


  EXAMPLE DSL USAGE

  ;; A. Full-fledged example

  {:action [:create :sdk :issue] ;; maps to a method in kaaf.dsl.core

   :description \"Create an issue without any meta data.\"

   :ctx {;; Much of the context would be propogated via thrush-like semantics,
         ;; and/or provided automagically via load-settings type functionality.
         ;; We should aim to use :ctx to send \"override\" information, and/or
         ;; any very action-specific information that we need to explicit about.

         :op {:env :localshiva}
         :client {:client-type :ios
                  :app-name \"Facebook-EN Only\"
                  :app-id \"test_site_app_id_123\"
                  :api-key \"q43245245gsdfgg2452345dfgsg-such-secret\"
                  :domain \"test.helpshift.mobi\"
                  :profile-id \"test_site_sdk_profile_123\"}
         :entity {\"message\" \"I need help plz.\"}}

   :contract {;; Here we see all possible constraints on events. We may specify one, or
              ;; several, or none of these. Unspecified contract means the contract is
              ;; \"true\" for it. No constraint means all \"contracts\" are always true.
              ;;
              ;; NOTE: event keys (:event-a, :event-b etc...) should be
              ;; _exactly_ as they appear in the mint repo.

              :exactly-ordered [:event-a :event-b]
              :each-once [:event-r :event-n]
              :each-atleast-once [:event-m :event-o]
              :never [:event-x]}}


  ;; B. Some event contracts specified

  {:action [:create :dashboard :reply]
   :description \"Send a reply via user having admin role.\"
   :ctx {:op {...} :client {...} :entity {...}}
   :contract {:each-once [:event-r]
              :each-atleast-once [:event-m]}}


  ;; C. No events specified, means all \"contracts\" are always true.

  {:action [:read :mongo :issue]
   :description \"I don't know why we'd read from mongo. But it's a possiblity.\"
   :ctx {:op {...} :client {...} :entity {...}}
   :contract {}}


  WAITS

  We may need to insert waits for use between actions, or to explicitly define
  a max-wait to allow previous action(s) to complete, and/or optionally wait for
  automated actions to run, expected to generate events within the wait period.

  The most basic wait action is:

  {:action [:wait :any :any] ;; dumb wait
   :ctx {:op {:env :localshiva
              :milliseconds 3000}
         :client {}
         :entity {}}
   :contract {:each-once [:event-a :event-b]}}


  BUT WAIT! IT'S HARD TO WAIT!

  It turns out various subtle \"wait\" scenarios are possible, illustrated
  below for example. There is no plan right now to support any of these.

  {:action [:read :mongo :timba] ;; assuming timbas were created previously,
   ;; then, for a particular timba, fetch & export
   ;; issue ids, on which timba is expected to apply.
   :ctx {:op {:env :localshiva}
         :client {...mongo-connection-ctx}
         :entity {...mongo-query-filtering-issues-for-timba...}}
   :contract {}}

  ;; vvv
  ;; export :issue-ids into the context
  ;; vvv

  {:action [:wait :any :issues]   ;; \"path-dependent\" wait
   :ctx {:op {:env :localshiva
              :milliseconds 3000}
         :client {}
         ;; TODO: How to validate contracts for collections?
         :entity {:issue-ids [\"id1\" \"id2\" \"id3\"]}}
   :contract {:each-once [:event-a :event-b]}}."
  (:require [clojure.spec.alpha :as s]
            [kaaf.dsl.core :as dsl]
            [mint.validation.mappings :as mvm]))


;;
;; Utilities for creating the DSL
;;

(def known-event?
  (into #{} (keys mvm/types->codes)))

;;
;; Spec for 'action' part of the DSL
;;

(s/def ::action
  (s/cat :op-key dsl/allowed-operation?
         :client-key dsl/valid-client?
         :entity-key (dsl/valid-entity-k?)))

;;
;; Spec for 'description' part of the DSL
;;

(s/def ::description
  string?)

;;
;; Spec for "ctx" part of DSL
;;

(s/def ::env
  dsl/known-env?)

(s/def ::op
  (s/and map?
         (s/keys :req-un [::env])))

(s/def ::client
  map?)

(s/def ::entity
  map?)

(s/def ::ctx
  (s/and map?
         (s/keys :req-un [::op]
                 :opt-un [::client ::entity])))

;;
;; Spec for the 'contract' part of the DSL, which lets test writers
;; declare the event sequence contract.
;;

(s/def ::event-sequence-contract
  (s/coll-of known-event?))

(s/def ::exactly-ordered
  ::event-sequence-contract)

(s/def ::each-once
  ::event-sequence-contract)


(s/def ::each-atleast-once
  ::event-sequence-contract)

(s/def ::never
  ::event-sequence-contract)

(s/def ::contract
  (s/and map?
         (s/keys :opt-un
                 [::exactly-ordered
                  ::each-once
                  ::each-atleast-once
                  ::never])))

;;
;; Composite spec for each test written using the DSL
;;

(s/def ::dsl
  (s/and map?
         (s/keys :req-un [::action
                          ::description]
                 :opt-un [::ctx
                          ::contract])))


(comment
  (s/explain-data ::dsl
                  {:action [:create :sdk :issue]
                   :description "Some useful description."
                   :ctx {:op {:env :localshiva}
                         :client {"profile-id" "foobar" "message" "baz"}
                         :entity {}}
                   ;; all possible constraints on events
                   :contract {:exactly-ordered [:create_issue_by_customer :first_reply_to_issue_by_customer]
                              :each-once []
                              :each-atleast-once []
                              :never [:resolve_issue]}})


  (s/valid? ::dsl
            {:action [:create :sdk :issue]
             :description "Some useful description."
             :ctx {:op {:env :localshiva}
                   :client {"profile-id" "foobar" "message" "baz"}
                   :entity {}}
             ;; all possible constraints on events
             :contract {:exactly-ordered [:create_issue_by_customer :non-existing-event]
                        :each-once []
                        :each-atleast-once []
                        :never [:resolve_issue]}})


  (s/valid? ::dsl
            {:action [:create :sdk :issue],
             :description "User creates an issue via sdk",
             :ctx {:op {:env :localshiva},
                   :client {:client-type :android,
                            :app-name "MyFirstTestApp",
                            :app-id
                            "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2",
                            :api-key "8ea83b7703b51c0f8e42a87f0da40bbc",
                            :domain "nishanttesting.helpshift.mobi"},
                   :entity {:message-text "Message from user",
                            :type "txt",
                            :platform-id
                            "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2",
                            :profile-id
                            "nishanttesting_profile_20200203143823731-749c12230be1bf4",
                            :timestamp "123123333.678",
                            :headers {:x-hs-request-id "8ea83b7703b51c0f8e42a87f0da40bbc",
                                      :content-type "application/x-www-form-urlencoded"}}},
             :contract {:exactly-ordered [],
                        :each-once [:create_issue_by_customer :first_reply_to_issue_by_customer],
                        :each-atleast-once [:create_issue_by_customer],
                        :never [:resolve_issue]}})

  (s/valid? ::contract {:exactly-ordered [:create_issue_by_customer :non-existing-event],
                        :each-once [:create_issue_by_customer :first_reply_to_issue_by_customer],
                        :each-atleast-once [:create_issue_by_customer],
                        :never [:resolve_issue]})

  (s/valid? ::dsl
            {:action [:create :sdk :issue]
             :description "Some useful description."
             :ctx {:op {:env :localshiva}
                   :client {"profile-id" "foobar" "message" "baz"}
                   :entity {}}
             ;; all possible constraints on events
             :contract {}}))
