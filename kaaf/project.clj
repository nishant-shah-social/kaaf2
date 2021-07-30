(defproject kaaf "0.1.0-testing-SNAPSHOT"
  :description "A long time ago, in a Kafka cluster far far away,
We tried to publish our messages in our futures.
But sometimes the messages went kafaK,
And sometimes the messages went Kaka.
All that was left, was Kaaf.

Kaaf, it turned out, has many interpretations and some expansions.
One dubious source says it symbolises "wrong, or amiss" in Urdu.

So not only did our messages go kaaf,
We didn't know anymore what they meant.
And our consumers and our databases would kaaf too.
system-wide kaaf-ing fits...

`kaaf`, the project, hopes to help us verify that what we send to Kafka
is indeed what we can consume, without coughing up exceptions."
  :url "https://gerrit.helpshift.com/#/admin/projects/kaaf"
  :license {:name "Proprietary"
            :url "https://www.helpshift.com/"}
  :plugins [[s3-wagon-private "1.1.2"]]
  :repositories [["private" {:url "s3p://maven.helpshift.com/releases/"
                             ;; assumes LEIN_USERNAME and LEIN_PASSPHRASE
                             ;; environment variables
                             :username :env
                             :passphrase :env}]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-future-spec "1.9.0" ;; To specify properties of events
                  :exclusions [org.clojure/clojure]]
                 [clj-http "3.10.0" ;; To emulate client actions via HTTP
                  :exclusions [org.clojure/clojure]]
                 [clj-time "0.15.2"]
                 [org.clojure/tools.cli "1.0.194"]
                 [helpshift/helpshift-kafka "0.3.0" ;; To consume events
                  :exclusions [org.clojure/clojure
                               com.stuartsierra/component
                               clj-time
                               log4j/log4j
                               org.slf4j/slf4j-api
                               org.slf4j/slf4j-log4j12
                               primitive-math
                               tardigrade
                               helpshift/helpshift-logging
                               helpshift/settings]]
                 [sthiraank "0.10.0" ;; For constants shared across our stack
                  :exclusions [org.clojure/clojure
                               cheshire]]
                 [helpshift/mint "1.50.0" ;; For event type mappings
                  :exclusions [org.clojure/clojure
                               prismatic/schema
                               clj-time
                               helpshift/helpshift-logging]]
                 [helpshift/settings "0.2.1"]
                 [helpshift/helpshift-logging "0.8.0"]
                 [helpshift/vulcan "4.2.0"
                  :exclusions [org.clojure/clojure
                               clj-kafka
                               clj-http
                               helpshift/settings]]]
  :main ^:skip-aot kaaf.dsl.runners
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
