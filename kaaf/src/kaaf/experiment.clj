(ns kaaf.experiment
  (:require [clojure.spec.alpha :as s]))


;; multi-specs playground

(s/def :event/type keyword?)
(s/def :event/timestamp integer?)
(s/def :search/url string?)
(s/def :error/message string?)
(s/def :error/code integer?)

(defmulti event-type :event/type)

(defmethod event-type :event/search [_]
  (s/keys :req [:event/type :event/timestamp :search/url]))

(defmethod event-type :event/error [_]
  (s/keys :req [:event/type :event/timestamp :error/message :error/code]))

(s/def :event/event (s/multi-spec event-type :event/type))

(s/valid? :event/event
          {:event/type :event/search
           :event/timestamp 14932929323
           :search/url "http://www.google.com"})
