(ns kaaf.data.mocks
  "Utils to craft payloads that mimic client payloads
  (such as sdk, dashboard, hs-api etc)."
  (:require [clj-time.coerce :as timecoerce]
            [clj-time.core :as time])
  (:import java.util.UUID))


(defn mock-tag-name [tagname]
  (str tagname (UUID/randomUUID)))


(defn gen-hsft-timestamp
  "From time utils, taken from shuriken.util"
  []
  (let [t (timecoerce/to-long
           (time/now))]
    (format "%s.%03d"
            (quot t 1000)
            (rem t 1000))))


(defn gen-uuid-str
  "Might be useful as a utility function."
  []
  (str (UUID/randomUUID)))


(defn make-params
  [params]
  (into (sorted-map) params))


(defn mock-client-params
  [app-or-client-id]
  (make-params
   {"platform-id" app-or-client-id}))


(defn mock-time-params
  []
  (make-params
   {"timestamp" (gen-hsft-timestamp)}))


(defonce mock-unique-user-id
  (gen-uuid-str))


(defn mock-new-user-params
  [uuid-str]
  (make-params
   {:displayname (str "IAmGroot " uuid-str)
    :email (str "groot" "+" uuid-str "@gmail.com")
    :identifier uuid-str}))


(defn mock-user-profile-params
  [profile-id]
  (make-params
   {"profile-id" profile-id}))


(defn init-asignee-id
  [profile-id]
  (make-params
   {"assignee-id" profile-id}))


(defn mock-message-params
  []
  (make-params
   {"type" "txt"
    "message-text" (str "Are you kaafing, " (gen-uuid-str) ", my friend?")}))


(defn mock-text-message-params
  []
  {"type" "txt"
   "message-text" (str "This is a test message body " (gen-uuid-str))})


(defn append-headers
  [payload-ctx header-key header-value]
  (if (contains? payload-ctx :headers)
    (update payload-ctx :headers conj {header-key header-value})
    (assoc payload-ctx :headers {header-key header-value})))


(defn gen-headers-sdk []
  {"x-hs-request-id" (gen-uuid-str)
   "content-type" "application/x-www-form-urlencode"})


(defn gen-headers-xhr []
  {"user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0"
   "x-hs-request-id" (gen-uuid-str)
   "content-type" "application/x-www-form-urlencoded"
   "X-Requested-With" "XMLHttpRequest"})


(defn mock-payload-ctx
  "Pass in a uuid in the x-hs-request-id header field, for end-to-end tracing
  of action, to events.

  moby.ring.middleware/wrap-request-id fetches request-id from the header,
  and passes it along to event payloads in the 'request-id' field, which is
  mandatory for events.

  Ref. Slack discussion:
  https://helpshift.slack.com/archives/G02VD11H7/p1578899768020900?thread_ts=1578899033.018900"
  [client-config-map & http-request-param-maps]
  (assoc client-config-map
         :request-params (make-params
                          (apply merge http-request-param-maps))
         :headers {"x-hs-request-id" (gen-uuid-str)}))
