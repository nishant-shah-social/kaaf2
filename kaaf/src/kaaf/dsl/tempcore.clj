
(defn dsl-implemented?
  [this-method action-permutations]
  (let [met-map (methods this-method)]
    (reduce (fn [acc-map action]
              (let [k (if (contains? met-map action)
                        [:implemented]
                        [:not-implemented])]
                (update-in acc-map k conj action)))
            {:implemented []
             :not-implemented []}
            action-permutations)))


(comment
  (contains? (methods kaaf.core.dsl/action) [:create :sdk :issue]))
;;
;; API and utils providing a "low-level" DSL for action utility calls.
;; Higher-level DSL API to be created later based on necessity and/or
;; new design insights.
;;


(defn- strip-clean-uri-fragment
  "Strip clean leading and trailing spaces, and slashes from a URI fragment,
  so we can later compose it reliably into a well-formed URI,
  with slashes at the appropriate places."
  [uri-frag]
  (-> uri-frag
      string/trim ;; trim before deslashing, _and_ after as well
      (string/replace #"^/|/$" "")
      string/trim))


(defn make-url-ctx
  "Produce a map of well-formed URL parts, and the full url that conform's
  to moby's expectations, as seen in
  - moby/api.lib.routes and
  - moby/api.lib.ring.middleware."
  ([env-k client-k entity-k]
   (make-url-ctx
    entity-info env-k client-k entity-k []))
  ([env-k client-k entity-k args]
   (make-url-ctx
    entity-info env-k client-k entity-k args))
  ([entity-map env-k client-k entity-k args]
   {:pre [(every? (partial contains? entity-map)
                  [:env :client :entity])
          (known-env? env-k)
          (valid-entity-k? entity-k)
          (and (coll? args)
               (= (count args)
                  ;; all formatters are %-prefixed
                  ;; ref: https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
                  (count (re-seq #"%"
                                 (get-in entity-map [:entity entity-k])))))]}
   (let [get-clean-fragment (comp
                             strip-clean-uri-fragment
                             (partial get-in entity-map))
         base-url    (get-clean-fragment [:env env-k :url])
         plat-path   (get-clean-fragment [:client client-k])
         entity-route-pattern (get-clean-fragment [:entity entity-k])
         entity-route   (format "/%s/%s/" ;; must have leading and trailing slashes
                                plat-path
                                (apply format entity-route-pattern args))]
     {:base-url base-url
      :entity-route entity-route
      :url (str base-url entity-route)})))


(comment
  (make-url-ctx :localshiva :sdk :profiles)
  )


(defmulti action
  (fn [op-k client-k entity-k
       {:keys [request-params client-type
               app-id api-key domain]
        :or {request-params (sorted-map)}
        :as payload-ctx}]
    {:pre [(or (not= client-k :sdk) (sorted? request-params))
           (boolean (allowed-operation? op-k))]}
    [op-k client-k entity-k]))


(defmethod action :default ;; :sdk, :hs-api, :dashboard
  [op-k client-k entity-k payload-ctx]
  (throw (Exception. (format "Invalid Action: %s, Client: %s, or Entity:%s, or Payload context: %s"
                             op-k client-k entity-k payload-ctx))))


(defmethod action [:create :sdk :profiles]
  [op-k client-k entity-k
   {:keys [env client-type app-id api-key
           request-params headers]
    :or {env :localshiva}
    :as payload-ctx}]
  (let [{:keys [url entity-route]} (make-url-ctx env client-k entity-k)
        signature (crypto/gen-signature "POST"
                                        entity-route
                                        request-params
                                        api-key)
        signed-params (assoc request-params
                             "signature" signature)]
    (http/post url
               {:form-params signed-params
                :headers (conj {"content-type" "application/x-www-form-urlencoded"}
                               headers)})))


(defmethod action [:create :sdk :issue]
  [op-k client-k entity-k
   {:keys [env client-type app-id api-key
           request-params headers]
    :or {env :localshiva}
    :as payload-ctx}]
  (let [{:keys [url entity-route]} (make-url-ctx env client-k entity-k)
        signature (crypto/gen-signature "POST"
                                        entity-route
                                        request-params
                                        api-key)
        signed-params (assoc request-params
                             "signature" signature)]
    (http/post url
               {:form-params signed-params
                :headers (conj {"content-type" "application/x-www-form-urlencoded"}
                               headers)})))


(defmethod action [:create :sdk :message] ;; :sdk, :hs-api, :dashboard
  [op-k client-k entity-k
   {:keys [env client-type app-id api-key
           issue-id
           request-params headers]
    :or {env :localshiva}
    :as payload-ctx}]
  (let [{:keys [url entity-route]} (make-url-ctx env client-k entity-k [issue-id])
        signature (crypto/gen-signature "POST"
                                        entity-route
                                        request-params
                                        api-key)
        signed-params (assoc request-params
                             "signature" signature)]
    (http/post url
               {:form-params signed-params
                :headers (conj {"content-type" "application/x-www-form-urlencoded"}
                               headers)})))


(defmethod action [:wait :any :any]
  [op-k client-k entity-k
   {:keys [env milliseconds]
    :or {env :localshiva}
    :as payload-ctx}]
  (Thread/sleep milliseconds))


;;performs agent login via login http calls
;;stores cookies in cookie-store post login
;;so that cookies can be used in further xhr calls
(defmethod action [:create :anon :login]
  [op-k client-k entity-k
   {:keys [env client-type
           user-agent cookie-store
           request-params headers]
    :or {env :localshiva
         cookie-store (clj-http.cookies/cookie-store)}
    :as action-ctx}]
  (let [{:keys [url]} (make-url-ctx env client-k entity-k)]
    (binding [clj-http.core/*cookie-store* cookie-store]
      (http/get url
                {:headers headers
                 :cookie-store clj-http.core/*cookie-store*})
      (http/post url
                 {:headers headers
                  :form-params (conj {:_csrf_token (-> clj-http.core/*cookie-store*
                                                       clj-http.cookies/get-cookies
                                                       (get "_csrf_token")
                                                       :value)}
                                     request-params)
                  :cookie-store clj-http.core/*cookie-store*})
      ;; return the cookies generated out of login
      (hash-map :cookie-store clj-http.core/*cookie-store*))))


;;agent adding new message to an issue via xhr call
(defmethod action [:create :xhr :message]
  [op-k client-k entity-k
   {:keys [env client-type
           cookie-store
           request-params headers]
    :or {env :localshiva
         cookie-store (clj-http.cookies/cookie-store)}
    :as action-ctx}]
  (let [{:keys [url]} (make-url-ctx env client-k entity-k ["add"])]
    (binding [clj-http.core/*cookie-store* cookie-store]
      (http/post url
                 {:headers headers
                  :form-params (conj {:_csrf_token (-> clj-http.core/*cookie-store*
                                                       clj-http.cookies/get-cookies
                                                       (get "_csrf_token")
                                                       :value)}
                                     request-params)
                  :cookie-store clj-http.core/*cookie-store*}))))


;; agent updating the issue
;; e.g. updating status of the issue to resolved
(defmethod action [:update :xhr :issue]
  [op-k client-k entity-k
   {:keys [env client-type
           user-agent cookie-store
           issue-status issue-id
           request-params headers]
    :or {env :localshiva
         cookie-store (clj-http.cookies/cookie-store)}
    :as action-ctx}]
  (let [{:keys [url]} (make-url-ctx env client-k entity-k ["edit"])]
    (binding [clj-http.core/*cookie-store* cookie-store]
      (http/post url
                 {:headers headers
                  :form-params (conj {:_csrf_token (-> clj-http.core/*cookie-store*
                                                       clj-http.cookies/get-cookies
                                                       (get "_csrf_token")
                                                       :value)}
                                     request-params)
                  :cookie-store clj-http.core/*cookie-store*}))))


(comment
  (action :create :anon :login
          {})

  (make-url-ctx :localshiva :xhr :message ["add"])
  )
