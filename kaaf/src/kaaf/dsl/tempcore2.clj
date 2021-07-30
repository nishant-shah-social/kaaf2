(ns kaaf.dsl.core
  (:require [clojure.string :as string]
            [kaaf.utils.crypto :as crypto]
            [clojure.walk :as cw]
            [clj-http.client :as http]
            clj-http.cookies
            clj-http.core
            [kaaf.dsl.core :as dsl]
            [kaaf.data.mocks :as mock]
            [helpshift-settings.core :as settings
             :refer [load-settings s! s>]]
            [clojure.spec.alpha :as s]))


(def entity-info
  "Exactly match request routes to what you find in moby/api.lib.routes
  - DO THIS:    {:faq \"/faqs/%s/\"}
  - DO NOT DO:  {:faq  \"faqs/%s\"}"
  (load-settings :localshiva))

(def http-verb?
  #{"GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE" "PATCH"})

(def known-env?
  #{:localshiva :ash})

(def allowed-operation?
  #{:create :read :update :delete :wait})

(def reserved-keyword?
  #{:any})

(defn- inject-reserved-keywords
  [dsl-fragment-set]
  {:pre [(not-any? reserved-keyword? dsl-fragment-set)]}
  (into dsl-fragment-set reserved-keyword?))

(def valid-client?
  (inject-reserved-keywords
   #{:anon :sdk :hs-api :webui :dashboard :bot :email}))

(def valid-entity-k?
  "Move out into a clojure.spec spec later, as the DSL gets better defined."
  (->> entity-info
       :entity
       vals
       (map #(keys %))
       flatten
       (into #{})
       inject-reserved-keywords))


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
                                 (get-in entity-map [:entity client-k entity-k])))))]}
   (let [get-clean-fragment (comp
                             strip-clean-uri-fragment
                             (partial get-in entity-map))
         base-url    (get-clean-fragment [:env env-k :url])
         plat-path   (get-clean-fragment [:client client-k])
         entity-route-pattern (get-clean-fragment [:entity client-k entity-k])
         entity-route   (format "/%s/%s/" ;; must have leading and trailing slashes
                                plat-path
                                (apply format entity-route-pattern args))]
     {:base-url base-url
      :entity-route entity-route
      :url (str base-url entity-route)})))


(comment
  (make-url-ctx :localshiva :sdk :profiles)
  (ns-unmap *ns* 'action)
  )


(defmulti action
  (fn [{[op-key client-key entity-key] :action
        description :description
        ctx :ctx
        contract :contract
        :as dsl-action-definition}]
    {:pre [(s/valid? :kaaf.dsl.spec/dsl
                     dsl-action-definition)]}
    [op-key client-key entity-key]))


(defmethod action :default
  [dsl-action-definition]
  (throw (Exception. (format "Unsupported action definition %s"
                             dsl-action-definition))))


(defmethod action [:create :sdk :profiles]
  ;; FIXME: My argument list must conform with the :kaaf.dsl.spec/dsl spec.
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
  ;; FIXME: My argument list must conform with the :kaaf.dsl.spec/dsl spec.
  [{[op-key client-key entity-key] :action
    description :description
    {ctx-op :op
     ctx-app :client
     ctx-entity :entity} :ctx
    contract :contract
    :as dsl-action-definition}]
  (let [{:keys [url entity-route]} (make-url-ctx (:env ctx-op) client-key entity-key)
        request-params (into (sorted-map) (cw/stringify-keys (dissoc ctx-entity :headers)))
        api-key (:api-key ctx-app)
        signature (crypto/gen-signature "POST"
                                        entity-route
                                        request-params
                                        api-key)
        ctx-with-signed-params (assoc request-params
                                      "signature" signature)
        headers (cw/stringify-keys (:headers ctx-entity))]
    #_(clj-http.cookies/get-cookies clj-http.core/*cookie-store*)
    #_(http/post url
                 {:form-params ctx-with-signed-params
                  :headers headers})
    {:url url :headers headers :params ctx-with-signed-params :api-key api-key :entity-route entity-route :requestparams request-params}))


(defmethod action [:create :sdk :message] ;; :sdk, :hs-api, :dashboard
  ;; FIXME: My argument list must conform with the :kaaf.dsl.spec/dsl spec.
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
    (clj-http.cookies/get-cookies clj-http.core/*cookie-store*)
    #_(http/post url
                 {:form-params signed-params
                  :headers (conj {"content-type" "application/x-www-form-urlencoded"}
                                 headers)})))


(defmethod action [:wait :any :any]
  ;; FIXME: My argument list must conform with the :kaaf.dsl.spec/dsl spec.
  [op-k client-k entity-k
   {:keys [env milliseconds]
    :or {env :localshiva}
    :as payload-ctx}]
  (Thread/sleep milliseconds))


(defmethod action [:create :anon :login]
  ;; See how my argument list conforms to the :kaaf.dsl.spec/dsl spec.
  [{[op-k client-k entity-k] :action
    {:keys [op client entity]} :ctx
    :as dsl-action-definition}]
  ;; Enforce mandatory requirements specific to this type of action.
  {:pre [(contains? client :cookie-store)
         (contains? client :user-agent)
         (contains? entity :username)
         (contains? entity :password)]
   ;; TODO: Figure out how to automate this post-condition check, OR do it
   ;; better, via clojure.spec, instead of having to add by hand.
   ;; Such a check in every method implemented will help enforce DSL-compliant
   ;; zmaps through the whole call chain for business actions. This in turn,
   ;; will help us build reliable "thrush" logic, which in turn will let us
   ;; make life convenient for test writers, by reducing the need to do manual
   ;; state management.
   :post [(s/valid? :kaaf.dsl.spec/dsl %)]}
  ;; Construct or extract information specific to this specific action.
  (let [{:keys [url]} (make-url-ctx (:env op) client-k entity-k)
        {:keys [env]} op
        {:keys [cookie-store user-agent]} client
        {:keys [username password]} entity]
    ;; Perform the specific action.
    ;; TODO: Error handling for this action (e.g. handle HTTP errors)
    (binding [clj-http.core/*cookie-store* cookie-store]
      (http/get url
                {:headers user-agent
                 :query-params {"next" "/admin/"}
                 :cookie-store clj-http.core/*cookie-store*})
      (http/post url
                 {:headers user-agent
                  :query-params {"next" "/admin/"}
                  :form-params {:username username
                                :password password
                                :_csrf_token (-> clj-http.core/*cookie-store*
                                                 clj-http.cookies/get-cookies
                                                 (get "_csrf_token")
                                                 :value)}
                  :cookie-store clj-http.core/*cookie-store*})
      ;; Finally, return input map with values updated as necessary.
      ;; - Try to provide thrush-like semantics, to reduce the need for explicit
      ;;   state management.
      ;; - For example, here we can simply return input ctx, with the updated
      ;;   cookie store.
      ;; - Pass through any other piece of new/updated information that may be
      ;;   needed by later actions, downstream from this action.
      (assoc-in dsl-action-definition
                [:ctx :client :cookie-store] clj-http.core/*cookie-store*))))


(comment
  ;; FIRST: Verify for yourself that the business action map
  ;; conforms to our DSL spec.
  (let [browser-user-agent-string
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0"]
    (s/valid? :kaaf.dsl.spec/dsl
              {:action [:create :anon :login]
               :description "Perform first-time login to the Moby dashboard, and grab hold of cookies."
               :ctx {:op {:env :localshiva}
                     :client {:user-agent {"User-Agent" browser-user-agent-string}
                              :cookie-store (clj-http.cookies/cookie-store)}
                     :entity {:username "user+admin+1@helpshift.com"
                              :password "No fate123!"}}
               :contract {}}))

  ;; THEN: Perform the action itself, to debug any problems with
  ;; the actual HTTP call.
  (let [browser-user-agent-string
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0"]
    (action {:action [:create :anon :login]
             :description "Perform first-time login to the Moby dashboard, and grab hold of cookies."
             :ctx {:op {:env :localshiva}
                   :client {:user-agent {"User-Agent" browser-user-agent-string}
                            :cookie-store (clj-http.cookies/cookie-store)}
                   :entity {:username "user+admin+1@helpshift.com"
                            :password "No fate123!"}}
             :contract {}}))

  (s/valid? :kaaf.dsl.spec/dsl
            {:action [:create :sdk :issue]
             :description "User creates an issue via sdk"
             :ctx {:op {:env :localshiva}
                   :client {:client-type :android
                            :app-name "MyFirstTestApp"
                            :app-id "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2"
                            :domain "nishanttesting.helpshift.mobi"}
                   :entity {:message-txt "Message from user"
                            :type "txt"
                            :platform-id mock/test-app-id
                            :timestamp (mock/gen-hsft-timestamp)
                            :headers {:x-hs-request-id (mock/gen-uuid-str)
                                      }}
                   :contract {:create_issue_by_customer :first_reply_to_issue_by_customer}}})
  )
