(ns kaaf.dsl.core
  "Action implementation for different actions available in moby."
  (:require [kaaf.utils.crypto :as crypto]
            [clojure.walk :as cw]
            [clj-http.client :as http]
            clj-http.cookies
            clj-http.core
            clojure.pprint
            [kaaf.data.mocks :as mock]
            [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [kaaf.settings :as ks]))


(def http-verb?
  "Different HTTP actions available"
  #{"GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE" "PATCH"})

(def known-env?
  "Different environments available for execution"
  #{:localshiva :ash :sandbox})

(def allowed-operation?
  "Different operations available in the
  moby actions implemented in kaaf"
  #{:create :read :update :delete :wait})

(def reserved-keyword?
  "Reserved keywords allowed in the action DSL
  specified in Scenarios.edn"
  #{:any})


(defn properties-file
  "Reads the config.properties file present in
  resources directory."
  []
  (ks/load-properties-file "config.properties"))


(defn environment
  "Fetches the environment against which the
  tests need to be executed"
  []
  (keyword (get (properties-file)
                "test.env")))


(defn entity-info
  "Fetches the entity client and entity routes info from routes_config.
  This is present in resources directory"
  []
  "Exactly match request routes to what you find in moby/api.lib.routes
  - DO THIS:    {:faq \"/faqs/%s/\"}
  - DO NOT DO:  {:faq  \"faqs/%s\"}"
  (ks/load-routes-config))


(defn- inject-reserved-keywords
  "Injects the reserved keywords into the expected list of clients."
  [dsl-fragment-set]
  {:pre [(not-any? reserved-keyword? dsl-fragment-set)]}
  (into dsl-fragment-set reserved-keyword?))

(def valid-client?
  "Creates a hash of valid clients"
  (inject-reserved-keywords
   #{:anon :sdk :hs-api :webui :dashboard :bot :email :xhr}))

(defn valid-entity-k?
  "validates entity details coming from Scenarios.edn"
  []
  "Move out into a clojure.spec spec later, as the DSL gets better defined."
  (->> (entity-info)
       :entity
       vals
       (mapcat keys)
       (into #{})
       inject-reserved-keywords))


(defn get-test-app-id
  "Fetches the platform-id of the app under test"
  []
  (get (properties-file)
       "test.platform-id"))


(defn get-client-config
  "Fetches the mobile sdk api key"
  []
  (let [properties (properties-file)]
    {:sdk-api-key (properties "test.api-token")}))


(defn get-agent-username
  "Fetches the admin username"
  []
  (get (properties-file)
       "test.admin-username"))


(defn get-agent-password
  "Fetches the admin password"
  []
  (get (properties-file)
       "test.admin-password"))


(defn get-user-profile
  "Fetches the user profile id of the test app"
  []
(get (properties-file)
       "test.profile-id"))


(defn get-user-email
  "Fetches the email id of the user of the test app"
  []
  (get (properties-file)
       "test.profile-email"))


(defn get-app-config
  "Fetches client config along with environment"
  [env]
  (-> (get-client-config)
      (assoc :env env)))


(defn dsl-implemented?
  "Verifies whether specified DSL is implemented or not"
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
      str/trim ;; trim before deslashing, _and_ after as well
      (str/replace #"^/|/$" "")
      str/trim))


(defn get-app-domain
  "Fetches the domain name of the app under test"
  []
  (get (properties-file)
       "test.domain"))


(defn gen-url-ctx
  "Produce a map of well-formed URL parts, and the full url that conform's
  to moby's expectations, as seen in
  - moby/api.lib.routes and
  - moby/api.lib.ring.middleware."
  ([client-k entity-k app-id]
   (gen-url-ctx
    (entity-info) client-k entity-k app-id []))
  ([client-k entity-k app-id args]
   (gen-url-ctx
    (entity-info) client-k entity-k app-id args))
  ([entity-map client-k entity-k app-id args]
   {:pre [(every? (partial contains? entity-map)
                  [:client :entity])
          (known-env? (environment))
          ((valid-entity-k?) entity-k)
          (and (coll? args)
               (= (count args)
                  ;; all formatters are %-prefixed
                  ;; ref: https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html
                  (count (re-seq #"%"
                                 (get-in entity-map
                                         [:entity client-k entity-k])))))]}
   (let [get-clean-fragment (comp
                             strip-clean-uri-fragment
                             (partial get-in entity-map))
         domain (get-app-domain)
         base-url (str "http://" domain)
         plat-path (get-clean-fragment [:client client-k])
         entity-route-pattern (get-clean-fragment [:entity client-k entity-k])
         entity-route (format "/%s/%s/" ;; must have leading and trailing slashes
                              plat-path
                              (apply format entity-route-pattern args))]
     {:base-url base-url
      :entity-route entity-route
      :url (str base-url entity-route)})))


(defn filter-response
  "Filters out the API response with only desired and required fields."
  [response-with-result headers]
  (let [resp-status-body (select-keys (:response response-with-result)
                                      [:status :body])
        resp-success? (:success? response-with-result)]
    {:headers headers
     :response resp-status-body
     :success? resp-success?}))


(defn make-xhr-call
  "Makes XHR API calls to moby."
  [method url params cookies]
  (let [headers (mock/gen-headers-xhr)
        form-params (assoc params :_csrf_token (-> cookies
                                                   clj-http.cookies/get-cookies
                                                   (get "_csrf_token")
                                                   :value))
        resp-with-result (try
                           (binding [clj-http.core/*cookie-store* cookies]
                             (case method
                               :post {:response (http/post url
                                                           {:headers headers
                                                            :query-params {"next" "/admin/"}
                                                            :form-params form-params})
                                      :success? true}
                               :get {:response (http/get url
                                                         {:headers headers
                                                          :query-params params})
                                     :success? true}))
                           (catch Exception ex
                             {:response (ex-data ex)
                              :success? false}))]
    (filter-response resp-with-result
                     headers)))


(defn make-sdk-api-call
  "Makes mobile SDK API calls to moby."
  [ctx-app ctx-entity method url entity-route base-params]
  (let [ctx-entity (merge base-params ctx-entity)
        request-params (into (sorted-map) (cw/stringify-keys ctx-entity))
        headers (mock/gen-headers-sdk)
        api-key (:sdk-api-key ctx-app)
        signature (crypto/gen-signature :post
                                        entity-route
                                        request-params
                                        api-key)
        ctx-with-signed-params (assoc request-params
                                      "signature" signature)
        resp-with-result (try
                           (case method
                             :post {:response (http/post url
                                                         {:form-params ctx-with-signed-params
                                                          :headers headers})
                                    :success? true}
                             :get {:response (http/get url
                                                       {:query-params ctx-with-signed-params
                                                        :headers headers})
                                   :success? true})
                           (catch Exception ex
                             {:response (ex-data ex)
                              :success? false}))]
    (filter-response resp-with-result
                     headers)))


(defmulti action
  "Action multimethods.
  Each method is an implementation of a moby action."
  (fn [{[op-key client-key entity-key] :action
        description :description
        ctx :ctx
        contract :contract
        :as dsl-action-definition} data-collector]
    ;;This line is failing when kaaf is used as
    ;;a library in another project. Hence, commenting
    ;;it out. It's a known issue around specs :-
    ;;https://github.com/HCADatalab/powderkeg/issues/29
    #_{:pre [(s/valid? :kaaf.dsl.spec/dsl
                       dsl-action-definition)]}
    [op-key client-key entity-key]))


(defmethod action :default
  [dsl-action-definition _]
  (throw (Exception. (format "Unsupported action definition %s"
                             dsl-action-definition))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; User Profile Actions ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this method requires changes. Should be done
;; when we are dealing with the dsl action for
;; creating profile
(defmethod action [:create :sdk :profiles]
  [{[op-key client-key entity-key] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity} :ctx
    contract :contract
    :as dsl-action-definition} _]
  (let [{:keys [url entity-route]} (gen-url-ctx client-key
                                                entity-key
                                                (get-test-app-id))
        base-params (into (sorted-map)
                          (cw/stringify-keys (dissoc ctx-entity
                                                     :headers)))]
    (make-sdk-api-call ctx-client
                       ctx-entity
                       :post
                       url
                       entity-route
                       base-params)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Login Actions ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action [:create :anon :login]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} _]
  ;; Enforce mandatory requirements specific to this type of action.
  {:pre [(or (empty? ctx-entity)
             (every? #{:username :password}
                     (keys ctx-entity)))]}
  ;; Construct or extract information specific to this action.
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id)
        base-params {:username (get-agent-username)
                     :password (get-agent-password)}
        request-params (merge base-params
                              ctx-entity)
        cookie-store (clj-http.cookies/cookie-store)
        headers {:user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:72.0) Gecko/20100101 Firefox/72.0"
                 :x-hs-request-id (mock/gen-uuid-str)}]
    (binding [clj-http.core/*cookie-store* cookie-store]
      (http/get url
                {:headers headers
                 :query-params {"next" "/admin/"}})
      {:headers (cw/stringify-keys headers)
       :response (select-keys (http/post url
                                         {:headers headers
                                          :query-params {"next" "/admin/"}
                                          :form-params (assoc request-params
                                                              :_csrf_token (-> clj-http.core/*cookie-store*
                                                                               clj-http.cookies/get-cookies
                                                                               (get "_csrf_token")
                                                                               :value))})
                              [:status])
       :data {:cookies clj-http.core/*cookie-store*}
       :success? true})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Issue Actions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod action [:create :sdk :issue]
  [{[op-key client-key entity-key] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    contract :contract
    :as dsl-action-definition} _]
  {:pre [(or (empty? ctx-entity)
             ;; Check if :entity of dsl action has valid params
             ;; required by create -> sdk -> issue request
             (every? #{:message-text :type :platform-id :profile-id :timestamp}
                     (keys ctx-entity)))]}
  (let [test-app-id (get-test-app-id)
        {:keys [url entity-route]} (gen-url-ctx client-key
                                                entity-key
                                                test-app-id)
        ;; Basic parameters required for :create :sdk :issue. if dsl
        ;; action does not have :entity section then these params will
        ;; be used
        base-params {:message-text "Message from user"
                     :type "txt"
                     :platform-id test-app-id
                     :profile-id (get-user-profile)
                     :timestamp (mock/gen-hsft-timestamp)}
        {:keys [headers response success?]} (make-sdk-api-call ctx-client
                                                               ctx-entity
                                                               :post
                                                               url
                                                               entity-route
                                                               base-params)
        response-body (json/read-str (get response :body)
                                     :key-fn keyword)
        ;; fields of the response body expected to be used in subsequent
        ;; requests
        desired-fields {:issue-id (get response-body
                                       :id "issue id not found in response")
                        :publish-id (get response-body
                                         :publish_id "publish id not found in response")}
        response (assoc response
                        :body
                        response-body)]
    {:headers headers
     :response response
     :data desired-fields
     :success? success?}))


(defmethod action [:create :sdk :message]
  [{[op-key client-key entity-key] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    contract :contract
    :as dsl-action-definition}
   data-collector]
  {:pre [(or (empty? ctx-entity)
             (every? #{:message-text :type :platform-id :profile-id :timestamp :issue_id :general}
                     (keys ctx-entity)))]}
  (let [test-app-id (get-test-app-id)
        base-params {:message-text "Another message from user"
                     :type "txt"
                     :platform-id test-app-id
                     :profile-id (get-user-profile)
                     :timestamp (mock/gen-hsft-timestamp)
                     :issue-id (if-let [issue-id (:issue-id ctx-entity)]
                                 issue-id
                                 (get data-collector
                                      :issue-id))}
        {:keys [url entity-route]} (gen-url-ctx client-key
                                                entity-key
                                                test-app-id
                                                [(get data-collector :issue-id)])
        {:keys [headers response success?]} (make-sdk-api-call ctx-client
                                                               ctx-entity
                                                               :post
                                                               url
                                                               entity-route
                                                               base-params)]
    {:headers headers
     :response response
     :success? success?}))


(defmethod action [:read :xhr :issue-details]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} data-collector]
  {:pre [(or (empty? ctx-entity)
             (every? #{:publish_id :viewing :get_tmpl :show_faq_suggestion
                       :issue_id :issue_author_id :user_email :user_id}
                     (keys ctx-entity)))]}
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id
                                   ["view" ""])
        base-params {:publish_id (get data-collector
                                      :publish-id)
                     :issue_id (get data-collector
                                    :issue-id)
                     :viewing true
                     :get_tmpl false
                     :show_faq_suggestion true
                     :issue_author_id (get-user-profile)
                     :user_email (get-user-email)
                     :user_id "undefined"}
        request-params (merge base-params
                              ctx-entity)
        cookies (get data-collector
                     :cookies)]
    (make-xhr-call :get
                   url
                   request-params
                   cookies)))


(defmethod action [:create :xhr :message]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} data-collector]
  ;; Enforce mandatory requirements specific to this type of action.
  {:pre [(or (empty? ctx-entity)
             (every? (set (keys ctx-entity))
                     [:body]))]}
  ;; Construct or extract information specific to this specific action.
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id
                                   ["add"])
        base-params {:body "We are looking into your payment issue"
                     :issue_id (get data-collector :issue-id)}
        request-params (merge base-params
                              ctx-entity)
        cookies (get data-collector
                     :cookies)]
    ;; Perform the specific action.
    (make-xhr-call :post
                   url
                   request-params
                   cookies)))


(defmethod action [:update :xhr :issue]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} data-collector]
  ;; Enforce mandatory requirements specific to this type of action.
  {:pre [(or (empty? ctx-entity)
             (every? #{:status :id :tags}
                     (keys ctx-entity)))]}
  ;; Construct or extract information specific to this specific action.
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id
                                   ["edit"])
        ctx-entity (if-let [tags (get ctx-entity :tags)]
                     (assoc ctx-entity
                            :tags
                            (str "[\"" tags "_" (get data-collector :tag-guid) "\"]"))
                     ctx-entity)
        base-params {:id (get data-collector
                              :issue-id)}
        request-params (merge base-params
                              ctx-entity)
        cookies (get data-collector
                     :cookies)]
    ;; Perform the specific action.
    (make-xhr-call :post
                   url
                   request-params
                   cookies)))


(defmethod action [:update :xhr :issue-details]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} data-collector]
  {:pre [(or (empty? ctx-entity)
             (every? #{:issue_id :cif}
                     (keys ctx-entity)))]}
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id
                                   ["view" "cif"])
        ctx-entity (if-let [cif (get ctx-entity :cif)]
                     (assoc ctx-entity
                            :cif (str "{\"id\":\""
                                      (get data-collector :cif)
                                      "\",\"value\":\"2.30\"}"))
                     ctx-entity)
        base-params {:issue_id (get data-collector
                                    :issue-id)}
        request-params (merge base-params
                              ctx-entity)
        cookies (get data-collector
                     :cookies)]
    (make-xhr-call :post
                   url
                   request-params
                   cookies)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; TAGS CRUD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod action [:create :xhr :tags]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} data-collector]
  {:pre [(or (empty? ctx-entity)
             (every? #{:tags}
                     (keys ctx-entity)))]}
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id
                                   ["add"])
        guid (mock/gen-uuid-str)
        ctx-entity (if-let [tags (get ctx-entity :tags)]
                     (assoc ctx-entity
                            :tags (str "[\"" tags "_" guid "\"]"))
                     ctx-entity)
        base-params {:tags (str "[\"mytag_" guid "\"]")}
        request-params (merge base-params
                              ctx-entity)
        cookies (get data-collector
                     :cookies)
        {:keys [headers response success?]} (make-xhr-call :post
                                                           url
                                                           request-params
                                                           cookies)
        desired-fields {:tag-guid guid}]
    {:headers headers
     :response response
     :data desired-fields
     :success? success?}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; CIF CRUD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod action [:create :xhr :cif]
  [{[op-k client-k entity-k] :action
    description :description
    {{env :env} :op
     ctx-client :client
     ctx-entity :entity
     :or {ctx-client (get-client-config)
          ctx-entity {}}} :ctx
    :as dsl-action-definition} data-collector]
  {:pre [(or (empty? ctx-entity)
             (every? #{:type :key :label}
                     (keys ctx-entity)))]}
  (let [test-app-id (get-test-app-id)
        {:keys [url]} (gen-url-ctx client-k
                                   entity-k
                                   test-app-id
                                   ["add"])
        ;;key field does not accept hyphens
        guid (str/replace (mock/gen-uuid-str)
                          "-" "_")
        ctx-entity (if-let [label (get ctx-entity
                                       :label)]
                     (assoc ctx-entity
                            :label (str label
                                        "_"
                                        guid)
                            :key (str label
                                      "_"
                                      guid))
                     ctx-entity)
        base-params {:label (str "osversion_" guid)
                     :key (str "osversion_" guid)
                     :type "singleline"}
        request-params (merge base-params ctx-entity)
        cookies (get data-collector
                     :cookies)
        {:keys [headers response success?]} (make-xhr-call :post
                                                           url
                                                           request-params
                                                           cookies)
        desired-fields {:cif (get (json/read-str (get response
                                                      :body))
                                  "id")}]
    {:headers headers
     :response response
     :data desired-fields
     :success? success?}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; MISC ACTIONS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod action [:wait :any :any]
  [{[op-k client-k entity-k] :action
    description :description
    {ctx-op :op} :ctx
    :as dsl-action-definition} _]
  (let [{:keys [env ms]} ctx-op]
    (try
      (Thread/sleep ms)
      {:success? true}
      (catch Exception _
        {:success? false}))))


;; Verifying action multi methods with different options
;; Later will be moved to unit tests
(comment
  ;; FIRST: Verify for yourself that the business action map
  ;; conforms to our DSL spec.
  (Thread/sleep 1000)
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

  (action {:action [:create :sdk :issue]
           :description "User creates an issue via sdk"
           :ctx {:op {:env :localshiva}}
           :contract {:exactly-ordered [:create_issue_by_customer
                                        :first_reply_to_issue_by_customer]}} [])

  (http/post "http://nishanttesting.helpshift.mobi/api/lib/2/issues/"
             {:form-params {"message-text" "Test message from user",
                            "platform-id"
                            "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2",
                            "profile-id"
                            "nishanttesting_profile_20200203143823731-749c12230be1bf4",
                            "signature"
                            "0f118f4769a41e2c35e3b6a426c5d228ab29986b51fc54491f993b37591ffc27",
                            "timestamp" "1584208604.85",
                            "type" "txt"}
              :headers {"x-hs-request-id" "02162e51-0d45-452d-851d-3ba72a68ab7e",
                        "content-type" "application/x-www-form-urlencode"}})
  ;; Dump of runtime discussion with somya and nishant
  ;; How to think of constructing business "scenarios"?
  ;;
  ;; (scenario "Bulk assign and bulk reply"
  ;;  admin logs in with (new cookie store)
  ;;  -> (updated cookie store, with csrf for logged-in admin)
  ;;  -> with :contract {}, because we care about agent actions only here
  ;;  admin assigns n tickets to agent
  ;;  -> (passes throught cookie store in zmap)
  ;;  -> (passes throught issue ids in zmap)
  ;;  agent logs in (new cookie store)
  ;;  -> (passes throught cookie store in zmap)
  ;;  agent replies to all newly-assigned issues
  ;;  -> (passes throught cookie store in zmap)
  ;;  -> (passes throught issue ids in zmap)
  ;;  admin logs in again to do something
  ;;  -> :contract {}
  ;;  )

  ;; (scenario "Admin login events scenario."
  ;;           (admin logs in)
  ;;           (admin takes x action)
  ;;           (agent logs in)
  ;;           (agent takes x action)
  ;;           (admin takes y action)
  ;;           (admin takes y action)
  ;;           )
  )

;; Verifying dsl-implemented? with different options
;; Later will be moved to unit tests
(comment
  (contains? (methods kaaf.dsl.core/action) [:create :sdk :issue])

  (dsl-implemented? kaaf.dsl.core/action
                    [[:create :sdk :issue]
                     [:create :sdk :profiles]
                     [:reply :sdk :issue]
                     [:delete :dashboard :issue]])

  (dsl-implemented? kaaf.dsl.core/action
                    [[:create :sdk :issue]
                     [:create :sdk :profile]
                     [:reply :sdk :issue]
                     [:delete :dashboard :issue]])

  ((juxt (comp count :implemented)
         (comp count :not-implemented))
   (dsl-implemented? kaaf.dsl.core/action
                     kaaf.dsl.state-machine/all-possible-action-permutations)))


;; Verifying the gen-url-ctx with different options
;; Later will be moved to unit tests
(comment
  (gen-url-ctx :sdk :profiles "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2")
  (gen-url-ctx :xhr :tags "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2" ["add"])
  (gen-url-ctx :localshiva :xhr :issue-details "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2" ["view" ""])
  (ns-unmap *ns* 'action)
  (gen-url-ctx :localshiva :xhr :cif ["add"])
  (gen-url-ctx :localshiva :sdk :issue "nishanttesting_platform_20200111175204093-781dbe6bf2b35d2"))
