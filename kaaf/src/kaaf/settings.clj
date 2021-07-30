(ns kaaf.settings
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.util Properties]))

(defonce ^{:private true :doc "properties map"} properties-map (atom {}))


(defn load-properties-file
  "loads and reads the config
  properties file"
  [file-name]
  (let [props-file (-> file-name
                       io/resource
                       io/file)
        ps (reduce (fn [x [y z]]
                     (assoc x y z))
                   {} (doto (new Properties)
                        (.load (io/input-stream props-file))))]
    (reset! properties-map ps)))


(defn load-routes-config
  "loads and reads the routes
  config file"
  []
  (-> "routes_config.clj"
      io/resource
      slurp
      edn/read-string))


(defn p>
  [k]
  (get @properties-map k))
