(require '[clojure.data.json :as json]
         '[qa.fixtures.kaaf :as qfk])


(defn setup
  [data]
  (let [input (json/read-str data)
        result (qfk/setup input)]
    (json/write-str result :escape-slash false)))


(defn teardown
  [data]
  (qfk/teardown (json/read-str data)))
