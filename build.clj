(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b])
  (:require [org.corfield.build :as bb]))

(def lib 'net.clojars.NightMachinery/vcard-to-json)
(def version "0.1.0-SNAPSHOT")
(def main 'NightMachinery.vcard-to-json)

(defn rebuild-uber
  "Deletes the old artifacts and builds a new uberjar."
  [opts]
  (-> opts
      (assoc :lib lib :version version :main main)
      ;; (bb/run-tests)
      (bb/clean)
      (bb/uber))
  )

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn ci "Run the CI pipeline of tests (and build the uberjar)." [opts]
  (-> opts
      (assoc :lib lib :version version :main main)
      (bb/run-tests)
      (bb/clean)
      (bb/uber)))
;;;
;;;
