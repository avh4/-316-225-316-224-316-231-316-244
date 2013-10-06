(ns user
  (:use εδιτ.core)
  (:require [εδιτ.system :as system]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer :all]
            [clojure.test :as test]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [clojure.stacktrace :refer (e print-stack-trace print-cause-trace)]))

(def system nil)

(defn init []
  (alter-var-root #'system
                  (constantly (system/system))))

(defn start []
  (system/start system))

(defn stop []
  (when system (system/stop system)))

(defn go []
  (init) (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
