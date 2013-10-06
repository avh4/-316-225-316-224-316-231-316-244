(ns εδιτ.model
  (:refer-clojure :exclude [list?])
  (:use [midje.sweet]))

(defn new-function [] '{:params []  :body []})

(defn parameter-reference? [%]
  (and (clojure.core/list? %)
       (= (first %) 'εδιτ.internal/param)))
(defn list? [%]
  (and (vector? %)
       (= (first %) :list)))
