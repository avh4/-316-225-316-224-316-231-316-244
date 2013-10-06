(ns εδιτ.util
  (:use [midje.sweet]))

(defn update-last [vec f]
  (update-in vec [(-> vec count dec)] f))
