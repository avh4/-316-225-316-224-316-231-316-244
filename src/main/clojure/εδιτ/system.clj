(ns εδιτ.system
  (:use εδιτ.core
        lamina.core)
  (:require [εδιτ.ui.input :as input]
            [εδιτ.model :as model])
  (:import [javax.swing JFrame SwingUtilities]
           [java.awt Dimension]))

(defn window [component]
  (let [this (JFrame.)]
    (.setPreferredSize component (Dimension. 800 600))
    (doto this
      (.add component)
      (.pack))
    (.requestFocusInWindow component)
    this))

(defn system []
  {:window (window (new-display! (ref {:saved false
                                       :model (model/new-function)})))})

(defn start [sys]
  (SwingUtilities/invokeLater
   (fn [] (doto (:window sys)
           (.setLocationRelativeTo nil)
           (.setVisible false)
           (.setVisible true)
           (.toFront)
           (.repaint)))))

(defn stop [sys]
  (doto (:window sys)
    (.dispose)))
