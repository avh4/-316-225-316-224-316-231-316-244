(ns εδιτ.core
  (:use [midje.sweet]
        [lamina.core])
  (:require [net.avh4.geometry.rect :as rect]
            [εδιτ.export.clojure :as clojure]
            [εδιτ.ui.input :as input]
            [εδιτ.input.clojure])
  (:import [javax.swing JComponent]
           [java.awt Color RenderingHints]))

(def ^:private edit (find-ns 'εδιτ.input.clojure))
(def ^:private edit-new-selection (ns-resolve edit 'new-selection))
(def ^:private edit-input (ns-resolve edit 'input))

(def ^:private save-green (.darker Color/GREEN))
(def ^:private save-red   (.darker Color/RED))

;; UI
(defn fill-rect [g bounds color]
  (.setColor g color)
  (.fillRect g (bounds 0) (bounds 1) (bounds 2) (bounds 3)))
;;

(defn- paint-save-bar [g saved bounds]
  (let [[save-color save-text]
        (if saved [save-green "Saved"] [save-red "Modified"])]
    (doto g
      (fill-rect bounds save-color)
      (.setColor Color/BLACK)
      (.drawString save-text 10 (- (rect/max-y bounds) 7)))))

(defn keymap [k]
  (cond
   (char? k) k
   (keyword? k) (k {:tab :next
                    :command-P :param})
   :else (throw (RuntimeException. (str "Invalid key: " k)))))

(defn new-display! [!doc]
  (let [keys (channel)
        !selection (ref (edit-new-selection))
        this (proxy [JComponent] []
               (paintComponent [g]
                 (let [bounds (rect/of-size (.getWidth this) (.getHeight this))
                       doc @!doc
                       saved (:saved doc)
                       text (:content doc)

                       save-bar-bounds (rect/bottom bounds 24)]
                   (doto g
                     (.setRenderingHints
                      {RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON
                       RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_DEFAULT
                       RenderingHints/KEY_FRACTIONALMETRICS RenderingHints/VALUE_FRACTIONALMETRICS_ON
                       })
                     (paint-save-bar saved save-bar-bounds)
                     (.setColor Color/BLACK)
                     (.drawString (pr-str (clojure/export (:model doc))) 10 30)
                     ))))]
    (.addKeyListener this (input/awt-listener keys keymap))
    (.setFocusTraversalKeysEnabled this false)
    (add-watch !doc this (fn [_ _ _ _] (.repaint this)))
    (add-watch !selection this (fn [_ _ _ _] (.repaint this)))
    (receive-all keys (fn [key]
                        (dosync
                         (let [[new-model new-sel] (edit-input (:model @!doc) @!selection key)]
                           (ref-set !selection new-sel)
                           (alter !doc assoc :model new-model)))))
    this))
