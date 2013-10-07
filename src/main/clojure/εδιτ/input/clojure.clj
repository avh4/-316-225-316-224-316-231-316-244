(ns εδιτ.input.clojure
  (:use [midje.sweet])
  (:require [εδιτ.model :as model]
            [εδιτ.util :as util]))

(defn new-selection [] [:name])

(defn- process-body-input [_ _ _ _]) ; forward declaration
(defn- recurse-vector [params elem path key]
   (process-body-input params elem
                      (cons (-> path rest first inc) (-> path rest rest)) key))

(defn- append-string [p elem _ key] [p (str elem key)])
(defn- append-symbol [p elem _ key] [p (symbol (str elem key))])
(defn- create-string [p _ _ key] [p (str)])
(defn- create-param [params _ _ _]
  (let [index (count params)]
    [(assoc params index {}) (list 'εδιτ.internal/param index)]))
(defn- create-symbol [p _ _ key] [p (symbol (str key))])
(defn- append-param-name [params elem _ key]
  (let [index (second elem)]

    [(update-in params [index :name] str key)
     elem]))

(defn- process-body-input [params body path key]
  (let [index (first path)
        elem (if (== index (count body)) nil (nth body index))
        f (cond
           (nil? elem) (case key
                         \" create-string
                         :param create-param
                         create-symbol)
           (vector? elem) recurse-vector
           (string? elem) append-string
           (symbol? elem) append-symbol
           (model/parameter-reference? elem) append-param-name
           :else (throw (RuntimeException. (str "Unrecognized element: "
                                                (prn-str params body path key)))))
        [new-params new-body] (f params elem path key)]
    [new-params (assoc body index new-body)]))

(facts "about process-body-input"
       (fact "appends key to symbol"
             (process-body-input ..p.. ['st] [0] \r) => [..p.. ['str]])
       (fact "recurses into lists"
             (process-body-input ..p.. [[:list 'st]] [0 0] \r) => [..p.. [[:list 'str]]]
             (process-body-input ..p.. [:a [:list 'st]] [1 0] \r)
             => [..p.. [:a [:list 'str]]]
             (process-body-input ..p.. [:a [:list :b 'st]] [1 1] \r)
             => [..p.. [:a [:list :b 'str]]])
       (fact "creates next symbol"
             (process-body-input ..p.. [[:list]] [0 0] \s) => [..p.. [[:list 's]]]
             (process-body-input ..p.. [] [0] \s) => [..p.. ['s]])
       (fact "creates param name"
             (process-body-input [{}] ['(εδιτ.internal/param 0)] [0] \n)
             => [[{:name "n"}] ['(εδιτ.internal/param 0)]])
       (fact "appends param name"
             (process-body-input [{:name "nam"}] ['(εδιτ.internal/param 0)] [0] \e)
             => [[{:name "name"}] ['(εδιτ.internal/param 0)]]))

(defn input [model selection key]
  (case (first selection)
    :body
    (case key
      \u0028
      [(assoc-in model [:body] [[:list]])
       [:body 0 0]]
      :next
      [model (util/update-last selection inc)]
      (let [[new-params new-body] (process-body-input (:params model) (:body model) (rest selection) key)]
        [(-> model (assoc-in [:body] new-body) (assoc-in [:params] new-params))
         selection]))
    :name
    (case key
      :next [model [:body]]
      [(update-in model [:name] str key)
       selection])))

(def ^:private new-function model/new-function)

(defn- apply-keys
  ([model selection keys]
     (first (reduce (fn [[m s] k] (input m s k)) [model selection] keys)))
  ([keys] (apply-keys (new-function) [:name] keys)))

(facts "about input"
       (reduce input (new-function) [])
       => {:params []  :body []}
       (apply-keys [\h])
       => {:name "h" :params []  :body []}
       (apply-keys [\h \e \l \l \o])
       => {:name "hello" :params []  :body []}
       (apply-keys [\h \e \l \l \o :next \u0028])
       => {:name "hello" :params []  :body [[:list]]}
       (apply-keys [\h \e \l \l \o :next \u0028 \s])
       => {:name "hello" :params []  :body [[:list 's]]}
       (apply-keys [\h \e \l \l \o :next \u0028 \s \t \r])
       => {:name "hello" :params []  :body [[:list 'str]]}
       (apply-keys [\h \e \l \l \o :next \u0028 \s \t \r :next \"])
       => {:name "hello" :params []  :body [[:list 'str ""]]}
       (apply-keys
               [\h \e \l \l \o :next \u0028 \s \t \r :next \" \H \e \l \l \o \space])
       => {:name "hello" :params []  :body [[:list 'str "Hello "]]}
       (apply-keys
               [\h \e \l \l \o
                :next \u0028 \s \t \r
                :next \" \H \e \l \l \o \space
                :next :param])
       => {:name "hello" :params [{}]
           :body [[:list 'str "Hello " '(εδιτ.internal/param 0)]]}
       (apply-keys
               [\h \e \l \l \o
                :next \u0028 \s \t \r
                :next \" \H \e \l \l \o \space
                :next :param \n \a \m \e])
       => {:name "hello" :params [{:name "name"}]
           :body [[:list 'str "Hello " '(εδιτ.internal/param 0)]]}
       (apply-keys
               [\h \e \l \l \o
                :next \u0028 \s \t \r
                :next \" \H \e \l \l \o \space
                :next :param \n \a \m \e
                :next \" \!])
       => {:name "hello" :params [{:name "name"}]
           :body [[:list 'str "Hello " '(εδιτ.internal/param 0) "!"]]}
       )
