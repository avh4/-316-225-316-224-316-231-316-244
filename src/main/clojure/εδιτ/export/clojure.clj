(ns εδιτ.export.clojure
  (:use [midje.sweet])
  (:require [εδιτ.model :as model]))

(defn- function-name-symbol [name-or-nil]
  (if name-or-nil
    (symbol name-or-nil)
    'εδιτ.internal.unique/f0))

(defn- param-name-symbol [code]
  (if (:name code)
    (symbol (:name code))
    'εδιτ.internal.unique/p0))

(defn- translate-param-name [params code]

  (cond
   (model/parameter-reference? code)
   (param-name-symbol (nth params (second code)))
   (model/list? code)
   (map (partial translate-param-name params) (list* (rest code)))
   :else
   code))

(defn- translate-param-names [params code]
  (map (partial translate-param-name params) code))

(defn- function-body [params code]
  (translate-param-names params code))

(defn- function-params [code]
  (if code
    (into [] (map param-name-symbol code))
    []))

(defn export [code]
  (concat (list 'defn
                (function-name-symbol (:name code))
                (function-params (:params code)))
          (function-body (:params code) (:body code))))

(facts "about export"
       (fact "renders function with no name"
             (export {})
             => '(defn εδιτ.internal.unique/f0 []))
       (fact "renders function name"
             (export {:name "hello" :body []})
             => '(defn hello []))
       (fact "renders simple function body"
             (export {:name "hello" :body [[:list]]})
             => '(defn hello [] ())
             (export {:name "hello" :body [[:list 'str]]})
             => '(defn hello [] (str))
             (export {:name "hello" :body [[:list 'str "Hello "]]})
             => '(defn hello [] (str "Hello ")))
       (fact "renders simple function with parameter reference"
             (export {:name "hello" :params [{}]
                      :body [[:list 'str "Hello " '(εδιτ.internal/param 0)]]})
             => '(defn hello [εδιτ.internal.unique/p0]
                   (str "Hello " εδιτ.internal.unique/p0))
             (export {:name "hello" :params [{:name "name"}]
                      :body [[:list 'str "Hello " '(εδιτ.internal/param 0)]]})
             => '(defn hello [name] (str "Hello " name))
              (export {:name "hello" :params [{:name "name"}]
                       :body [[:list 'str "Hello " '(εδιτ.internal/param 0) "!"]]})
             => '(defn hello [name] (str "Hello " name "!"))))
