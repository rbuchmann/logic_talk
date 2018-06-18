(ns logic-talk.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [run* conde conda defne defna featurec ==]]))

(defne geto [m k v]
  ([[[k x] . _] _ x])
  ([[_     . r] _ _] (geto r k v)))


(defne evalo [f arg result]
  ([[:keyword k]          [:map m] _] (geto m k result))
  ([[:vector [fst . rst]] _        _] (l/fresh [head tail]
                                        (evalo fst arg head)
                                        (evalo [:vector rst] arg tail)
                                        (l/conso head tail result)))
  ([[:vector []]          _       []]))


(defn encode-value [x]
  (cond
    (keyword? x) [:keyword   x]
    (vector?  x) [:vector    x]
    (map?     x) [:map (vec x)]
    :default     x))

(defn decode-value [[k content]]
  (case k
    :keyword content
    :vector  content
    :map     (into {} content)
    k))

(defn encode-fn [x]
  (cond
    (keyword? x) [:keyword x]
    (vector? x)  [:vector (mapv encode-fn x)]
    (map? x)     [:map (vec (for [[k v] x]
                              [k (encode-fn v)]))]))

(defn decode-fn [[k content]]
  (case k
    :keyword content
    :vector (mapv decode-fn content)
    :map (into {} (for [[k v] content] [k (decode-fn v)]))))

(defn eval-code [f arg]
  (run* [q]
    (evalo (encode-fn f) (encode-value arg) q)))

































(defn guess-code [arg result]
  (-> (l/run 1 [q]
        (evalo q (encode-value arg) result))
      first
      decode-fn))
