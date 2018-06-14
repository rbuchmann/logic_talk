(ns logic-talk.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [run* conde conda defne defna featurec ==]]))

(defne geto [m k v]
  ([[[k x] . _] _ x])
  ([[_     . r] _ _] (geto r k v)))


(defne evalo [f arg result]
  ([[:keyword k]          _ _] (geto arg k result))
  ([[:vector [fst . rst]] _ _] (l/fresh [head tail]
                                 (evalo fst arg head)
                                 (evalo [:vector rst] arg tail)
                                 (l/conso head tail result)))
  ([[:vector []]          _ []]))
