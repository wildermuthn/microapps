(ns microapps.macros
  (:require [cljs.spec :as s]))

(defmacro kmap [& args]
  (let [ks (mapv keyword args)
        vs (mapv identity args)]
    `(zipmap ~ks ~vs)))

(defmacro register-spec [ns pred]
  `(s/def ~ns ~pred))

(defmacro console-time
  [label & body]
  `(do
     (.time js/console ~label)
     (let [result# (do ~@body)]
       (.timeEnd js/console ~label)
       result#)))