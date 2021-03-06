(ns microapps.macros
  (:require [cljs.spec :as s]))

(defmacro kmap [& args]
  (let [ks (mapv keyword args)
        vs (mapv identity args)]
    `(zipmap ~ks ~vs)))
