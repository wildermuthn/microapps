(ns microapps.utils
  (:require [clojure.string :as str]
            [cljs.pprint :refer [pprint]]))

(defn remove-fns [s]
  (str/replace
    (str/replace
      s
      #"\n"
      "")
    #"function.(.*?)\(.*?\).*;\}"
    #(last (.split (second %) "$"))))

(defn fn-name [f]
  (keyword (remove-fns (str f))))

(defn timer [f & params]
  (do
    (.time js/console "timer")
    (apply f params)
    (.timeEnd js/console "timer")))

(defn spec-value-map [values specs]
  (reduce (fn [acc spec] (assoc acc spec (get values spec)))
          {}
          specs))