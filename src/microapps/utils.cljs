(ns microapps.utils
  (:require [clojure.string :as str]))

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
