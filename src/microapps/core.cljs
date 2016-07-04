(ns microapps.core
  (:require [microapps.config :refer [development?]]
            [cljs.spec :as s]
            [goog.object :as obj]
            [microapps.sample]))

;;;; JS Interop

(defn js-print [s]
  (println s))

(defn js-make-keyword [s-ns s-key]
  (keyword s-ns s-key))

(defn js-make-spec [s pred]
  (let [k (keyword s)]
    (s/def k pred)))

(obj/set js/module "exports" #js {:print       js-print
                                  :makeKeyword js-make-keyword
                                  :makeSpec    js-make-spec
                                  :isValid     s/valid?
                                  :explain     s/explain-data})


