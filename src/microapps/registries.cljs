(ns microapps.registries
  (:require [microapps.utils :refer [fn-name]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [microapps.macros :refer [kmap]]))

;;; Atoms

(def producers (atom []))
(def consumers (atom []))
(def transformers (atom []))
(def enabled (atom #{}))
(def ordered (atom []))

;;; Setters

(defn add-consumer [spec handler]
  (swap! consumers conj [spec handler])
  [spec handler])

(defn add-producer [call-producer spec]
  (swap! producers conj [spec identity])
  (partial call-producer spec))

(defn add-transformer [spec handler]
  (swap! transformers conj [spec handler])
  [spec handler])

(defn add-constant-producer [spec handler type]
  (swap! producers conj (with-meta [spec handler] {type true}))
  [spec handler])

;;; Getters

;; TODO: replace with constants on invoke of microapp.apps/run
(defn get-consumers []
  (->> @consumers
       (filter @enabled)
       (sort-by #(or (get @ordered %)
                     99999))))

(defn get-producers []
  (filter @enabled @producers))

(defn get-transformers []
  (->> @transformers
       (filter @enabled)
       (sort-by #(or (get @ordered %)
                     99999))))

;;; Utils

(defn reset-registries []
  (reset! producers [])
  (reset! consumers [])
  (reset! transformers []))

(defn print-registries []
  (let [reducer #(reduce-kv (fn [acc k v]
                              (assoc acc k (fn-name v)))
                            {}
                            (deref %1))
        [consumer transformers] (map reducer [consumers transformers])
        producers @producers]
    (pprint (kmap consumer transformers producers))))
