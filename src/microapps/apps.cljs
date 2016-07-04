(ns microapps.apps
  (:refer-clojure :exclude [print])
  (:require [microapps.config :refer [development?]]
            [microapps.utils :refer [fn-name]]
            [cljs.spec :as s]
            [cljs.nodejs :as nodejs]
            [cljs.pprint :refer [pprint]]
            [cljs.reader]
            [clojure.set :refer [difference]]
            [goog.object :as obj])
  (:require-macros [microapps.macros :refer [kmap]]))

;;; Declarations

(declare run producer-fn reset-registries print-registries
         consumer-registry producer-registry transformer-registry)

;;; NodeJS Inits

(nodejs/enable-util-print!)
(def -main (fn [] (println (str "development? " development?))))
(set! *main-cli-fn* -main)

;;; Spec Utils

(s/def ::on-start ::s/any)

;;; Registration Atoms

(def producer-registry (atom []))
(def consumer-registry (atom []))
(def transformer-registry (atom []))

;;; Registration Utils

(defn reset-registries []
  (reset! producer-registry [])
  (reset! consumer-registry [])
  (reset! transformer-registry []))

(defn print-registries []
  (let [reducer #(reduce-kv (fn [acc k v]
                              (assoc acc k (fn-name v)))
                            {}
                            (deref %1))
        [consumer transformers] (map reducer [consumer-registry transformer-registry])
        producers @producer-registry]
    (pprint (kmap consumer transformers producers))))

;;; Registration Adders

(defn add-consumer [spec handler]
  (swap! consumer-registry conj [spec handler]))

(defn add-producer [spec]
  (swap! producer-registry conj [spec identity])
  (partial producer-fn spec))

(defn add-transformer [spec handler]
  (swap! transformer-registry conj [spec handler]))

(defn add-constant-producer [spec handler type]
  (swap! producer-registry conj (with-meta [spec handler] {type true})))

(defn validate-app [app]
  (let [{:keys [spec-in spec-out]} app]
    (cond
      (and spec-in spec-out) [spec-in spec-out]
      spec-in spec-in
      spec-out spec-out
      :else (throw (js/Error. "App must have either a spec-in or spec-out")))))

(defn register-app [app]
  (validate-app app)
  (let [{:keys [spec-in spec-out handler type]} app]
    (cond
      (and (= type :pure) spec-in spec-out) (add-transformer [spec-in spec-out] (memoize handler))
      (= type :pure) (add-constant-producer [spec-in spec-out] (memoize handler) :pure)
      (= type :repeat) (add-constant-producer [spec-in spec-out] handler :repeat)
      (and spec-in spec-out) (add-transformer [spec-in spec-out] handler)
      spec-in (add-consumer [spec-in nil] handler)
      spec-out (add-producer [nil spec-out])
      :else (throw (js/Error. "App must have either a spec-in or spec-out")))))

;;;; App Handlers

(defn producer-fn [[_ spec-out] msg]
  (let [value (if-not (map? msg)
                {spec-out msg}
                msg)]
    (run value)))

(defn call-valid-handler [values app params]
  (let [[[spec-in spec-out] handler] app
        result (handler params)
        valid? (s/valid? spec-out result)
        explain (if-not valid?
                  (merge {:spec-in  spec-in
                          :spec-out spec-out}
                         (s/explain-data spec-out result)))]
    (if valid?
      (swap! values assoc spec-out result)
      (pprint [:output-invalid explain]))))

(defn call-handler [values app params]
  (let [[spec-in spec-out] (first app)
        spec-desc (s/describe spec-in)
        req-spec-in (if (and (list? spec-desc)
                             (= 'keys (first spec-desc)))
                      (s/keys)
                      spec-in)
        valid? (s/valid? req-spec-in params)
        explain (if-not valid?
                  (merge {:spec-in  spec-in
                          :spec-out spec-out}
                         (s/explain-data spec-in params)))]
    (if valid?
      (call-valid-handler values app params)
      (pprint [:input-invalid explain]))))

(defn spec-value-map [values specs]
  (reduce (fn [acc spec] (assoc acc spec (get values spec)))
          {}
          specs))

;;; Handler Invocations

(defn invoke-app
  [app values handler]
  (let [[spec-in _] (first app)
        spec-desc (s/form spec-in)
        direct-value (get values spec-in)]
    (cond

      ;; value from recursive loop
      direct-value (handler direct-value)

      ;; keys spec
      (and (list? spec-desc)
           (= 'cljs.spec/keys (first spec-desc)))
      (let [{:keys [req opt]} (->> (rest spec-desc)
                                   (apply hash-map)
                                   (map #(vector (first %) (set (last %))))
                                   (into {}))
            values-map (->> (spec-value-map values (concat req opt))
                            (filter (fn [[k v]]
                                      (if v
                                        true
                                        (req k))))
                            (into {}))]
        (handler values-map))

      ;; producer with no spec-in
      (not spec-in) (handler true)
      :else (when-let [params (get values spec-in)]
              (handler params)))))

(defn invoke-transformers
  [all-values output-already]
  (let [apps (filter (fn [[k v]]
                       (not (output-already (last k)))) @transformer-registry)]
    (doseq [app apps]
      (invoke-app app @all-values (partial call-handler all-values app)))))

(defn invoke-consumers
  [all-values]
  (doseq [app @consumer-registry]
    (invoke-app app @all-values (last app))))

(defn invoke-repeating-producers
  [all-values]
  (doseq [app (filter #(or (:pure (meta %))
                           (:repeat (meta %))) @producer-registry)]
    (invoke-app app @all-values (partial call-handler all-values app))))

(defn invoke-producer
  [all-values value]
  (swap! all-values merge value))

;;; Main loop

(defn run [& [producer]]
  (let [kill-n (atom 0)
        original-values (atom {})]
    (invoke-repeating-producers original-values)
    (invoke-producer original-values producer)
    (loop [all-values (atom @original-values)]
      (invoke-transformers all-values (-> @all-values keys set))
      (let [diff (difference (-> @all-values keys set)
                             (-> @original-values keys set))]
        (if (and (not= 0 (count diff))
                 (< @kill-n 10))
          (do
            (swap! kill-n inc)
            (reset! original-values @all-values)
            (recur all-values))
          (invoke-consumers all-values))))))
