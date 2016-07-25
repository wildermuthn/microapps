(ns microapps.apps
  (:refer-clojure :exclude [print])
  (:require [microapps.config :refer [development?]]
            [microapps.utils :refer [fn-name spec-value-map]]
            [microapps.registries :as registry]
            [cljs.spec :as s]
            [cljs.nodejs :as nodejs]
            [cljs.pprint :refer [pprint]]
            [cljs.reader]
            [clojure.set :refer [difference]]
            [goog.object :as obj])
  (:require-macros [microapps.macros :refer [kmap]]))

;;; Declarations

(declare run)

;;; NodeJS Inits

(nodejs/enable-util-print!)
(def -main (fn [] (println (str "development? " development?))))
(set! *main-cli-fn* -main)

;;; Specs

(s/def ::on-start ::s/any)

(s/def ::key-or-spec (s/or :k keyword?
                           :spec #(s/spec? %)))

(s/def ::spec-in ::key-or-spec)
(s/def ::spec-out ::key-or-spec)
(s/def ::handler fn?)
(s/def ::type #{:pure :repeat})
(s/def ::app (s/and (s/keys :opt-un [::type])
                    (s/or
                      ::a-transformer (s/keys :req-un [::spec-in ::spec-out
                                                       ::handler])
                      ::a-consumer (s/keys :req-un [::spec-in ::handler])
                      ::a-producer (s/keys :req-un [::spec-out]))))


;;;; App Invocation

(defn call-producer [[_ spec-out] msg]
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
  [app-results]
  (let [apps (registry/get-transformers)]
    (doseq [app apps]
      (invoke-app app @app-results (partial call-handler app-results app)))))

(defn invoke-consumers
  [app-results]
  (let [consumers (registry/get-consumers)]
    (doseq [app consumers]
      (invoke-app app @app-results (last app)))))

(defn invoke-constant-producers
  [app-results]
  (let [producers (registry/get-producers)]
    (doseq [app (filter #(or (:pure (meta %))
                             (:repeat (meta %))) producers)]
      (invoke-app app @app-results (partial call-handler app-results app)))))

(defn merge-invoked-value
  [app-results value]
  (swap! app-results merge value))

;;; Main loop

(defn run [& [invoked-producer-value]]
  (let [app-results (atom {})]
    (invoke-constant-producers app-results)
    (merge-invoked-value app-results invoked-producer-value)
    (invoke-transformers app-results)
    (invoke-consumers app-results)))

;;; Validation and Registration

(defn register-app [app]
  (let [{:keys [spec-in spec-out handler type]} app]
    (cond
      (and (= type :pure) spec-in spec-out) (registry/add-transformer [spec-in spec-out] (memoize handler))
      (= type :pure) (registry/add-constant-producer [spec-in spec-out] (memoize handler) :pure)
      (= type :repeat) (registry/add-constant-producer [spec-in spec-out] handler :repeat)
      (and spec-in spec-out) (registry/add-transformer [spec-in spec-out] handler)
      spec-in (registry/add-consumer [spec-in nil] handler)
      spec-out (registry/add-producer call-producer [nil spec-out])
      :else nil)))

(defn enable-apps [apps]
  (reset! registry/enabled apps))

(defn order-apps [apps]
  (reset! registry/ordered
          (into {} (map-indexed (fn [i e]
                                  [e i]) apps))))

(s/fdef register-app
  :args (s/cat :app ::app))

(s/instrument #'register-app)