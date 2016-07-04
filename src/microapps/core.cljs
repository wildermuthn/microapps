(ns microapps.core
  (:refer-clojure :exclude [print])
  (:require [microapps.config :refer [development?]]
            [microapps.utils :refer [fn-name]]
            [cljs.spec :as s]
            [cljs.nodejs :as nodejs]
            [cljs.pprint :refer [pprint]]
            [cljs.reader]
            [goog.object :as obj])
  (:require-macros [microapps.macros :refer [kmap]]))

;;;; Initialization

(declare producer-fn run)

;;;; NodeJS Inits

(nodejs/enable-util-print!)
(def -main (fn [] (println (str "development? " development?))))
(set! *main-cli-fn* -main)

;;;; Spec Utils

(s/def ::on-start ::s/any)
(defn register-spec [ns pred]
  (s/def ns pred))

;;;; Registration

(def producer-registry (atom []))
(def consumer-registry (atom []))
(def transformer-registry (atom []))

(defn print-registries []
  (let [reducer #(reduce-kv (fn [acc k v]
                              (assoc acc k (fn-name v)))
                            {}
                            (deref %1))
        [consumer transformers] (map reducer [consumer-registry transformer-registry])
        producers @producer-registry]
    (pprint (kmap consumer transformers producers))))

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

;;;; Start Initializations

(defn stop []
  (reset! producer-registry [])
  (reset! consumer-registry [])
  (reset! transformer-registry []))

;;;; Run

(defn producer-fn [[_ spec-out] & msg]
  (let [value (if-not (vector? spec-out)
                {spec-out (first msg)}
                (zipmap spec-out msg))]
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
                         (s/explain-data spec-in params)))
        spec-form (s/form req-spec-in)]
    (if valid?
      (call-valid-handler values app params)
      (pprint [:input-invalid explain]))))

(defn spec-value-map [values specs]
  (reduce (fn [acc spec] (assoc acc spec (get values spec)))
          {}
          specs))

(defn invoke-app
  [app values handler]
  (let [[spec-in _] (first app)
        spec-desc (s/form spec-in)]
    (cond

      ;; keys spec
      (and (list? spec-desc)
           (= 'cljs.spec/keys (first spec-desc)))
      (let [{:keys [req opt]} (->> (rest spec-desc)
                                   (apply hash-map)
                                   (map #(vector (first %) (set (last %))))
                                   (into {}))
            value-map (spec-value-map values (concat req opt))
            required-keys-map (s/conform (s/keys) (->> (filter #(req (first %)) value-map)
                                                       (into {})))]
        (handler required-keys-map))
      ;; producer with no spec-in
      (not spec-in) (handler true)
      :else (when-let [params (get values spec-in)]
              (handler params)))))

(defn invoke-transformers
  [all-values]
  (doseq [app @transformer-registry]
    (invoke-app app @all-values (partial call-handler all-values app))))

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

(defn run [& [producer]]
  (let [kill-n (atom 0)
        original-values (atom {})]
    (loop [all-values (atom {})]
      (invoke-producer all-values producer)
      (invoke-repeating-producers all-values)
      (invoke-transformers all-values)
      (invoke-consumers all-values)
      #_(when (and (not= @all-values @original-values)
                   (< @kill-n 10))
          (swap! kill-n inc)
          (reset! original-values @all-values)
          (println "RECURRING")
          (recur all-values)))))

(comment

  (do
    (stop)

    ;;; Specs

    (s/def ::number number?)
    (s/def ::string string?)
    (s/def ::map map?)
    (s/def ::keyword keyword?)

    (s/def ::user.id ::number)
    (s/def ::user.first-name ::string)
    (s/def ::user.last-name ::string)
    (s/def ::user.full-name ::string)
    (s/def ::user.salutation ::string)
    (s/def ::user.image ::string)
    (s/def ::user.updated-at ::number)
    (s/def ::user.object (s/keys :req [::user.full-name ::user.id ::user.salutation ::user.updated-at]
                                 :opt [::user.image]))
    (s/def ::user.database (s/coll-of ::user.object []))
    (s/def ::user.database.operation ::keyword)

    (s/def ::user.names (s/keys :req [::user.first-name ::user.last-name]))
    (s/def ::user.all (s/keys :req [::user.full-name ::user.id ::user.salutation]
                              :opt [::user.image]))

    (s/def ::date.today ::string)

    ;;; Constants

    (register-app {:spec-out ::user.first-name
                   :type     :pure
                   :handler  #(identity "Nate")})

    (register-app {:spec-out ::user.last-name
                   :type     :pure
                   :handler  #(identity "Wildermuth")})

    (register-app {:spec-out ::date.today
                   :type     :pure
                   :handler  #(identity "Jul 3, 2016")})

    ;;; Repeaters

    (register-app {:spec-out ::user.id
                   :type     :repeat
                   :handler  #(rand-int 1000)})

    ;;; Pure Transformers


    (register-app {:spec-in  ::user.names
                   :spec-out ::user.full-name
                   ;;:type     :pure
                   :handler  #(do
                               (println "Calculating ::user.full-name\n")
                               (str (::user.first-name %)
                                    " "
                                    (::user.last-name %)))})

    (register-app {:spec-in  ::user.full-name
                   :spec-out ::user.salutation
                   :type     :pure
                   :handler  #(str "Mr. " %)})

    ;;; Transformers


    (register-app {:spec-in  ::user.all
                   :spec-out ::user.object
                   :handler  #(hash-map
                               ::user.salutation (::user.salutation %)
                               ::user.full-name (::user.full-name %)
                               ::user.id (::user.id %)
                               ::user.image (or (::user.image %) "")
                               ::user.updated-at (.getTime (js/Date.))
                               )})

    ;;; Stateful

    (defonce users (atom []))
    (def last-image (atom ""))

    (register-app {:spec-in  ::user.object
                   :spec-out ::user.database
                   :handler  (fn [user]
                               (swap! users conj user)
                               @users)})

    (register-app {:spec-in  ::user.database.operation
                   :spec-out ::user.database
                   :handler  (fn [operation]
                               (condp = operation
                                 :clear (reset! users [])
                                 false)
                               @users)})

    (register-app {:spec-in  (s/nilable ::user.image)
                   :spec-out ::user.image
                   :type     :repeat
                   :handler  (fn [image]
                               (when image
                                 (reset! last-image image))
                               @last-image)})

    ;;; Consumers

    (register-app {:spec-in ::user.first-name
                   :handler #(println "Your first name:" %1 "\n")})

    (register-app {:spec-in ::user.full-name :handler #(println "Your name:" %1 "\n")})

    (register-app {:spec-in ::user.full-name
                   :handler #(println "Seriously, Your name:" %1 "\n")})

    (register-app {:spec-in (s/keys :req [::user.object ::date.today])
                   :handler #(do
                              (println (str "Today is " (::date.today %) "."))
                              (println "Your user object:")
                              (pprint (::user.object %)))})

    (register-app {:spec-in ::user.database
                   :handler #(println "Users saved: " (count %))})

    ;;; Producers

    (def user-first-name (register-app {:spec-out ::user.first-name}))
    (def user-image (register-app {:spec-out ::user.image}))
    (def user-info (register-app {:spec-out (s/keys :req [::user.first-name ::user.image])}))
    (def db-operation (register-app {:spec-out ::user.database.operation}))

    (run)
    true
    )

  (user-image "my-image")
  (user-info "Nathaniel" "another-image")

  (pprint @users)

  (print-registries)
  )

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


