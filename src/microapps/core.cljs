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

;;; Initialization

(declare producer-fn run)

;;; NodeJS Inits

(nodejs/enable-util-print!)
(def -main (fn [] (println (str "development? " development?))))
(set! *main-cli-fn* -main)

;;; JS Interop

(defn js-print [s]
  (println s))

(defn js-make-keyword [s-ns s-key]
  (keyword s-ns s-key))

(defn js-make-spec [s pred]
  (let [k (keyword s)]
    (s/def k pred)))

;;; Spec Utils

(defn register-spec [ns pred]
  (s/def ns pred))

;;; Registration

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

(defn get-spec [app]
  (let [{:keys [spec-in spec-out]} app]
    (cond
      (and spec-in spec-out) [spec-in spec-out]
      spec-in spec-in
      spec-out spec-out
      :else (throw (js/Error. "App must have either a spec-in or spec-out")))))

(defn register-app [app]
  (let [{:keys [spec-in spec-out handler type]} app]
    (cond
      (and (= type :pure) spec-in spec-out) (add-transformer [spec-in spec-out] (memoize handler))
      (and (= type :pure) spec-out) (add-constant-producer [nil spec-out] (memoize handler) :pure)
      (and (= type :repeat) spec-out) (add-constant-producer [nil spec-out] handler :repeat)
      (and spec-in spec-out) (add-transformer [spec-in spec-out] handler)
      spec-in (add-consumer [spec-in nil] handler)
      spec-out (add-producer [nil spec-out])
      :else (throw (js/Error. "App must have either a spec-in or spec-out")))))

;;; Start Initializations

(defn stop []
  (reset! producer-registry [])
  (reset! consumer-registry [])
  (reset! transformer-registry []))

;;; Run

(defn producer-fn [[_ spec-out] msg]
  (run {spec-out msg}))

(defn call-handler [values app params]
  (let [[[_ spec-out] handler] app
        result (handler params)
        valid? (s/valid? spec-out result)]
    (if valid?
      (swap! values assoc spec-out result)
      (throw (ex-info "Output data is invalid."
                      (s/explain-data spec-out result))))))

(defn invoke-transformers
  [all-values]
  (let [invoke (partial call-handler all-values)]
    (doseq [app @transformer-registry]
      (let [[spec-in _] (first app)
            values @all-values]
        (cond
          (vector? spec-in)
          (let [spec-in-vals (reduce (fn [acc spec]
                                       (assoc acc spec (get values spec)))
                                     {}
                                     spec-in)]
            (invoke app spec-in-vals))
          :else
          (let [params (get values spec-in)]
            (invoke app params)))))))

(defn invoke-consumers
  [all-values]
  (doseq [app @consumer-registry]
    (let [[spec-in _] (first app)
          values @all-values
          handler (last app)]
      (cond
        (vector? spec-in)
        (let [spec-in-vals (reduce (fn [acc spec]
                                     (assoc acc spec (get values spec)))
                                   {}
                                   spec-in)]
          (handler spec-in-vals))
        :else
        (let [params (get values spec-in)]
          (handler params))))))

(defn run [& [producer-message]]
  (let [all-values (atom {})
        invoke (partial call-handler all-values)]
    (doseq [app (filter #(or (:pure (meta %))
                             (:repeat (meta %))) @producer-registry)]
      (invoke app true))
    (swap! all-values merge producer-message)
    (invoke-transformers all-values)
    (invoke-consumers all-values)))

(comment

  (do
    (stop)

    ;; Specs

    (s/def ::user.id number?)
    (s/def ::user.first-name string?)
    (s/def ::user.last-name string?)
    (s/def ::user.full-name string?)
    (s/def ::user.salutation string?)
    (s/def ::user.image string?)
    (s/def ::user.object map?)
    (s/def ::date.today string?)

    ;; Constants

    (register-app {:spec-out ::user.first-name
                   :type     :pure
                   :handler  #(identity "Nate")})

    (register-app {:spec-out ::user.last-name
                   :type     :pure
                   :handler  #(identity "Wildermuth")})

    (register-app {:spec-in  ::on-start
                   :spec-out ::date.today
                   :type     :pure
                   :handler  #(identity "Jul 3, 2016")})

    ;; Repeaters

    (register-app {:spec-out ::user.id
                   :type     :repeat
                   :handler  #(rand-int 1000)})

    ;; Pure Transformers

    (register-app {:spec-in  [::user.first-name ::user.last-name]
                   :spec-out ::user.full-name
                   :type     :pure
                   :handler  #(do
                               (println "Calculating ::user.full-name\n")
                               (str (::user.first-name %)
                                    " "
                                    (::user.last-name %)))})

    (register-app {:spec-in  ::user.full-name
                   :spec-out ::user.salutation
                   :type     :pure
                   :handler  #(str "Mr. " %)})

    ;; Transformers

    (register-app {:spec-in  [::user.full-name ::user.id ::user.image ::user.salutation]
                   :spec-out ::user.object
                   :handler  #(hash-map
                               :salutation (::user.salutation %)
                               :full-name (::user.full-name %)
                               :id (::user.id %)
                               :image (::user.image %)
                               :updated-at (.getTime (js/Date.)))})

    ;; Stateful

    (defonce users (atom []))

    (s/def ::user.database (s/* ::user.object))
    (register-app {:spec-in  ::user.object
                   :spec-out ::user.database
                   :handler  (fn [user]
                               (swap! users conj user)
                               @users)})

    (s/def ::user.database.operation keyword?)
    (register-app {:spec-in  ::user.database.operation
                   :spec-out ::user.database
                   :handler  (fn [operation]
                               (condp = operation
                                 :clear (reset! users [])
                                 false)
                               @users)})

    ;; Consumers

    (register-app {:spec-in ::user.first-name
                   :handler #(println "Your first name:" %1 "\n")})

    (register-app {:spec-in ::user.full-name
                   :handler #(println "Your name:" %1 "\n")})

    (register-app {:spec-in ::user.full-name
                   :handler #(println "Seriously, Your name:" %1 "\n")})

    (register-app {:spec-in [::user.object ::date.today]
                   :handler #(do
                              (println (str "Today is " (::date.today %) "."))
                              (println "Your user object:")
                              (pprint (::user.object %)))})

    (register-app {:spec-in ::user.database
                   :handler #(println "Users saved: " (count %))})

    ;; Producers

    (def user-first-name (register-app {:spec-out ::user.first-name}))
    (def user-image (register-app {:spec-out ::user.image}))
    (def user-info (register-app {:spec-out [::user.first-name ::user.image]}))
    (def db-operation (register-app {:spec-out ::user.database.operation}))

    (run)
    true
    )


  (print-registries)
  )

;;; JS Exports

(obj/set js/module "exports" #js {:print       js-print
                                  :makeKeyword js-make-keyword
                                  :makeSpec    js-make-spec
                                  :isValid     s/valid?
                                  :explain     s/explain-data})

