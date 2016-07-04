(ns microapps.sample
  (:require [microapps.apps :refer [run reset-registries register-app
                                    print-registries]]
            [microapps.utils :refer [timer]]
            [cljs.spec :as s]
            [cljs.pprint :refer [pprint]])
  (:require-macros [microapps.macros :refer [kmap]]))

(defn sample []

  (reset-registries)

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

  (def users (atom []))
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

  (register-app {:spec-out ::user.image
                 :type     :repeat
                 :handler  #(identity @last-image)})

  (register-app {:spec-in ::user.image
                 :handler (fn [image]
                            (when image
                              (reset! last-image image)))})

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

  (let [db-operation (register-app {:spec-out ::user.database.operation})
        user-info (register-app {:spec-out (s/keys :req [::user.first-name ::user.image])})
        user-image (register-app {:spec-out ::user.image})
        user-first-name (register-app {:spec-out ::user.first-name})
        ops (kmap user-first-name user-image user-info db-operation)]
    (timer run)
    ops))

(def ops (sample))

(defn op [k & params]
  (let [f (get ops k)]
    (apply timer f params)))

(comment

  (op :db-operation :clear)
  (op :user-first-name "Nathan")
  (op :user-image "one-image")
  (op :user-info {::user.first-name "Nathaniel"
                  ::user.image "two-image"})



  )