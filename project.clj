(defproject microapps "0.1.0-SNAPSHOT"
  :description "A FP approach to microservices"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.36"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [figwheel-sidecar "0.5.0"]]

  :cljsbuild {:builds {:dev
                       {:source-paths ["src" "dev"]
                        :compiler {:main user.core
                                   :target :nodejs
                                   :optimizations :none
                                   :output-to "target/index.js"
                                   :output-dir "target"
                                   :source-map true
                                   :parallel-build true
                                   :cache-analysis true
                                   :closure-defines {"microapps.config.DEVELOPMENT" true}}}
                       :prod
                       {:source-paths ["src"]
                        :compiler {:main microapps.core
                                   :output-to "lib/index.js"
                                   :target :nodejs
                                   :hashbang false
                                   :optimizations :simple
                                   :closure-defines {"microapps.config.DEVELOPMENT" false}}}}}

  :plugins [[lein-cljsbuild "1.1.3"]]

  :clean-targets ^{:protect false} ["target"
                                    "lib"])