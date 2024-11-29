(defproject clojure-object-model "0.1.0-SNAPSHOT"
  :description "A CLOS-like object model implemented in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :plugins [[lein-codox "0.10.7"]  ; Documentation generation
            [lein-cloverage "1.2.2"]  ; Code coverage
            [lein-kibit "0.1.8"]]  ; Static code analysis
  :codox {:output-path "target/doc"
          :source-paths ["src"]
          :doc-files ["README.md"]}
  :main ^:skip-aot clojure-object-model.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "1.3.0"]]}})