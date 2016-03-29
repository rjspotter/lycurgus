(defproject lycurgus "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/core.contracts "0.0.6-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot lycurgus.core
  :target-path "target/%s"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :profiles {:uberjar {:aot :all}})
