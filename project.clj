(defproject εδιτ "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [net.avh4.math/geometry-clj "0.0.3-SNAPSHOT"]
                 [lamina "0.5.0"]]
  :source-paths   ["src/main/clojure"]
  :resource-paths ["src/main/resources"]
  :test-paths     ["src/test/clojure"]
  :profiles {
    :dev {
      :source-paths ["src/dev/clojure"]
      :plugins [[lein-midje "3.0.0"]]
      :dependencies [[midje "1.5.0"]] }} )
