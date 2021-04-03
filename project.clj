(defproject liczebniki "1.0"
  :description "Declension tables for Polish numerals"
  :url ""
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot liczebniki.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
