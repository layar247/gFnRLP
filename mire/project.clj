(defproject mire "0.13.1"
  :description "A multiuser text adventure game/learning project."
  :main mire.server
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.jline/jline "3.19.0"]
                 [server-socket "1.0.0"]]
  :uberjar-name "mire_game.jar"
  :profiles {:uberjar {:aot :all}})
