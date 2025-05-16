(ns mire.server
  (:gen-class)
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [mire.letters :as letters]
            [mire.items :as items]
            [mire.chests :as chests]))

(defn- cleanup []
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "Name taken, try again: ") (flush) (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (try
    (binding [*in* (io/reader in)
              *out* (io/writer out)
              *err* (io/writer System/err)]
      
      (println "\nWelcome to Mire!")  ; Добавлено приветствие
      (print "What is your name? ") (flush)
      
      (binding [player/*name* (get-unique-player-name (read-line))
                player/*current-room* (ref (or (@rooms/rooms :start)
                                              (do (println "FATAL: No start room!") (System/exit 1))))
                player/*inventory* (ref #{})
                player/*luck* 20
                player/*money* 0
                player/*current-chest* nil]
        
        (dosync
         (commute (:inhabitants @player/*current-room*) conj player/*name*)
         (commute player/streams assoc player/*name* *out*))
        
        (println "\n" (commands/look))  ; Добавлен перенос строки
        (print (player/health-bar)) 
        (println " Money:" player/*money*)
        (print player/prompt) (flush)
        
        (loop [input (read-line)]
          (when input
            (try
              (println (commands/execute input))
              (catch Exception e 
                (println "Error:" (.getMessage e))))
            (print (player/health-bar)) (flush)
            (println " Money:" player/*money*)
            (print player/prompt) (flush)
            (recur (read-line)))))
    
    (catch Exception e
      (println "Connection error:" (.getMessage e)))
    (finally 
      (cleanup))))

(defn -main
  ([port dir]
   (println "Loading game data from" dir)
   (rooms/add-rooms (str dir "rooms"))
   (chests/add-chests (str dir "chests"))
   (letters/add-letters (str dir "letters"))
   (items/add-items (str dir "items"))
   
   (when (empty? @rooms/rooms)
     (println "ERROR: No rooms loaded! Check your rooms files.")
     (System/exit 1))
   
   (println "Starting server on port" port)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Server ready!"))
  
  ([port] (-main port "resources/"))
  ([] (-main 3333)))