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
  "Drop all inventory and remove player from room and player list."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (println "Новый клиент подключился!") ;; сюда
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]
    (print "\nWhat is your name? ") (flush)
    (println "Ожидаем имя...") ;; сюда

    (let [name (read-line)]
      (println "Получено имя:" name) ;; сюда
      (binding [player/*name* (get-unique-player-name name)
                player/*current-room* (ref (@rooms/rooms :start))
                player/*inventory* (ref #{})
                player/*luck* 20
                player/*money* 0
                player/*current-chest* nil]
        (println "Инициализация игрока...") ;; сюда
        (println "Ключи в карте комнат:" (keys @rooms/rooms))
        (println "Комната: " @player/*current-room*)
        (dosync
         (commute (:inhabitants @player/*current-room*) conj player/*name*)
         (commute player/streams assoc player/*name* *out*))
        
        (println "Игрок добавлен в комнату.")
        (println (commands/look))
        (print (player/health-bar))
        (println (str " Money: " player/*money*))
        (print player/prompt) (flush)

        ;; основной игровой цикл
        (try
          (loop [input (read-line)]
            (when input
              (println (commands/execute input))
              (.flush *err*)
              (print (player/health-bar)) (flush)
              (println (str " Money: " player/*money*)) (flush)
              (print player/prompt) (flush)
              (recur (read-line))))
          (finally
            (println "Клиент отключился. Очистка...")
            (cleanup)))))))

(defn -main
  ([port dir]
     (rooms/add-rooms (str (io/file dir "rooms")))
     (chests/add-chests (str dir "chests"))
     (letters/add-letters (str dir "letters"))
     (items/add-items (str dir "items"))
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/"))
  ([] (-main 3333)))
