(ns mire.server
  (:gen-class)
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [clojure.core.async :as async]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(defn- log [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn- cleanup [name room]
  (when (and name room)
    (dosync
     (commute (:inhabitants @room) disj name)
    (commute player/streams dissoc name)))

(defn- handle-commands [in out name]
  (binding [*in* (io/reader in)
            *out* (io/writer out)]
    (loop []
      (when-let [input (.readLine *in*))]
        (try
          (let [response (commands/execute input)]
            (.write *out* (str response "\n"))
            (.flush *out*))
          (catch Exception e
            (log "Command error:" e)))
        (recur))))

(defn- mire-handle-client [in out]
  (future
    (try
      (let [name (loop []
                   (.write *out* "What is your name? ")
                   (.flush *out*)
                   (let [name (.readLine *in*))]
                     (if (empty? name)
                       (recur)
                       name))]
        (binding [player/*name* name
                  player/*current-room* (ref (@rooms/rooms :start))]
          (handle-commands in out name))
      (finally
        (cleanup player/*name* @player/*current-room*)))))

(defn -main [& args]
  (rooms/add-rooms "resources/rooms")
  (when-not (@rooms/rooms :start)
    (log "ERROR: No :start room defined")
    (System/exit 1))
  
  (socket/create-server 3333 mire-handle-client)
  (log "Server running on port 3333"))