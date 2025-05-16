(ns mire.rooms)

(def rooms (ref {}))

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :is_closed (ref (or (:is_closed room) false))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items (ref (or (:items room) #{}))
            :letters (ref (or (:letters room) #{})) 
            :chests (ref (or (:chests room) #{})) 
            :inhabitants (ref #{})}})))

(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms dir]
  (dosync
   (reduce load-room rooms
           (.listFiles (java.io.File. dir)))))

(defn add-rooms
  "Look through all the files in a dir for files describing rooms and add
  them to the mire.rooms/rooms map."
  [dir]
  (dosync
   (alter rooms load-rooms dir)))

(defn room-cont-chest?
  [room chest]
  (@(:chests room) (keyword chest)))

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing)))

(defn room-closed?
  [room]
  (@(:is_closed room)))
