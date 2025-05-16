(ns mire.chests)

(def chests (ref {}))

(defn load-chest [chests file]
  (let [chest (read-string (slurp (.getAbsolutePath file)))]
    (conj chests
          {(keyword (.getName file))
           (atom 
           {:name (keyword (.getName file))
            :desc (:desc chest)
            :is_closed (or (:is_closed chest) false)
            :items (ref (or (:items chest) #{})) 
            })})))

(defn load-chests
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing chest data."
  [chests dir]
  (dosync
   (reduce load-chest chests
           (.listFiles (java.io.File. dir)))))

(defn add-chests
  "Look through all the files in a dir for files describing chests and add
  them to the mire.chests/chests map."
  [dir]
  (dosync
   (alter chests load-chests dir)))

(defn chest-contains?
  [chest thing]
  (@(:items chest) (keyword thing)))

(defn chest-closed?
  [chest]
  (@(:is_closed chest)))
