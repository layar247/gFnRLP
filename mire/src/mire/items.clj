(ns mire.items)
(def items (ref {}))

(defn load-item [items file]
  (let [item (read-string (slurp (.getAbsolutePath file)))]
    (conj items
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc item)
            :price (:price item)
            }})))


(defn load-items
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing item data."
  [items dir]
  (dosync
   (reduce load-item items
           (.listFiles (java.io.File. dir)))))

(defn add-items
  "Look through all the files in a dir for files describing items and add
  them to the mire.items/items map."
  [dir]
  (dosync
   (alter items load-items dir)))