(ns mire.letters)

(def letters (ref {}))

(defn load-letter [letters file]
  (let [letter (read-string (slurp (.getAbsolutePath file)))]
    (conj letters
          {(keyword (.getName file))
           (atom
            {:name (keyword (.getName file))
             :desc (:desc letter)
             :content (:content letter)
             })})))

(defn load-letters
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing letter data."
  [letters dir]
  (dosync
   (reduce load-letter letters
           (.listFiles (java.io.File. dir)))))

(defn add-letters
  "Look through all the files in a dir for files describing letters and add
  them to the mire.letters/letters map."
  [dir]
  (dosync
   (alter letters load-letters dir)))
