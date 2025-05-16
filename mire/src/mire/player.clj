(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *luck*)
(def ^:dynamic *money*)
(def ^:dynamic *current-chest*)
(def max-luck 100)


(defn health-bar []
  (let [health-per-block (/ max-luck 10)
        blocks (int (/ *luck* health-per-block))]
    (apply str (concat ["LUCK: ["] (repeat blocks "█") (repeat (- 10 blocks) "░") ["]"]))))


(def prompt "> ")
(def streams (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))
