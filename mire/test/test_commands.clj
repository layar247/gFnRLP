(ns test-commands
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [mire.commands :refer :all]
            [mire.player :as player]
            [mire.rooms :as rooms]))

(rooms/add-rooms "resources/rooms/")

(defmacro def-command-test [name & body]
  `(deftest ~name
     (binding [player/*current-room* (ref (:start @rooms/rooms))
               player/*inventory* (ref #{})
               player/*name* "Tester"
               player/*luck* 20
               player/*money* 0
               player/*current-chest* nil]
       ~@body)))

(def-command-test test-execute
  ;; Silence the error!
  (binding [*err* (io/writer "/dev/null")]
    (is (= "You can't do that!"
           (execute "discard a can of beans into the fridge")))) 
  (is (re-find #"closet" (execute "north")))
  (is (= @player/*current-room* (:closet @rooms/rooms))))

(def-command-test test-move
  (is (re-find #"hallway" (execute "south")))
  (is (re-find #"promenade" (move "south")))
  (is (re-find #"can't" (move "east"))))

(def-command-test test-look
  (binding [player/*current-room* (ref (:closet @rooms/rooms))]
    (doseq [look-for [#"closet" #"keys" #"south"]]
    (is (re-find look-for (look))))))

(def-command-test test-inventory
  (binding [player/*inventory* (ref [:keys :bunny])]
    (is (re-find #"bunny" (inventory)))
    (is (re-find #"keys" (inventory)))))

(def-command-test test-grab
  (binding [player/*current-room* (ref (:closet @rooms/rooms))]
    (is (not (= "There isn't any keys here"
                (grab "keys"))))
    (is (player/carrying? :keys))
    (is (empty? @(@player/*current-room* :items)))))

(def-command-test test-discard
  (binding [player/*inventory* (ref #{:bunny})]
    (is (re-find #"dropped" (discard "bunny")))
    (is (not (player/carrying? "bunny")))
    (is (rooms/room-contains? @player/*current-room* "bunny"))))
