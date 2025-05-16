(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.chests :as chests]
            [mire.letters :as letters]
            [mire.items :as items]
            [mire.player :as player]))

(def shop_items #{:helmet :luck-potion :pickaxe})

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- remove-from-ref
  "Remove one instance of obj from ref. Must call in a transaction."
  [obj from]
  (alter from disj obj))



(defn- item-info [item]
  (if-let [item-details (get @items/items item)]
    (str (:name item-details) " (Price: " (:price item-details) ")")
    item))

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (if-let [current-chest-name player/*current-chest*]
    (let [chest (@chests/chests current-chest-name)] 
      (str "You are looking inside the " (:name @chest) "\n"
           (str/join "\n" (map #(str "There is " % " here.\n")
                               @(:items @chest)))))
    (str (:desc @player/*current-room*)
       "\nExits: " (keys @(:exits @player/*current-room*)) "\n"
       (str/join "\n" (map #(str "You can read " % " here.\n")
                           @(:letters @player/*current-room*)))  
       (str/join "\n" (map #(str "There is " % " here.\n")
                           @(:items @player/*current-room*))) 
       (str/join "\n" (map #(str "There is " % " here.\n")
                           @(:chests @player/*current-room*)))
       ))
  (if (= (:name @player/*current-room*) :shop)
    (str "\nYou can buy:\n"
         (str/join "\n" (map item-info shop_items)))
    ""))
  )


(defn- teleport
  [target]
  (move-between-refs player/*name*
                     (:inhabitants @player/*current-room*)
                     (:inhabitants target))
  (ref-set player/*current-room* target)
  )

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target 
         (if (and (= target-name :swamp) (< player/*luck* 80))
           (do
             (println "You're unlucky today...an ogre just killed you")
             (set! player/*luck* (max 0 (- player/*luck* 20)))
             (teleport (@rooms/rooms :start))
             "")
           (if (= target-name :end)
             (if (@player/*inventory* :golden_key)
               (do
                 (teleport target) 
                 (look))
               "You can't come here yet!")
             (do
               (teleport target)
               (if (= target-name :swamp)
                 (println "It seems the ogre is sleeping! You can go to the casino today")
                 "")
               (look))))
         "You can't go that way."))))


(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (not (nil? player/*current-chest*))
     (let [chest (@chests/chests player/*current-chest*)]
       (if (chests/chest-contains? @chest thing)
         (do (move-between-refs (keyword thing)
                                (:items @chest)
                                player/*inventory*)
             (str "You picked up the " thing "."))
         (str "There isn't any " thing " here.")))
     (if (rooms/room-contains? @player/*current-room* thing)
       (do (move-between-refs (keyword thing)
                              (:items @player/*current-room*)
                              player/*inventory*)
           (str "You picked up the " thing "."))
       (str "There isn't any " thing " here.")))))



(defn- chest_items
  "See what's in the chest"
  [chest]
  (str "Chest contains:\n"
       (str/join "\n" (seq @(:items chest)))))

(defn- open_chest [chest]
  (swap! chest #(assoc % :is_closed false))
  (set! player/*current-chest* (:name @chest))
  ""
  )

(defn back
  "Close the current chest."
  []
  (set! player/*current-chest* nil)
  (look)
  )

(defn open
  "Open chest"
  [chest-name]
  (dosync
   (if (not ((:chests @player/*current-room*) (keyword chest-name)))
     (str "There is no chest with that name in this room.")  
     (let [chest (@chests/chests (keyword chest-name))]
       (if (:is_closed @chest)
         (if (@player/*inventory* :keys) 
           (do 
             (println "You use a keys to open the chest.")
             (println (chest_items @chest)) 
             (open_chest chest)
             (remove-from-ref :keys
                              player/*inventory*)
             ""
             ) 
           (str "The chest is closed, and you don't have any keys to open it.")) 
         (do 
           (set! player/*current-chest* (:name @chest)) 
           (print (look))
           ""
           )
         )
       )
     )
   )
  )

;; (defn print_chests []
;;   (
;;     (println "List of all chests:")
;;     (doseq [name @chests/chests]
;;       (println "- " name))))

(defn- print_chests
  "Chests"
  []
  (str (str/join "\n" (map #(str "There is " (first %) " here.\n") @letters/letters))))

(defn read
  "Read letter"
  [letter-name]
  (dosync
   (if (not ((:letters @player/*current-room*) (keyword letter-name)))
     (str "There is no letter with that name in this room.")
     (str (:content @(@letters/letters (keyword letter-name))) "\n")
     ))
)

(defn- cheat_luck
  []
  (set! player/*luck* player/max-luck)
  )
(defn- cheat_money
  []
  (set! player/*money* (+ player/*money* 1000)))

(defn use
  "Use item"
  [thing]
  (dosync
   (if (player/carrying? thing) 
     (cond 
       (= (keyword thing) :luck-potion)
       (if (>= player/*luck* player/max-luck)
         (str "You are already at full health.")
         (do
           (println "You use luck-potion.")
           (set! player/*luck* (min player/max-luck (+ player/*luck* 50)))
           (remove-from-ref (keyword thing)
                              player/*inventory*)
           ""
           )
         )
       (= (keyword thing) :another-item)
       (str "You use another item.")) 
     (str "You don't have that item."))))

(defn dig
  "Dig ore"
  []
  (dosync
   (if (player/carrying? "pickaxe")
     (if (= (:name @player/*current-room*) :cave)
       (let [ore-count (+ 1 (rand-int 5)) ore-minus (+ 1 (rand-int 5))]
         (set! player/*money* (+ player/*money* ore-count))
         (println "You dig" ore-count "gold!")
         (if (and (= ore-minus 5) (not (player/carrying? "helmet")))
           (do
             (set! player/*money* (max 0 (- player/*money* 20)))
             (println "A rock fell on you and you lost gold") 
             )
           ""
           )
         ""
         )
       "You're not in the cave"
       )
     (str "You don't have pickaxe"))))

(defn buy
  "Buy items"
  [thing]
  (dosync
   (if (= (:name @player/*current-room*) :shop)
     (if (contains? shop_items (keyword thing))
       (let [item (get @items/items (keyword thing))]
         (if (< player/*money* (:price item))
           "Not enough money"
           (if (player/carrying? thing)
             "You already have this item"
             (do 
               (alter player/*inventory* conj (keyword thing))
               (set! player/*money* (- player/*money* (:price item)))
               (str "You bought the " thing ".")
               ))))
       "This item is not available in the store"
       )
     "This action is only available in the shop"))
  )


(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (if (not (nil? player/*current-chest*))
       (do (move-between-refs (keyword thing)
                              player/*inventory*
                              (:items @player/*current-chest*))
           (str "You dropped the " thing "."))
       (do (move-between-refs (keyword thing)
                              player/*inventory*
                              (:items @player/*current-room*))
           (str "You dropped the " thing "."))
       ) 
     (str "You're not carrying a " thing "."))))


(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (map item-info @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println (str player/*name* " said: " message))
        (println player/prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))
;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "dig" dig
               "open" open
               "chests" print_chests
               "cheat_luck" cheat_luck
               "cheat_money" cheat_money
               "use" use
               "buy" buy
               "say" say
               "read" read
               "back" back
               "help" help})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
