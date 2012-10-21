(ns zombies.core
  (:require [lanterna.terminal :as lterm]
            [lanterna.screen :as lscreen])
  (:gen-class))

(def ^:dynamic *maxx* 80)
(def ^:dynamic *maxy* 22)

(defrecord Player [x y hp gold score])
(defrecord Zombie [x y])
;;
;; Constructor functions
(defn make-player
  [x y & {:keys [hp gold score] :or {hp 20 gold 0 score 0}}]
  (Player. x y hp gold score))

(defn make-zombie
  [x y]
  (Zombie. x y))

;;
;; Utility functions
(defn pos-valid?
  [{nx :x ny :y}]
  (and (< -1 nx *maxx*)
       (< -1 ny *maxy*)))

(defn next-pos
  [player dx dy]
  (let [nx (+ dx (:x player))
        ny (+ dy (:y player))]
    {:x (if (< -1 nx *maxx*) nx (:x player))
     :y (if (< -1 ny *maxy*) ny (:y player))}))

(defn update-object-pos
  [player dx dy]
  (let [nx (+ dx (:x player))
        ny (+ dy (:y player))]
    (assoc player
      :x (if (< -1 nx *maxx*) nx 0)
      :y (if (< -1 ny *maxy*) ny 0))))

(defn get-pos-char
  "Get character at position"
  [{screen-cfg :screen} {x :x y :y}]
  (nth (nth screen-cfg y) x))

(defn pos-char?
  "Given the screen configuration, position and the
  query character, check if the character at the position
  is the query one "
  [game-state pos pch]
  (= (get-pos-char game-state pos) pch))

(defn set-pos-char
  "Set the character at given position"
  [game-state {x :x y :y} posv]
  (let [screen-cfg (game-state :screen)
        pos-line (screen-cfg y)]
    (->> (assoc pos-line x posv)
         (assoc screen-cfg y)
         (assoc game-state :screen))))

(defn update-state-pos
  "Set the character from a position to another position"
  [game-state from-pos to-pos]
  (let [posv (get-pos-char game-state from-pos)]
    (-> game-state
        (set-pos-char from-pos \.)
        (set-pos-char to-pos posv))))

(defn pos-free?
  "Is the given position free ?"
  [game-state pos]
  (pos-char? game-state pos \.))
(defn pos-zombie?
  "Is the given position a zombie ?"
  [game-state pos]
  (pos-char? game-state pos \Z))
(defn pos-tree?
  "Is the given position a tree ?"
  [game-state pos]
  (pos-char? game-state pos \#))
(defn pos-teleporter?
  "Is the given position a teleporter ?"
  [game-state pos]
  (pos-char? game-state pos \^))
(defn pos-exit?
  "Is the given position an exit ?"
  [game-state pos]
  (pos-char? game-state pos \>))
(defn pos-player?
  "Is the given position a zombie ?"
  [game-state pos]
  (pos-char? game-state pos \@))
(defn pos-gold?
  "Is the given position a gold ?"
  [game-state pos]
  (pos-char? game-state pos \$))

;; Initialization
(defn init-screen
  "Initialize screen of size *maxx* X *maxy* with ."
  []
  (vec (for [x (range *maxy*)]
         (vec (for [y (range *maxx*)] \.)))))
;; Mark points on the screen
(defn mark-points
  "Mark the given points with point-char
  on the screen-cfg. screen-cfg is a vector (of length *maxy*), each
  one is of vector (of length *maxx*) of chars"
  [screen-cfg points point-char]
  (reduce (fn [cfg z]
            (assoc cfg (:y z)
                   (assoc (cfg (:y z)) (:x z) point-char)))
          screen-cfg
          points))

(defn gen-rand-points
  [n-points]
  (vec (for [i (range (rand-int n-points))]
         {:x (rand-int *maxx*)
          :y (rand-int *maxy*)})))

(defn mark-rand-points
  "Randomly select n-points and mark them with the given point-char
  on the screen-cfg. screen-cfg is a vector (of length *maxy*), each
  one is of vector (of length *maxx*) of chars"
  [screen-cfg n-points point-char]
  (mark-points screen-cfg (gen-rand-points n-points) point-char))

(defn mark-gold [screen-cfg]
  "Randomly select points and mark them for Gold"
  (vec (mark-rand-points screen-cfg 100 \$)))
(defn mark-teleporters [screen-cfg]
  "Randomly select points and mark them for Teleporter"
  (vec (mark-rand-points screen-cfg 10 \^)))
(defn mark-trees [screen-cfg]
  "Randomly select points and mark them for Trees"
  (vec (mark-rand-points screen-cfg 80 \#)))
;; Construct a new level
(defn new-level
  "Construct a new level.  A level is represented by the
   (i) screen configuration (ii) player object
   (iii) zombie objects  "
  [lvl]
  (let [next-lvl (+ 1 lvl)
        zombies (vec (for [i (range (+ 8 (* 2 next-lvl)))]
                       (make-zombie
                        (rand-int *maxx*) (rand-int *maxy*))))
        exit {:x (rand-int *maxx*) :y (rand-int *maxy*)}
        player (make-player (rand-int *maxx*) (rand-int *maxy*))
        screen-cfg (-> (init-screen)
                       (mark-gold)
                       (mark-teleporters)
                       (mark-trees)
                       (mark-points zombies \Z)
                       (mark-points [exit] \>)
                       (mark-points [player] \@))]
    {:screen screen-cfg
     :level next-lvl
     :fmsg (format "Welcome to Level %d. Press ? for help." next-lvl)
     :player player :zombies zombies :exit exit}))

(defn teleport [game-state thing]
  (loop [rpos-x (rand-int *maxx*)
         rpos-y (rand-int *maxy*)]
    (if (pos-free? game-state {:x rpos-x :y rpos-y})
      (assoc thing :x rpos-x :y rpos-y)
      (recur (rand-int *maxx*)
             (rand-int *maxy*)))))

(defn update-zombie-smart [game-state z]
  "Update zombie's position - move towards the person"
  (let [z-x (:x z)
        z-y (:y z)
        p-x (:x (game-state :player))
        p-y (:y (game-state :player))
        new-pos {:x (cond
                     (> z-x p-x) (dec z-x)
                     (< z-x p-x) (inc z-x)
                     :else z-x)
                 :y (cond
                     (> z-y p-y) (dec z-y)
                     (< z-y p-y) (inc z-y)
                     :else z-y)}]
    (cond
     (pos-tree? game-state new-pos) false
     (pos-zombie? game-state new-pos) false
     (pos-gold? game-state new-pos) false
     (pos-exit? game-state new-pos) false
     (pos-teleporter? game-state new-pos) (teleport game-state z)
     :else (assoc z :x (:x new-pos) :y (:y new-pos)))))

(defn update-zombie-random [game-state z]
  "Update zombie's position random;y"
  (let [upd-x (rand-int 3)
        upd-y (rand-int 3)
        new-pos {:x (cond (= upd-x 0) (inc (:x z))
                          (= upd-x 1) (dec (:x z))
                          :else (:x z))
                 :y (cond (= upd-y 0) (inc (:y z))
                          (= upd-y 1) (dec (:y z))
                          :else (:y z))}]
    (cond
     (not (pos-valid? new-pos)) z
     (pos-zombie? game-state new-pos) z
     (pos-tree? game-state new-pos) z
     (pos-gold? game-state new-pos) z
     (pos-exit? game-state new-pos) z
     (pos-teleporter? game-state new-pos) (teleport game-state z)
     :else (assoc z :x (new-pos :x) :y (new-pos :y)))))

(defn update-zombie [game-state z]
  (let [new-zombie (or (and (< 0 (rand-int 7))
                            (update-zombie-smart game-state z))
                       (update-zombie-random game-state z))]
    (if (pos-player? game-state new-zombie)
      (-> (update-in game-state [:player :hp] dec)
          (assoc :fmsg "Ouch!")
          (update-in [:zombies] conj z))
      (-> (update-state-pos game-state z new-zombie)
          (update-in [:zombies] conj new-zombie)))))

(defn update-zombies [game-state]
  (reduce (fn [gstate z]
            (update-zombie gstate z))
          (assoc game-state :zombies [])
          (:zombies game-state)))

(defn update-player [game-state pos]
  "Update the position of the player. Increase gold if I got gold"
  (let [player (game-state :player)
        lvl (game-state :level)
        {new-x :x new-y :y} pos
        new-player (if (and (pos-valid? pos)
                            (pos-teleporter? game-state pos))
                     (teleport game-state player)
                     (assoc player :x new-x :y new-y))]
    (cond
     (not (pos-valid? new-player)) game-state
     (pos-tree? game-state new-player) game-state
     (pos-zombie? game-state new-player) game-state
     (pos-exit? game-state new-player) (new-level lvl)
     :else
     (-> (assoc game-state :player new-player)
         (#(if (pos-gold? %1 new-player)
            (-> (update-in %1 [:player :gold] inc)
                (assoc :fmsg "Got 1 Gold!"))
            %1))
         (update-state-pos player new-player)))))

(defn print-screen
  "Print the level screen based on the configuration
    and waits for any input key"
  [{screen-cfg :screen lvl :level player :player fmsg :fmsg} scr]
  (let [nmsg (count screen-cfg) ]
    (println player)
    (lscreen/put-string
     scr 0 0 (format "Level : %d  HP : %d Gold : %d"
                     lvl (:hp player) (:gold player)))
    (dorun
     (map #(lscreen/put-string scr 0 %2 (apply str %1))
          screen-cfg (range 1 (inc nmsg))))
    (lscreen/put-string scr 0 (inc nmsg) fmsg)
    (lscreen/move-cursor scr (:x player) (inc (:y player)))
    (lscreen/redraw scr)))

(defn print-help
  "Print the help screen and waits for any input key"
  [scr]
  (let [help-msg
        ["Welcome to Zombies!"
         "The world has been overrun by zombies. You have no weapons "
         "or other means of self-defense. All you can do is run for "
         "the exit! That, and collect gold."
         ""
         "Objects :  "
         " @ - You"
         " Z - Zombies!"
         " $ - Gold"
         " # - Trees"
         " ^ - Teleporters"
         " > - Level Exit"
         ""
         "Controls : "
         "y k u"
         "h @ l"
         "b j n"
         ""
         ". - Wait"
         "q - Quit"
         ""
         "Press any key to continue"]
        nmsg (count help-msg)]
    (lscreen/clear scr)
    (dorun
     (map #(lscreen/put-string scr 0 %2 %1)
          help-msg (range 1 (inc nmsg))))
    (lscreen/move-cursor scr 0 (inc nmsg))
    (lscreen/redraw scr)
    (lscreen/get-key-blocking scr)))


(defn zombies-event-loop [game-state scr]
  (lscreen/clear scr)
  (print-screen game-state scr)
  (let [keyv (lscreen/get-key-blocking scr)
        player (game-state :player)
        hits (:hp player)]
    (cond
     (<= hits 0)  (do
                   (lscreen/put-string
                    scr 0 (inc *maxy*)
                    "Game Over. Press any key to quit")
                   (lscreen/redraw scr)
                   (lscreen/get-key-blocking scr))
     (= keyv \?) (do
                   (print-help scr)
                   (recur game-state scr))
     (= keyv \q) (do
                   (lscreen/put-string scr 0 (inc *maxy*) "Goodbye.")
                   0)
     (= keyv \j) (recur (->> (next-pos player 0 +1)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \k) (recur (->> (next-pos player 0 -1)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \l) (recur (->> (next-pos player +1 0)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \h) (recur (->> (next-pos player -1 0)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \y) (recur (->> (next-pos player -1 +1)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \u) (recur (->> (next-pos player +1 +1)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \b) (recur (->> (next-pos player -1 -1)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     (= keyv \n) (recur (->> (next-pos player +1 -1)
                             (update-player game-state)
                             (update-zombies))
                        scr)
     :else (recur game-state scr))))

(defn start-zombies []
  (let [scr (lscreen/get-screen :swing)]
    (lscreen/in-screen
     scr
     (zombies-event-loop (new-level 0) scr))))

(defn -main
  [& args]
  (start-zombies))

(start-zombies)
