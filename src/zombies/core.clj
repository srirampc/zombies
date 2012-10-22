(ns zombies.core
  (:require [lanterna.terminal :as lterm]
            [lanterna.screen :as lscreen])
  (:gen-class))

(def _maxx_ 80)
(def _maxy_ 22)
(def _tree_ \#)
(def _gold_ \$)
(def _zombie_ \Z)
(def _player_ \@)
(def _empty_ \.)
(def _teleporter_ \^)
(def _exit_ \>)

(defrecord Player
    [x y hp gold score])
(defrecord Zombie
    [x y])
(defrecord GameState
    [screen level zombies player nhits exit fmsg])

;;
;; Constructor functions
(defn make-player
  [x y & {:keys [hp gold score] :or {hp 20 gold 0 score 0}}]
  (Player. x y hp gold score))

(defn make-zombie
  [x y]
  (Zombie. x y))

(defn make-game-state
  [& {:keys [screen level zombies player nhits exit fmsg]
      :or {screen [] level 0 zombies []
           player nil nhits 0 exit {}
           fmsg ""}}]
  (GameState. screen level zombies player nhits exit fmsg))

;;
;; Utility functions
(defn is-valid?
  [{nx :x ny :y}]
  (and (< -1 nx _maxx_)
       (< -1 ny _maxy_)))

(defn next-pos
  [player dx dy]
  (let [nx (+ dx (:x player))
        ny (+ dy (:y player))]
    {:x (if (< -1 nx _maxx_) nx (:x player))
     :y (if (< -1 ny _maxy_) ny (:y player))}))

(defn update-object-pos
  [player dx dy]
  (let [nx (+ dx (:x player))
        ny (+ dy (:y player))]
    (assoc player
      :x (if (< -1 nx _maxx_) nx 0)
      :y (if (< -1 ny _maxy_) ny 0))))

(defn get-char-at
  "Get character at position"
  [{screen-cfg :screen} {x :x y :y}]
  (nth (nth screen-cfg y) x))

(defn char-at?
  "Given the screen configuration, position and the
  query character, return true if the character at the position
  is the queried one "
  [game-state pos pch]
  (= (get-char-at game-state pos) pch))

(defn set-char-at
  "Set the character posv at given position; and return the
   updated game state"
  [game-state {x :x y :y} pch]
  (let [screen-cfg (:screen game-state)
        y-line (screen-cfg y)]
    (->> (assoc y-line x pch)
         (assoc screen-cfg y)
         (assoc game-state :screen))))

(defn is-free?
  "Is the given position free ?"
  [game-state pos]
  (char-at? game-state pos _empty_))
(defn is-zombie?
  "Is the given position a zombie ?"
  [game-state pos]
  (char-at? game-state pos _zombie_))
(defn is-tree?
  "Is the given position a tree ?"
  [game-state pos]
  (char-at? game-state pos _tree_))
(defn is-teleporter?
  "Is the given position a teleporter ?"
  [game-state pos]
  (char-at? game-state pos _teleporter_))
(defn is-exit?
  "Is the given position an exit ?"
  [game-state pos]
  (char-at? game-state pos _exit_))
(defn is-player?
  "Is the given position a zombie ?"
  [game-state pos]
  (char-at? game-state pos _player_))
(defn is-gold?
  "Is the given position a gold ?"
  [game-state pos]
  (char-at? game-state pos _gold_))

(defn move-to
  "Move the 'character' from one position to another and
   return the updated game state"
  [game-state from-pos to-pos]
  (let [posv (get-char-at game-state from-pos)]
    (-> game-state
        (set-char-at from-pos _empty_)
        (set-char-at to-pos posv))))

(defn teleport [game-state thing]
  "Teleport the 'thing' to a random location."
  (loop [new-pos {:x (rand-int _maxx_)
                  :y (rand-int _maxy_)}]
    (if (is-free? game-state new-pos)
      (merge thing new-pos)
      (recur {:x (rand-int _maxx_)
              :y (rand-int _maxy_)}))))

;; Initialization
(defn init-screen
  "Initialize screen of size _maxx_ X _maxy_ with ."
  []
  (vec (for [x (range _maxy_)]
         (vec (for [y (range _maxx_)] _empty_)))))
;; Mark points on the screen
(defn mark-points
  "Mark the given points with point-char
  on the screen-cfg. screen-cfg is a vector (of length _maxy_), each
  one is of vector (of length _maxx_) of chars"
  [screen-cfg points point-char]
  (reduce (fn [cfg z]
            (assoc cfg (:y z)
                   (assoc (cfg (:y z)) (:x z) point-char)))
          screen-cfg
          points))

(defn gen-rand-points
  [n-points]
  (vec (for [i (range (rand-int n-points))]
         {:x (rand-int _maxx_)
          :y (rand-int _maxy_)})))

(defn mark-rand-points
  "Randomly select n-points and mark them with the given point-char
  on the screen-cfg. screen-cfg is a vector (of length _maxy_), each
  one is of vector (of length _maxx_) of chars"
  [screen-cfg n-points point-char]
  (mark-points screen-cfg (gen-rand-points n-points) point-char))

(defn mark-gold [screen-cfg]
  "Randomly select points and mark them for Gold"
  (vec (mark-rand-points screen-cfg 100 _gold_)))
(defn mark-teleporters [screen-cfg]
  "Randomly select points and mark them for Teleporter"
  (vec (mark-rand-points screen-cfg 10 _teleporter_)))
(defn mark-trees [screen-cfg]
  "Randomly select points and mark them for Trees"
  (vec (mark-rand-points screen-cfg 80 _tree_)))

;; Construct a new level
(defn make-screen-cfg
  "Make screen configuration based on players,zombies and exit position.
   Others are palced randomly"
  [player zombies exit]
  (-> (init-screen)
      (mark-gold)
      (mark-teleporters)
      (mark-trees)
      (mark-points zombies _zombie_)
      (mark-points [exit] _exit_)
      (mark-points [player] _player_)))

(defn new-level
  "Construct a new level.  A level is represented by the
   (i) screen configuration (ii) player object
   (iii) zombie objects  "
  [current-level player]
  (let [next-lvl (+ 1 current-level)
        zombies (vec (for [i (range (+ 8 (* 2 next-lvl)))]
                            (make-zombie
                             (rand-int _maxx_) (rand-int _maxy_))))
        exit {:x (rand-int _maxx_) :y (rand-int _maxy_)}
        new-player (merge player
                          {:x (rand-int _maxx_) :y (rand-int _maxy_)})]
    (merge (make-game-state)
           {:fmsg (format "Welcome to Level %d. Press ? for help."
                          next-lvl)
            :level next-lvl
            :screen (make-screen-cfg new-player zombies exit)
            :player new-player
            :zombies zombies
            :exit exit})))

;; Move zombies and update their location
(defn update-zombie-smart [game-state z]
  "Update zombie's position moving towards the player"
  (let [{z-x :x z-y :y} z
        {p-x :x p-y :y} (:player game-state)
        new-pos {:x (cond
                     (> z-x p-x) (dec z-x)
                     (< z-x p-x) (inc z-x)
                     :else z-x)
                 :y (cond
                     (> z-y p-y) (dec z-y)
                     (< z-y p-y) (inc z-y)
                     :else z-y)}]
    (cond
     (not (is-valid? new-pos)) false
     (is-teleporter? game-state new-pos) (teleport game-state z)
     (or (is-player? game-state new-pos)
         (is-free? game-state new-pos)) (merge z new-pos)
     :else false)))

(defn update-zombie-random [game-state z]
  "Update zombie's position to one of near-by positions randomly"
  (let [new-pos {:x (+ (:x z) (rand-nth [-1 0 1]))
                 :y (+ (:y z) (rand-nth [-1 0 1]))}]
    (cond
     (not (is-valid? new-pos)) z
     (is-teleporter? game-state new-pos) (teleport game-state z)
     (is-free? game-state new-pos) (merge z new-pos)
     :else z)))

(defn update-zombie [game-state z]
  "Update zombie's position either randomly or towards the player
   based on a die roll"
  (let [new-zombie (or (and (< 0 (rand-int 7))
                            (update-zombie-smart game-state z))
                       (update-zombie-random game-state z))]
    (if (is-player? game-state new-zombie)
      (-> (update-in game-state [:nhits] inc)
          (assoc :fmsg "Ouch!")
          (update-in [:zombies] conj z))
      (-> (move-to game-state z new-zombie)
          (update-in [:zombies] conj new-zombie)))))

(defn update-hits [game-state]
  "Update player hits. Hits decrease by one, even if
   more than one zombie has attacked the player."
  (if (> (:nhits game-state) 0)
    (update-in game-state [:player :hp] dec)
    game-state))

(defn update-zombies [game-state]
  "Update position of all the zombies"
  (update-hits
   (reduce #(update-zombie %1 %2)
           (assoc game-state :zombies [] :nhits 0)
           (:zombies game-state))))

;; Move Player and his/her location
(defn update-gold [game-state]
  "Update Gold if the player hits gold"
  (if (is-gold? game-state (:player game-state))
    (-> game-state
        (update-in [:player :gold] inc)
        (assoc :fmsg "Got 1 Gold!"))
    game-state))

(defn update-player [game-state pos]
  "Update the position of the player. Increase gold if the player
   hits gold"
  (if (not (is-valid? pos))
    game-state
    (let [player (:player game-state)
          new-player (if (is-teleporter? game-state pos)
                       (teleport game-state player)
                       (merge player pos))]
      (cond
       (is-tree? game-state new-player) game-state
       (is-zombie? game-state new-player) game-state
       (is-exit? game-state new-player) (new-level (:level game-state)
                                                   player)
       :else (-> game-state
                 (assoc :player new-player)
                 (update-gold)
                 (move-to player new-player))))))

;; Update player and zombie status
(defn update-game-state [game-state player [dx dy]]
  "Player has move dx in x direction, and dy in y direction
   Update the game state now"
  (->> (next-pos player dx dy)
       (update-player game-state)
       (update-zombies)))

;; Print functions
(defn print-screen
  "Print the level screen based on the configuration
    and waits for any input key"
  [{screen-cfg :screen lvl :level player :player fmsg :fmsg} scr]
  (let [nmsg (count screen-cfg) ]
    (lscreen/put-string
     scr 0 0 (format "Level : %d  HP : %d Gold : %d"
                     lvl (:hp player) (:gold player)))
    (dotimes [i nmsg]
      (lscreen/put-string scr 0 (inc i) (apply str (screen-cfg i))))
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

;; Main event loop
(def _keydir_ {\j [0 1] \k [0 -1] \l [+1 0] \h [-1 0]
               \y [-1 -1] \u [1 -1] \b [-1 1] \n [1 1] })

(defn zombies-event-loop [game-state scr]
  "Main dispatch loop for the key press"
  (lscreen/clear scr)
  (print-screen game-state scr)
  (let [keyv (lscreen/get-key-blocking scr)
        player (:player game-state)
        lvl (:level game-state)
        hits (:hp player)
        score (+ (* (:gold player) lvl) (* 10 (- lvl 1)))]
    (cond
     (<= hits 0)     (do (lscreen/put-string
                          scr 0 (inc _maxy_)
                          (format
                           "Game Over. Score %d. Press any key to quit."
                           score ))
                         (lscreen/redraw scr)
                         (lscreen/get-key-blocking scr))
     (= keyv \?)     (do (print-help scr)
                         (recur game-state scr))
     (= keyv \q)     (do (lscreen/put-string
                          scr 0 (inc _maxy_)
                          (format
                           "Goodbye. Score %d. Press any key to quit."
                           score))
                         (lscreen/redraw scr)
                         (lscreen/get-key-blocking scr))
     (_keydir_ keyv) (recur (update-game-state
                             game-state player (_keydir_ keyv))
                            scr)
     :else           (recur game-state scr))))

(defn start-zombies []
  (let [scr (lscreen/get-screen :swing)]
    (lscreen/in-screen
     scr
     (zombies-event-loop (new-level 0 (make-player 0 0)) scr))))

(defn -main
  [& args]
  (start-zombies))
