(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {
    :foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created
for a game or something."
           :title "in the foyer. There is a stair to go up."
           :dir {:south :Peabody
                 }
           :contents #{:raw-egg}}
   :Peabody {:desc "Out of the ARC."
              :title "You are on Peabody."
              :dir {:south :hall}
              :contents #{}}
    :hall {:desc "You have entered the Activity and Recreation Center. Two strong men beat you and said you have to find their secret in Protein-shake. Otherwise you cannot get out."
               :title "in the ARC."
               :dir {:north :check-in}
               :contents #{:I-card}}
     :check-in {:desc "You have to swipe your I-card to enter."
                :title "in the check-in."
                :dir {:north :treadmill
                      :south :hall
                      :downstair  :dining}
                :contents #{:basketball}}
      :treadmill {:desc "You wanna run and exercise."
                 :title "on the treadmill."
                 :dir {:west :volleyball-court
                       :south :check-in
                       :east  :badminton}
                 :contents #{}}
       :volleyball-court {:desc "You wanna play volleyball with girls."
                  :title "Playing volleyball"
                  :dir {:east  :treadmill}
                  :contents #{:volleyball}}
        :badminton {:desc "You wanna play badminton with boys."
                   :title "Playing badminton."
                   :dir {:west  :treadmill}
                   :contents #{:racket}}
         :dining {:desc "You wanna have meal in the dining hall."
                    :title "Having meal with friends."
                    :dir {:north  :climbing
                          :upstair  :check-in
                          :east :changing-room
                          :south :basketball-court
                          :downstair  :gym}
                    :contents #{:vendingmachine}}
          :climbing {:desc "You wanna challenge yourself."
                     :title "Climbing wall with friends."
                     :dir {:south  :dining}
                     :contents #{:gloves}}
           :changing-room {:desc "You wanna put on the sportswear."
                      :title "Changing cloth."
                      :dir {:west  :dining}
                      :contents #{:sportswear}}
            :basketball-court {:desc "You wanna play basketball."
                       :title "Playing basketball."
                       :dir {:north  :dining}
                       :contents #{:protein-shake :superstrong}}
             :gym {:desc "You wanna work out with guys."
                        :title "You wanna be strong and big."
                        :dir {:upstair  :dining}
                        :contents #{:dumbbell}}
          :vendingmachine  {:desc "You buy gatorade and snickers."}
          :basketball  {:desc "You borrow a basketball."}
          :volleyball  {:desc "You borrow a volleyball."}
          :sportswear  {:desc "You wear the sportswear."}
          :dumbbell   {:desc "You pick up the dumbbell."}
          :gloves   {:desc "You pick up the gloves for climbing wall."}
          :racket   {:desc "You borrow the badminton rackets."}
          :protein-shake  {:desc "It is protein shake that boost your muscles."
                    :contents #{:key}}


   })

(def adventurer
  {:location :foyer
   :inventory #{}
   :strength #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn eat [thing player]
  (let [things (player :inventory)]
    (if (things thing)
      (do (println "You take one snickers.") player)
      (do (println "Stay hungry, stay foolish.")
          player))))

(defn drink [contents player]
  (let [location (player :location)
          things (->> the-map location :contents)]
          (if (things :protein-shake)
            (if (player :inventory :snickers)
              (update-in player [:inventory] #(conj % :superstrong))
              (do (println "You have to add the snickers buff.")
                  player))
            (if (empty? things)
              (do (println "No protein no gains.")
                  player)
              (do (println things)
                  player)))))

(defn showstrength [thing player]
  (let [things (player :strength)]
    (if (empty? things)
        (do (println "You are weak.")
            player)
      (do (println things)
          player))))

(defn show [thing player]
  (let [things (player :inventory)]
    (if (empty? things)
        (do (println "Awkward.")
            player)
      (do (println things)
          player))))

(defn buy [thing player]
  (let [location (player :location)
        mything (player :inventory)
        things (->> the-map location :contents)]
    (if (mything :I-card)
      (if (things :vendingmachine)
        (update-in player [:inventory] #(conj % :snickers))
        (do (println "I cannot see a vendingmachine.")
            player))
      (do (println "Greed is good.") player))))

(defn swipe [thing player]
    (let [location (player :location)
          mything (player :inventory)
          things (->> the-map location :contents)]
      (if (mything :I-card)
        (if (things :basketball)
          (update-in player [:inventory] #(conj % :basketball))
          (do (println "This is not for check out.")
              player))
        (do (println "You don't have I-card") player))))

(defn fight [inventory player]
  (let [where (player :location)
      things (player :inventory)]
      (if (where :contents :hall)
      (if (things :superstrong)
        (do (println "You have beaten the bastards who bullied you.") player)
        (do (println "You were beaten again.")
            player))
      (do (println "It hurts.") player))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn exit [inventory player]
  (let [location (player :location)
        things (player :inventory)]
    (if (things :superstrong)
      (do (println "You exit ARC and it is the end.") player)
      (do (println "You cannot exit right now."))
    )))

  (defn pick [thing player]
    (let [location (player :location)
          things (->> the-map location :contents)]
      (if (things thing)
        (update-in player [:inventory] #(conj % thing))
        (do (println "You cannot pick from here.")
  player))))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:west] (go :west player)
         [:east] (go :east player)
         [:upstair] (go :upstair player)
         [:downstair] (go :downstair player)
         [:whatigot](show :inventory player)
         [:strength](showstrength :strength player)
         [:vendingmachine] (pick :vendingmachine player)
         [:basketball] (pick :basketball player)
         [:volleyball] (pick :volleyball player)
         [:sportswear] (pick :sportswear player)
         [:dumbbell] (pick :dumbbell player)
         [:gloves] (pick :gloves player)
         [:I-card] (pick :I-card player)
         [:racket] (pick :racket player)
         [:protein-shake] (pick :protein-shake player)
         [:swipe] (swipe :I-card player)
         [:eat] (eat :snickers player)
         [:buy] (buy :I-card player)
         [:drink] (drink :contents player)
         [:fight] (fight :superstrong player)
         [:exit] (exit :superstrong player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main
 "I don't do a whole lot ... yet."
 [& args]
 (loop [local-map the-map
      local-player adventurer]
     (let [pl (status local-player)
         _  (println "What do you want to do?")
         command (read-line)]
     (if (= command "exit")
       (println "You survived")
       (recur local-map (respond pl (to-keywords command)))))))
