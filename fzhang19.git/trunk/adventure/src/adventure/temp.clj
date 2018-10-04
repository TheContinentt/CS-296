(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created
for a game or something."
           :title "in the foyer."
           :dir {:south :grue-pen}
           :contents #{:raw-egg}}
   :grue-pen {:desc "It is very dark.  You are about to be eaten by a grue."
              :title "in the grue pen."
              :dir {:north :foyer
                    :south :hall}
              :contents #{}}
    :hall {:desc "You have entered the Activity and Recreation Center."
               :title "in the ARC."
               :dir {:north :check-in}
               :contents #{}}
     :check-in {:desc "You have to swipe your I-card to enter."
                :title "in the check-in."
                :dir {:north :treadmill
                      :south :hall
                      :downstair  :dining}
                :contents #{basketball}}
      :treadmill {:desc "You wanna run and exercise."
                 :title "on the treadmill."
                 :dir {:west :volleyball-court
                       :south :check-in
                       :east  :badminton}
                 :contents #{}}
       :volleyball-court {:desc "You wanna play volleyball with girls."
                  :title "Playing volleyball"
                  :dir {:east  :treadmill}
                  :contents #{volleyball}}
        :badminton {:desc "You wanna play badminton with boys."
                   :title "Playing badminton."
                   :dir {:west  :treadmill}
                   :contents #{racket}}
         :dining {:desc "You wanna have meal in the dining hall."
                    :title "Having meal with friends."
                    :dir {:north  :climbing
                          :upstair  :check-in
                          :east :changing-room
                          :south :basketball-court
                          :downstair  :gym}
                    :contents #{vending-machine}}
          :climbing {:desc "You wanna challenge yourself."
                     :title "Climbing wall with friends."
                     :dir {:south  :dining}
                     :contents #{gloves}}
           :changing-room {:desc "You wanna put on the sportswear."
                      :title "Changing cloth."
                      :dir {:west  :dining}
                      :contents #{sportswear}}
            :basketball-court {:desc "You wanna play basketball."
                       :title "Playing basketball."
                       :dir {:north  :dining}
                       :contents #{drink}}
             :gym {:desc "You wanna work out with guys."
                        :title "You wanna be strong and big."
                        :dir {:upstair  :dining}
                        :contents #{dumbbell}}
          :vending-machine  {:desc "You buy gatorade and snickers."}
          :basketball  {:desc "You borrow a basketball."}
          :volleyball  {:desc "You borrow a volleyball."}
          :sportswear  {:desc "You wear the sportswear."}
          :dumbbell   {:desc "You pick up the dumbbell."}
          :gloves   {:desc "You pick up the gloves for climbing wall."}
          :racket   {:desc "You borrow the badminton rackets."}
          :protein-shake  {:desc "It is protein shake that boost your muscles."
                    :title "an egg"}


   })

(def adventurer
  {:location :foyer
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

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

  (defn pick [thing player]
    (let [location (player :location)
          things (->> the-map location :contents)]
      (if (things thing)
        (update-in player [:inventory] #(conj % thing))
        (do (println "it's not there. You cannot pick it up")
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
         [:vending-machine] (pick :vending-machine player)

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
            (println "That's the end")
      (recur local-map (respond pl (to-keywords command))))))
