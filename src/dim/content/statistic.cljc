(ns dim.content.statistic)

(def attributes
  [:virus
   :data
   :vaccine
   :free])

(def dispositions
  [:stoic
   :active
   :normal
   :indoor
   :lazy])

(def small-attacks
  ["SMALL_FIRE",
   "SMALL_ICE",
   "SMALL_LIGHTNING",
   "SMALL_WATER",
   "MEDIUM_FIRE",
   "MEDIUM_ICE",
   "MEDIUM_LIGHTNING",
   "YELLOW_SHOCK_WAVES",
   "BLUE_SHOCK_WAVES",
   "HEARTS",
   "YELLOW_BLAST",
   "PURPLE_BLAST",
   "AIR_CUT",
   "MISSILE",
   "AIR_ARROW",
   "PUNCH",
   "FIRE_PUNCH",
   "ICE_PUNCH",
   "YELLOW_PUNCH",
   "PURPLE_PUNCH",
   "CLAW",
   "EARTH_SPIKES",
   "ICE_SPIKES",
   "BULLETS",
   "BLUE_CUTS",
   "GREEN_CUTS",
   "PURPLE_WAVE",
   "POOP",
   "BONE",
   "SKULL",
   "BEAK",
   "GREEN_WAVES",
   "BOMB",
   "AIR_SOMETHING?",
   "MOLTEN_ROCK",
   "ROCK",
   "BUBBLES",
   "BATS?",
   "MUSIC",
   "BIG_FIREBALL"])

(def big-attacks
  ["BIG_FIRE_BLAST",
   "BIG_WATER_BLAST",
   "BIG_HOLY_BLAST",
   "BIG_DARK_BLAST",
   "BIG_LIGHT_ARROW",
   "BIG_DARK_SPIRIT",
   "BIG_ROCK_DARTS",
   "BIG_ICE_DARTS",
   "BIG_AIR_CUTS",
   "MANY_MISSILES",
   "BLUE_TORNADO",
   "PURPLE_TORNADO",
   "GREEN_TORNADO",
   "BIG_GREEN_LIGHTNING",
   "BIG_HOLY_FIST",
   "BIG_POOP",
   "BIG_DARK_ENERGY",
   "BIG_LIGHT_ENERGY",
   "MANY_GREEN_DARTS",
   "BIG_BOMB",
   "BIG_BLUE_ORB",
   "BIG_PINK_BLAST"])

(defn statistic
  [_ row]
  (letfn [(read-byte [idx]
            (if (= 0xFFFF (aget row idx))
              nil
              (aget row idx)))]
    {:digimon/stage (read-byte 0)
     :digimon/unlock-required? (not (nil? (read-byte 1)))
     :digimon/attribute (get-in attributes [(dec (read-byte 2))] :free)
     :digimon/disposition (get-in dispositions [(read-byte 3)])
     :digimon/small-attack (get-in small-attacks [(read-byte 4)])
     :digimon/big-attack (get-in big-attacks [(read-byte 5)])
     :digimon/dp-stars (read-byte 6)
     :digimon/dp (read-byte 7)
     :digimon/hp (read-byte 8)
     :digimon/ap (read-byte 9)
     :digimon/battle-chance1 (read-byte 10)
     :digimon/battle-chance2 (read-byte 11)}))
