(ns dim.content.sprite
  (:require
   [gp-spifi.core :as gp-spifi]
   [util.bytes :as bytes]))

(def label-by-sprite-count
  {6 [:name
      :idle-1
      :idle-2
      :walk
      :happy
      :sleep]
   7 [:name
      :idle-1
      :idle-2
      :walk
      :happy
      :sleep
      :full]
   14 [:name
       :idle-1
       :idle-2
       :walk-1
       :walk-2
       :run-1
       :run-2
       :workout-1
       :workout-2
       :happy
       :sleep
       :attack
       :run-3
       :full]})

(defn sprites
  [header ^bytes bs]
  (let [gp-spifi (gp-spifi/parse-at bs 0x100000)
        sprites-bs (->> (:gp-spifi/groups gp-spifi)
                        (filter (fn [{:keys [group/type]}]
                                  (= type :image)))
                        (map :group/files)
                        (apply concat))
        sprite-dimensions (->> (bytes/selection bs
                                                0x60000
                                                (+ 0x60000
                                                   (* (count sprites-bs)
                                                      4)))
                               bytes/->uint16-buffer
                               (partition 2 2))]
    (map-indexed (fn [idx sprite-bs]
                   (let [[width height] (nth sprite-dimensions idx)]
                     (bytes/sprite->image-data
                      {:sprite/id (keyword
                                   (str "dim" (:dim/id header))
                                   (str "sprite" idx))
                       :sprite/width width
                       :sprite/height height
                       :sprite/data sprite-bs})))
                 sprites-bs)))
