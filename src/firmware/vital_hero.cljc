(ns firmware.vital-hero
  (:refer-clojure :exclude [read])
  (:require
   [dim.content.sprite :as sprite]
   [util.bytes :as bytes]
   [gp-spifi.core :as gp-spifi])
  #?(:clj
     (:import
      [java.io File])))

(defn read
  #?(:clj [^File file]
     :cljs [file])
  (let [bs (bytes/file->bytes file)
        gp-spifi (gp-spifi/parse-at bs 0x00200000)
        sprites-bs (->> (:gp-spifi/groups gp-spifi)
                        (filter (fn [{:keys [group/type]}]
                                  (= type :image)))
                        (map :group/files)
                        (apply concat))
        sprite-dimensions (->> (bytes/selection bs
                                                0x1E16
                                                (+ 0x1E16
                                                   (* (count sprites-bs)
                                                      4)))
                               bytes/->uint16-buffer
                               (partition 2 2))
        sprites (map-indexed (fn [idx sprite-bs]
                               (let [[width height] (nth sprite-dimensions idx)]
                                 (bytes/sprite->image-data
                                  {:sprite/id (keyword
                                               "firmware.vital-hero"
                                               (str "sprite" idx))
                                   :sprite/width width
                                   :sprite/height height
                                   :sprite/data sprite-bs})))
                             sprites-bs)]
    {:firmware/sprites sprites}))
