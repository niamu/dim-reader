(ns dim.content.sprite
  (:require
   [clojure.math :as math]
   [dim.helpers.bytes :as helper.bytes]))

(def label-by-sprite-count
  {6 [:name
      :idle
      :idle2
      :walk
      :happy
      :sleep]
   7 [:name
      :idle
      :idle2
      :walk
      :happy
      :sleep
      :full]
   14 [:name
       :idle
       :idle2
       :walk
       :walk2
       :run
       :run2
       :workout
       :workout2
       :happy
       :sleep
       :attack
       :run3
       :full]})

(defn scale-to-24bit
  [color from-bits]
  (/ (* color 0xFF)
     (dec (math/pow 2 from-bits))))

#?(:clj
   (defn bgra
     [{:sprite/keys [width height ^bytes data] :as sprite}]
     (assoc sprite
            :sprite/data
            (loop [pixel 0
                   ^bytes bs (byte-array (* width height 4))]
              (if (< pixel (* width height))
                (let [b0 (bit-and 0xFF (aget data (inc (* pixel 2))))
                      b1 (bit-and 0xFF (aget data (* pixel 2)))
                      red (scale-to-24bit (bit-shift-right (bit-and b0 2r11111000)
                                                           3)
                                          5)
                      green (scale-to-24bit
                             (bit-or (bit-shift-left (bit-and b0 2r00000111) 3)
                                     (bit-shift-right (bit-and b1 2r11100000) 5))
                             6)
                      blue (scale-to-24bit (bit-and b1 2r00011111)
                                           5)
                      alpha? (and (zero? red)
                                  (zero? blue)
                                  (= green 0xFF))
                      alpha (if alpha? 0 0xFF)]
                  (aset-byte bs (+ (* pixel 4) 0) (unchecked-byte blue))
                  (aset-byte bs (+ (* pixel 4) 1) (unchecked-byte (if alpha?
                                                                    0
                                                                    green)))
                  (aset-byte bs (+ (* pixel 4) 2) (unchecked-byte red))
                  (aset-byte bs (+ (* pixel 4) 3) (unchecked-byte alpha))
                  (recur (inc pixel)
                         bs))
                bs)))))

#?(:cljs
   (defn image-data
     [{:sprite/keys [width height data] :as sprite}]
     (let [uint8clamped
           (loop [pixel 0
                  bs (new js/Uint8ClampedArray (* width height 4))]
             (if (< pixel (* width height))
               (let [b0 (bit-and 0xFF (aget data (inc (* pixel 2))))
                     b1 (bit-and 0xFF (aget data (* pixel 2)))
                     red (scale-to-24bit (bit-shift-right (bit-and b0 2r11111000)
                                                          3)
                                         5)
                     green (scale-to-24bit
                            (bit-or (bit-shift-left (bit-and b0 2r00000111) 3)
                                    (bit-shift-right (bit-and b1 2r11100000) 5))
                            6)
                     blue (scale-to-24bit (bit-and b1 2r00011111)
                                          5)
                     alpha? (and (zero? red)
                                 (zero? blue)
                                 (= green 0xFF))
                     alpha (if alpha? 0 0xFF)]
                 (aset bs (+ (* pixel 4) 0) (unchecked-byte red))
                 (aset bs (+ (* pixel 4) 1) (unchecked-byte (if alpha?
                                                              0
                                                              green)))
                 (aset bs (+ (* pixel 4) 2) (unchecked-byte blue))
                 (aset bs (+ (* pixel 4) 3) (unchecked-byte alpha))
                 (recur (inc pixel)
                        bs))
               bs))]
       (assoc sprite
              :sprite/data
              (new js/ImageData uint8clamped width height)))))

(defn sprites
  [header ^bytes sprite-dimensions ^bytes sprite-data-section]
  (let [text (-> (helper.bytes/selection sprite-data-section 0x0 0x08)
                 helper.bytes/bytes->string)
        sprite-dimensions (helper.bytes/->uint16-buffer sprite-dimensions)
        final-offset (-> (helper.bytes/selection sprite-data-section 0x18 0x1C)
                         helper.bytes/->uint16-buffer)
        sprites-count (-> (helper.bytes/selection sprite-data-section 0x48 0x4C)
                          helper.bytes/->uint32-buffer
                          first)
        sprite-offsets (-> (helper.bytes/selection sprite-data-section
                                                   0x4C (+ 0x4C
                                                           (* sprites-count 4)))
                           helper.bytes/->uint32-buffer)]
    (loop [idx 0
           sprites []]
      (if (< idx sprites-count)
        (let [width (aget sprite-dimensions (* idx 2))
              height (aget sprite-dimensions (inc (* idx 2)))
              actual-size (* width height 2)
              expected-size (- (aget sprite-offsets
                                     (min (inc idx)
                                          (dec (#?(:clj count
                                                   :cljs .-length)
                                                sprite-offsets))))
                               (aget sprite-offsets idx))
              offset (or (aget sprite-offsets idx)
                         (+ 0x4C
                            (* sprites-count 4)
                            0x04))
              data (helper.bytes/selection sprite-data-section
                                           offset
                                           (+ offset actual-size))]
          (when (and (not= expected-size actual-size)
                     (not= idx (dec sprites-count)))
            (throw (#?(:clj Exception.
                       :cljs js/Error.)
                    (str "Expected sprite size "
                         expected-size
                         " doesn't match actual sprite size "
                         actual-size))))
          (recur (inc idx)
                 (conj sprites
                       (-> {:sprite/id (keyword
                                        (str "dim" (:dim/id header))
                                        (str "sprite" idx))
                            :sprite/width width
                            :sprite/height height
                            :sprite/data data}
                           #?(:clj bgra
                              :cljs image-data)))))
        sprites))))
