(ns dim.content.sprite
  (:require
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
     (dec (Math/pow 2 from-bits))))

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
             bs))))

(defn sprites
  [header sprite-dimensions ^bytes sprite-data-section]
  (let [text (String. (helper.bytes/selection sprite-data-section 0x0 0x08))
        final-offset (-> (helper.bytes/selection sprite-data-section 0x18 0x1C)
                         helper.bytes/->uint16-buffer)
        sprites-count (-> (helper.bytes/selection sprite-data-section 0x48 0x4C)
                          helper.bytes/->uint8-buffer
                          first)
        sprite-offsets (-> (helper.bytes/selection sprite-data-section
                                                   0x4C (+ 0x4C
                                                           (* sprites-count 4)))
                           helper.bytes/->uint8-buffer)]
    (loop [idx 0
           sprites []]
      (if (< idx sprites-count)
        (let [width (nth sprite-dimensions (* idx 2))
              height (nth sprite-dimensions (inc (* idx 2)))
              actual-size (* width height 2)
              expected-size (- (nth sprite-offsets (inc idx) 0)
                               (nth sprite-offsets idx 0))
              offset (nth sprite-offsets idx
                          (+ 0x4C
                             (* sprites-count 4)
                             0x04))
              data (helper.bytes/selection sprite-data-section
                                           offset (+ offset actual-size))]
          (when (and (not= expected-size actual-size)
                     (not= idx (dec sprites-count)))
            (throw (Exception.
                    (str "Expected sprite size "
                         expected-size
                         " doesn't match actual sprite size "
                         actual-size))))
          (recur (inc idx)
                 (conj sprites
                       (bgra {:sprite/id {:dim/id (:dim/id header)
                                          :sprite/id idx}
                              :sprite/width width
                              :sprite/height height
                              :sprite/data data}))))
        sprites))))
