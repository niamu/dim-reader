(ns util.bytes
  (:require
   [clojure.math :as math]
   [util.config :refer [config]]
   #?(:clj [clojure.java.io :as io]))
  #?(:clj
     (:import
      [java.io ByteArrayOutputStream]
      [java.nio ByteBuffer ByteOrder]
      [java.util Arrays])))

(defn file->bytes
  ([f]
   (file->bytes f false))
  ([f bit-not?]
   #?(:clj (with-open [xin (io/input-stream f)
                       xout (ByteArrayOutputStream.)]
             (io/copy xin xout)
             (byte-array (mapv (if bit-not?
                                 bit-not
                                 identity)
                               (.toByteArray xout))))
      :cljs (let [bv (new js/DataView f)]
              (doseq [idx (range (.-byteLength bv))]
                (.setUint8 bv idx (bit-not (.getUint8 bv idx))))
              (new js/Uint8Array (.-buffer bv))))))

(defn selection
  [^bytes bs start end]
  (#?(:clj Arrays/copyOfRange
      :cljs .slice) bs start end))

(defn bytes->string
  [^bytes bs]
  #?(:clj (String. bs)
     :cljs (let [text-decoder (new js/TextDecoder)]
             (.decode text-decoder bs))))

(defn ->uint16-buffer
  [^bytes bs]
  #?(:clj (let [unsigned-values (char-array (/ (count bs) 2))
                char-buffer (-> (ByteBuffer/wrap bs)
                                (.order ByteOrder/LITTLE_ENDIAN)
                                (.asCharBuffer)
                                (.get unsigned-values))
                values (int-array (alength unsigned-values))]
            (doseq [i (range (alength unsigned-values))]
              (aset-int values i (aget unsigned-values i)))
            values)
     :cljs (new js/Uint16Array (.-buffer bs))))

(defn uint16-at
  [^bytes bs at]
  (-> (selection bs at (+ at 2))
      ->uint16-buffer
      (aget 0)))

(defn ->uint32-buffer
  [^bytes bs]
  #?(:clj (let [values (int-array (/ (count bs) 4))
                int-buffer (-> (ByteBuffer/wrap bs)
                               (.order ByteOrder/LITTLE_ENDIAN)
                               (.asIntBuffer)
                               (.get values))]
            values)
     :cljs (new js/Uint32Array (.-buffer bs))))

(defn uint32-at
  [^bytes bs at]
  (-> (selection bs at (+ at 4))
      ->uint32-buffer
      (aget 0)))

(defn ->table
  [^bytes bs row-fn columns]
  (loop [idx 0
         accl []]
    (let [row (->> (selection bs idx (+ idx (* columns 2)))
                   ->uint16-buffer)]
      (if (and (some (complement zero?) row)
               (some (fn [x]
                       (and (not (zero? x))
                            (not= x 0xFFFF)))
                     row))
        (recur (+ idx (* columns 2))
               (conj accl (row-fn row)))
        accl))))

(defn- scale-to-24bit
  [color from-bits]
  (quot (* color 0xFF)
        (dec (int (math/pow 2 from-bits)))))

(defn sprite->image-data
  [{:sprite/keys [width height data] :as sprite}]
  (assoc sprite
         :sprite/data
         (loop [pixel 0
                bs #?(:clj (byte-array (* width height 4))
                      :cljs (new js/Uint8ClampedArray (* width height 4)))]
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
                               (= green 0xFF)
                               ;; Only allow alpha if not fullscreen
                               (not (and (= width (:screen/width config))
                                         (= height (:screen/height config)))))
                   alpha (if alpha? 0 0xFF)]
               (#?(:clj aset-byte
                   :cljs aset) bs (+ (* pixel 4) 2) (unchecked-byte blue))
               (#?(:clj aset-byte
                   :cljs aset) bs (+ (* pixel 4) 1) (unchecked-byte (if alpha?
                                                                      0
                                                                      green)))
               (#?(:clj aset-byte
                   :cljs aset) bs (+ (* pixel 4) 0) (unchecked-byte red))
               (#?(:clj aset-byte
                   :cljs aset) bs (+ (* pixel 4) 3) (unchecked-byte alpha))
               (recur (inc pixel)
                      bs))
             #?(:clj bs
                :cljs (new js/ImageData bs width height))))))
