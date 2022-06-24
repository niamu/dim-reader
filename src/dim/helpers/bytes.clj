(ns dim.helpers.bytes
  (:require
   [clojure.java.io :as io])
  (:import
   [java.io ByteArrayOutputStream]
   [java.nio ByteBuffer ByteOrder]
   [java.util Arrays]))

(defn file->bytes
  [f]
  (with-open [xin (io/input-stream f)
              xout (ByteArrayOutputStream.)]
    (io/copy xin xout)
    (byte-array (mapv bit-not (.toByteArray xout)))))

(defn selection
  [^bytes bs start end]
  (Arrays/copyOfRange bs start end))

(defn uint8
  [[b1 b2]]
  (bit-and 0xFF
           (bit-or (bit-shift-left b2 8)
                   b1)))

(defn uint16
  [[b1 b2]]
  (bit-and 0xFFFF
           (bit-or (bit-shift-left b2 8)
                   b1)))

(defn uint16-at
  [^bytes bs at]
  (uint16 (selection bs at (+ at 2))))

(defn ->uint8-buffer
  [^bytes bs]
  (let [int-buffer (-> (ByteBuffer/wrap bs)
                       (.order ByteOrder/LITTLE_ENDIAN)
                       (.asIntBuffer))]
    (for [idx (range 0 (.limit int-buffer))]
      (.get int-buffer idx))))

(defn ->uint16-buffer
  [^bytes bs]
  (let [char-buffer (-> (ByteBuffer/wrap bs)
                        (.order ByteOrder/LITTLE_ENDIAN)
                        (.asCharBuffer))]
    (reduce (fn [accl b]
              (conj accl (int b)))
            []
            char-buffer)))

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
