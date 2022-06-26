(ns dim.helpers.bytes
  #?(:clj
     (:require
      [clojure.java.io :as io]))
  #?(:clj
     (:import
      [java.io ByteArrayOutputStream]
      [java.nio ByteBuffer ByteOrder]
      [java.util Arrays])))

(defn file->bytes
  [f]
  #?(:clj (with-open [xin (io/input-stream f)
                      xout (ByteArrayOutputStream.)]
            (io/copy xin xout)
            (byte-array (mapv bit-not (.toByteArray xout))))
     :cljs (let [bv (new js/DataView f)]
             (doseq [idx (range (.-byteLength bv))]
               (.setUint8 bv idx (bit-not (.getUint8 bv idx))))
             (new js/Uint8Array (.-buffer bv)))))

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
