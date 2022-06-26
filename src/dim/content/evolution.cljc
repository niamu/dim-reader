(ns dim.content.evolution
  #?(:cljs (:require
            [goog.string :refer [format]])))

(defn evolution
  [{:keys [dim/id]} row]
  (when (not= (aget row 3) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (aget row 3)
            3))
  (when (not= (aget row 5) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (aget row 5)
            5))
  (when (not= (aget row 7) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (aget row 7)
            7))
  (when (not= (aget row 9) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (aget row 9)
            9))
  (when (not= (aget row 11) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (aget row 11)
            11))
  (let [read-byte (fn [idx]
                    (if (= (aget row idx) 0xFFFF)
                      nil
                      (aget row idx)))
        to (read-byte 10)]
    (when to
      {:evolution/from (keyword
                        (str "dim" id)
                        (str "digimon" (read-byte 0)))
       :evolution/to (keyword
                      (str "dim" id)
                      (str "digimon" to))
       :evolution/requirements {:requirement/hours (read-byte 1)
                                :requirement/vital-value (read-byte 2)
                                :requirement/trophies (read-byte 4)
                                :requirement/battles (read-byte 6)
                                :requirement/win-ratio (read-byte 8)}})))
