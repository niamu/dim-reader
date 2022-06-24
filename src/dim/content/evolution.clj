(ns dim.content.evolution)

(defn evolution
  [{:keys [dim/id]} row]
  (when (not= (nth row 3) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (nth row 3)
            3))
  (when (not= (nth row 5) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (nth row 5)
            5))
  (when (not= (nth row 7) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (nth row 7)
            7))
  (when (not= (nth row 9) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (nth row 9)
            9))
  (when (not= (nth row 11) 0xFFFF)
    (format "Unexpected value %d at index %d in evolution table for DIM."
            (nth row 11)
            11))
  (let [read-byte (fn [idx]
                    (if (= (nth row idx) 0xFFFF)
                      nil
                      (nth row idx)))
        to (read-byte 10)]
    (when to
      {:evolution/from {:dim/id id
                        :digimon/id (read-byte 0)}
       :evolution/to {:dim/id id
                      :digimon/id to}
       :evolution/requirements {:requirement/hours (read-byte 1)
                                :requirement/vital-value (read-byte 2)
                                :requirement/trophies (read-byte 4)
                                :requirement/battles (read-byte 6)
                                :requirement/win-ratio (read-byte 8)}})))
