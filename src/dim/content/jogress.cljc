(ns dim.content.jogress)

(defn jogress
  [{:keys [dim/id]} row]
  (letfn [(read-byte [idx]
            (when (not= 0xFFFF (aget row idx))
              (aget row idx)))]
    (->> [{:jogress/from (keyword
                          (str "dim" id)
                          (str "digimon" (read-byte 0)))
           :jogress/to (keyword
                        (str "dim" id)
                        (str "digimon" (read-byte 1)))
           :jogress/requirements [{:requirement/attribute :vaccine}]}
          {:jogress/from (keyword
                          (str "dim" id)
                          (str "digimon" (read-byte 0)))
           :jogress/to (keyword
                        (str "dim" id)
                        (str "digimon" (read-byte 2)))
           :jogress/requirements [{:requirement/attribute :data}]}
          {:jogress/from (keyword
                          (str "dim" id)
                          (str "digimon" (read-byte 0)))
           :jogress/to (keyword
                        (str "dim" id)
                        (str "digimon" (read-byte 3)))
           :jogress/requirements [{:requirement/attribute :virus}]}
          {:jogress/from (keyword
                          (str "dim" id)
                          (str "digimon" (read-byte 0)))
           :jogress/to (keyword
                        (str "dim" id)
                        (str "digimon" (read-byte 4)))
           :jogress/requirements [{:requirement/attribute :free}]}]
         (remove (fn [{:keys [jogress/to]}]
                   (= to (keyword
                          (str "dim" id)
                          (str "digimon" nil))))))))

(defn specific-jogress
  [{:keys [dim/id]} row]
  {:jogress/from (keyword
                  (str "dim" id)
                  (str "digimon" (aget row 0)))
   :jogress/to (keyword
                (str "dim" id)
                (str "digimon" (aget row 1)))
   :jogress/requirements
   [{:requirement/digimon (keyword
                           (str "dim" (aget row 2))
                           (str "digimon" (aget row 3)))}]})
