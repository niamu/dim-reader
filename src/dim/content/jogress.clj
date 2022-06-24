(ns dim.content.jogress)

(defn jogress
  [{:keys [dim/id]} row]
  (letfn [(read-byte [idx]
            (when (not= 0xFFFF (nth row idx))
              (nth row idx)))]
    (->> [{:jogress/from {:dim/id id
                          :digimon/id (read-byte 0)}
           :jogress/to {:dim/id id
                        :digimon/id (read-byte 1)}
           :jogress/requirements [{:requirement/attribute :vaccine}]}
          {:jogress/from {:dim/id id
                          :digimon/id (read-byte 0)}
           :jogress/to {:dim/id id
                        :digimon/id (read-byte 2)}
           :jogress/requirements [{:requirement/attribute :data}]}
          {:jogress/from {:dim/id id
                          :digimon/id (read-byte 0)}
           :jogress/to {:dim/id id
                        :digimon/id (read-byte 3)}
           :jogress/requirements [{:requirement/attribute :virus}]}
          {:jogress/from {:dim/id id
                          :digimon/id (read-byte 0)}
           :jogress/to {:dim/id id
                        :digimon/id (read-byte 4)}
           :jogress/requirements [{:requirement/attribute :free}]}]
         (remove (fn [j]
                   (nil? (get-in j [:jogress/to :digimon/id])))))))

(defn specific-jogress
  [{:keys [dim/id]} row]
  {:jogress/from {:dim/id id
                  :digimon/id (nth row 0)}
   :jogress/to {:dim/id id
                :digimon/id (nth row 1)}
   :jogress/requirements [{:requirement/digimon {:dim/id (nth row 2)
                                                 :digimon/id (nth row 3)}}]})
