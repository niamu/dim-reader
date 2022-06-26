(ns dim.content.adventure)

(defn adventure
  [{:keys [dim/id]} row]
  {:adventure/steps (aget row 0)
   :adventure/boss {:boss/digimon (keyword
                                   (str "dim" id)
                                   (str "digimon" (aget row 1)))
                    :boss/dp (aget row 2)
                    :boss/hp (aget row 3)
                    :boss/ap (aget row 4)}})
