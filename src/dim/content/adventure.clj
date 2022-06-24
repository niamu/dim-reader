(ns dim.content.adventure)

(defn adventure
  [{:keys [dim/id]} row]
  {:adventure/steps (nth row 0)
   :adventure/boss {:boss/digimon {:dim/id id
                                   :digimon/id (nth row 1)}
                    :boss/dp (nth row 2)
                    :boss/hp (nth row 3)
                    :boss/ap (nth row 4)}})
