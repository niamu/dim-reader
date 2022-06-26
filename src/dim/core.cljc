(ns dim.core
  (:refer-clojure :exclude [read])
  (:require
   #?(:clj [clojure.java.io :as io])
   [dim.content.adventure :as adventure]
   [dim.content.evolution :as evolution]
   [dim.content.header :as header]
   [dim.content.jogress :as jogress]
   [dim.content.statistic :as statistic]
   [dim.content.sprite :as sprite]
   [dim.helpers.bytes :as helper.bytes])
  #?(:clj
     (:import
      [java.io File])))

(defn valid-checksum?
  [^bytes bs]
  (let [calculated-checksum (->> (helper.bytes/selection bs 0x0 0x3FFFFE)
                                 (helper.bytes/->uint16-buffer)
                                 (reduce (fn [accl b]
                                           (bit-and 0xFFFF (+ accl b)))
                                         0))
        dim-checksum (helper.bytes/uint16-at bs 0x3FFFFE)]
    (when (not= calculated-checksum dim-checksum)
      (throw (#?(:clj Exception.
                 :cljs js/Error.) (str "Invalid Checksum. Expected "
                                       dim-checksum
                                       " and calculated "
                                       calculated-checksum))))
    bs))

(defn read
  #?(:clj [^File file]
     :cljs [filename file])
  (let [bs (-> (helper.bytes/file->bytes file)
               valid-checksum?)
        filename #?(:clj (.getName file)
                    :cljs filename)
        header (header/header (helper.bytes/selection bs 0x0 0x1030))
        statistics (->> (-> (helper.bytes/selection bs 0x30000 0x40000)
                            (helper.bytes/->table (partial statistic/statistic
                                                           header) 12))
                        (reduce (fn [accl {:keys [digimon/stage] :as s}]
                                  (if (<= stage 5)
                                    (conj accl s)
                                    accl))
                                []))
        evolutions (-> (helper.bytes/selection bs 0x40000 0x50000)
                       (helper.bytes/->table (partial evolution/evolution
                                                      header) 12))
        evolutions (->> evolutions
                        (map-indexed (fn [idx evolution]
                                       (assoc evolution
                                              :evolution/id
                                              (keyword
                                               (str "dim" (:dim/id header))
                                               (str "evolution" idx))))))
        adventures (-> (helper.bytes/selection bs 0x50000 0x60000)
                       (helper.bytes/->table (partial adventure/adventure
                                                      header) 5))
        adventures (->> adventures
                        (map-indexed (fn [idx adventure]
                                       (assoc adventure
                                              :adventure/id
                                              (keyword
                                               (str "dim" (:dim/id header))
                                               (str "adventure" idx))
                                              :adventure/stage
                                              (inc idx)))))
        jogresses (-> (helper.bytes/selection bs 0x70000 0x80000)
                      (helper.bytes/->table (partial jogress/jogress
                                                     header) 5)
                      flatten)
        specific-jogresses
        (-> (helper.bytes/selection bs 0x80000 0x100000)
            (helper.bytes/->table (partial jogress/specific-jogress
                                           header)
                                  4))
        sprites (sprite/sprites header
                                (helper.bytes/selection bs
                                                        0x60000
                                                        0x70000)
                                (helper.bytes/selection bs
                                                        0x100000
                                                        0x3FFFFE))
        evolutions-by-statistic-idx
        (-> (group-by :evolution/from evolutions)
            (update-vals #(map (fn [evolution]
                                 (dissoc evolution :evolution/from))
                               %)))
        jogresses-by-statistic-idx
        (-> (->> (concat jogresses specific-jogresses)
                 (map-indexed (fn [idx j]
                                (assoc j
                                       :jogress/id
                                       (keyword
                                        (str "dim" (:dim/id header))
                                        (str "jogress" idx)))))
                 (group-by :jogress/from))
            (update-vals #(map (fn [jogress]
                                 (dissoc jogress :jogress/from))
                               %)))
        sprites-by-statistic-idx
        (loop [idx 0
               remaining-sprites (drop 10 sprites)
               by-index {}]
          (if (< idx (count statistics))
            (let [{:digimon/keys [stage] :as stat} (nth statistics idx)
                  sprite-count (case stage
                                 0 6
                                 1 7
                                 14)]
              (recur (inc idx)
                     (drop sprite-count remaining-sprites)
                     (assoc by-index
                            idx
                            (->> (take sprite-count remaining-sprites)
                                 (map-indexed
                                  (fn [sprite-idx sprite]
                                    (assoc sprite
                                           :sprite/label
                                           (get-in sprite/label-by-sprite-count
                                                   [sprite-count
                                                    sprite-idx]))))))))
            by-index))
        digimon (map-indexed
                 (fn [idx statistic]
                   (let [evolutions (->> (get evolutions-by-statistic-idx
                                              (keyword
                                               (str "dim" (:dim/id header))
                                               (str "digimon" idx)))
                                         (into []))
                         jogresses (->> (get jogresses-by-statistic-idx
                                             (keyword
                                              (str "dim" (:dim/id header))
                                              (str "digimon" idx)))
                                        (into []))]
                     (cond-> (merge statistic
                                    {:digimon/id (keyword
                                                  (str "dim" (:dim/id header))
                                                  (str "digimon" idx))
                                     :digimon/sprites
                                     (->> (get sprites-by-statistic-idx idx)
                                          (into []))})
                       (not (empty? evolutions))
                       (assoc :digimon/evolutions evolutions)
                       (not (empty? jogresses))
                       (assoc :digimon/jogresses jogresses))))
                 statistics)]
    (merge header
           {:dim/filename filename
            :dim/sprites (->> (concat [(assoc (first sprites)
                                              :sprite/label :logo)
                                       (assoc (second sprites)
                                              :sprite/label :background)]
                                      (->> sprites
                                           (drop 2)
                                           (take 8)
                                           (map-indexed
                                            (fn [idx sprite]
                                              (assoc sprite
                                                     :sprite/label
                                                     (keyword (str "egg"
                                                                   idx))))))
                                      (->> digimon
                                           (mapcat :digimon/sprites)))
                              (into []))
            :dim/digimon (mapv (fn [d]
                                 (-> d
                                     (update :digimon/sprites
                                             (fn [sprites]
                                               (mapv :sprite/id sprites)))
                                     (update :digimon/evolutions
                                             (fn [evolutions]
                                               (mapv :evolution/id evolutions)))
                                     (update :digimon/jogresses
                                             (fn [jogresses]
                                               (mapv :jogress/id jogresses)))))
                               digimon)
            :dim/evolutions (->> digimon
                                 (mapcat :digimon/evolutions)
                                 (into []))
            :dim/jogresses (->> digimon
                                (mapcat :digimon/jogresses)
                                (into []))
            :dim/adventures (into [] adventures)})))

#?(:clj
   (comment
     (letfn [(save-ppm!
               [filename {:sprite/keys [width height ^bytes data] :as sprite}]
               (.mkdirs (io/file "resources/output"))
               (with-open [o (io/output-stream (str "resources/output/" filename
                                                    ".ppm"))]
                 (.write o (byte-array
                            (concat (.getBytes (format "P6\n%d %d\n255\n"
                                                       width height))
                                    (->> data
                                         (partition 3 4)
                                         (map reverse)
                                         flatten
                                         byte-array))))))]
       (->> (read (io/file "resources/dimcards/DIM_Agumon.bin"))
            :dim/sprites
            (map-indexed (fn [idx sprite]
                           (save-ppm! idx sprite)))))

     (do (time (->> (file-seq (io/file "resources/dimcards/"))
                    (filter (fn [f]
                              (let [filename (.getName f)]
                                (and (.isFile f)
                                     (= (subs filename
                                              (- (count filename) 4))
                                        ".bin")))))
                    (pmap read)
                    doall))
         nil)))
