(ns examples.dim.core)

(comment
  ;; Rough benchmark for how to long it takes to read multiple files
  (do (time (->> (file-seq (clojure.java.io/file "resources/dimcards/JA"))
                 (filter (fn [f]
                           (let [filename (.getName f)]
                             (and (.isFile f)
                                  (= (subs filename
                                           (- (count filename) 4))
                                     ".bin")))))
                 (pmap dim.core/read)
                 doall))
      nil)

  ;; Create a GIF of all the Agumon sprites from a DIM
  (util.gif/create-agif-from-images
   (clojure.java.io/output-stream
    (clojure.java.io/file "resources/Agumon.gif"))
   (->> (io/resource "dimcards/JA/DIM_AgumonEX.bin")
        dim.core/read
        :dim/sprites
        (drop 24)
        (take 11)
        (mapv (fn [{:sprite/keys [width height data]}]
                (let [image (java.awt.image.BufferedImage.
                             width
                             height
                             BufferedImage/TYPE_4BYTE_ABGR_PRE)
                      ^java.awt.image.Raster raster (.getRaster image)]
                  (.setDataElements raster 0 0 width height data)
                  image))))
   250)

  ;; Write out all the Agumon sprites from a DIM to the filesystem
  (for [sprite (->> (clojure.java.io/resource
                     "dimcards/JA/DIM_AgumonEX.bin")
                    dim.core/read
                    :dim/sprites
                    (drop 24)
                    (take 11))]
    (let [{:sprite/keys [width height data label]} sprite
          image (java.awt.image.BufferedImage.
                 width
                 height
                 java.awt.image.BufferedImage/TYPE_4BYTE_ABGR_PRE)
          ^java.awt.image.Raster raster (.getRaster image)]
      (.setDataElements raster 0 0 width height data)
      (javax.imageio.ImageIO/write image
                                   "png"
                                   (io/file (str "resources/"
                                                 (name label) ".png"))))))
