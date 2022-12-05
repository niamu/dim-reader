(ns examples.firmware.core)

(comment
  ;; Write out all the sprites from the Vital Hero firmware to the filesystem
  (let [sprites (->> (clojure.java.io/resource "firmwares/vital_hero.bin")
                     firmware.vital-hero/read
                     :firmware/sprites)]
    (doseq [idx (range (count sprites))]
      (let [sprite (nth sprites idx)
            {:sprite/keys [width height data]} sprite
            image (java.awt.image.BufferedImage.
                   width
                   height
                   java.awt.image.BufferedImage/TYPE_4BYTE_ABGR_PRE)
            ^java.awt.image.Raster raster (.getRaster image)]
        (.setDataElements raster 0 0 width height data)
        (javax.imageio.ImageIO/write image
                                     "png"
                                     (clojure.java.io/file
                                      (str "resources/firmware/"
                                           idx ".png")))))))
