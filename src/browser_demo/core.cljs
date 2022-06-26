(ns browser-demo.core
  (:require
   [dim.core :as dim]
   [dim.helpers.bytes :as helper.bytes]
   [dim.content.header :as header]))

(defn spritesheet
  [sprites]
  (let [sprites (->> (rest sprites)
                     (drop-last 2)
                     vec)
        width (reduce + (map :sprite/width sprites))
        height (reduce max (map :sprite/height sprites))
        canvas (.createElement js/document "canvas")
        ctx (.getContext canvas "2d")
        img (.. js/document (querySelector "img#spritesheet"))]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (loop [idx 0
           x-offset 0]
      (when (< idx (count sprites))
        (do (.putImageData ctx
                           (get-in sprites [idx :sprite/data])
                           x-offset 0)
            (recur (inc idx)
                   (+ x-offset (get-in sprites [idx :sprite/width]))))))
    (.toBlob canvas
             (fn [blob]
               (let [url (.createObjectURL js/URL blob)]
                 (set! (.-src img) url)
                 (.revokeObjectURL js/URL url))))))

(let [file-input (.. js/document (querySelector "input#file"))
      canvas-screen (.. js/document (querySelector "canvas#screen"))
      canvas-sprite (.. js/document (querySelector "canvas#sprite"))
      ctx-screen (.getContext canvas-screen "2d")
      ctx-sprite (.getContext canvas-sprite "2d")]
  (set! (.-onchange file-input)
        (fn []
          (when-let [file (first (.-files file-input))]
            (let [reader (new js/FileReader)]
              (set! (.-onload reader)
                    (fn [e]
                      (let [ab (.. e -target -result)
                            dim (dim/read (.-name file) ab)
                            logo (get-in dim [:dim/sprites 0])
                            bg (get-in dim [:dim/sprites 1])
                            sprite-by-id (reduce (fn [m s]
                                                   (assoc m
                                                          (:sprite/id s)
                                                          s))
                                                 {}
                                                 (:dim/sprites dim))
                            idle (->> (nth (:dim/digimon dim) 2)
                                      :digimon/sprites
                                      (map sprite-by-id)
                                      second)]
                        (spritesheet (->> (nth (:dim/digimon dim) 2)
                                          :digimon/sprites
                                          (map sprite-by-id)))
                        (.putImageData ctx-screen (:sprite/data bg) 0 0))))
              (.readAsArrayBuffer reader file))))))

(.log js/console "DiM Reader ready.")
