(ns browser-demo.core
  (:require
   [dim.core :as dim]
   [dim.helpers.bytes :as helper.bytes]
   [dim.content.header :as header]))

(let [file-input (.. js/document (querySelector "input#file"))]
  (set! (.-onchange file-input)
        (fn []
          (when-let [file (first (.-files file-input))]
            (let [reader (new js/FileReader)]
              (set! (.-onload reader)
                    (fn [e]
                      (let [ab (.. e -target -result)
                            dim (dim/read (.-name file) ab)]
                        (prn dim))))
              (.readAsArrayBuffer reader file))))))

(.log js/console "DiM Reader ready.")
