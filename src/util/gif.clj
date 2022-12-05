(ns util.gif
  (:require
   [clojure.java.io :as io])
  (:import
   [java.awt.image BufferedImage]
   [java.nio ByteBuffer ByteOrder]
   [javax.imageio IIOImage ImageIO ImageTypeSpecifier ImageWriter]
   [javax.imageio.metadata IIOMetadataNode]))

(defn- first-node
  [root & tags]
  (->> tags
       (map (fn [tag]
              (let [nodes (.getElementsByTagName root tag)]
                (when (pos? (.getLength nodes))
                  (.item nodes 0)))))
       (remove nil?)
       first))

(defn- get-graphic-control-extension
  [root]
  (first-node root "GraphicControlExtension"))

(defn- create-graphic-control-extension
  [root]
  (let [gce (IIOMetadataNode. "GraphicControlExtension")
        reference (first-node root
                              "PlainTextExtension"
                              "ApplicationExtensions"
                              "CommentExtensions")]
    (.insertBefore root gce reference)
    gce))

(defn- set-delay-time
  [d node]
  (let [gce (or (get-graphic-control-extension node)
                (create-graphic-control-extension node))]
    (.setAttribute gce "delayTime" (str (/ d 10)))
    (.setAttribute gce "disposalMethod" "restoreToBackgroundColor")))

(defn write-image
  [^BufferedImage buffered-image
   ^ImageWriter image-writer
   ^long delay-time]
  (let [param (.getDefaultWriteParam image-writer)
        type (ImageTypeSpecifier/createFromRenderedImage buffered-image)
        metadata (.getDefaultImageMetadata image-writer type param)
        format (.getNativeMetadataFormatName metadata)
        tree (.getAsTree metadata format)]
    (set-delay-time delay-time tree)
    (.setFromTree metadata format tree)
    (.writeToSequence image-writer
                      (IIOImage. buffered-image nil metadata)
                      nil)))

(defn create-agif-from-images
  [os images delay-time]
  (with-open [ios (ImageIO/createImageOutputStream os)]
    (let [wr (->> "image/gif" ImageIO/getImageWritersByMIMEType .next)]
      (.setOutput wr ios)
      (.prepareWriteSequence wr nil)
      (doseq [f images]
        (write-image f wr delay-time))
      (.endWriteSequence wr)
      (.dispose wr))))
