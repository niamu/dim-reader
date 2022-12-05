(ns dim.content.header
  (:require
   [util.bytes :as bytes]
   #?(:cljs [goog.string :refer [format]]))
  #?(:clj
     (:import
      [java.text SimpleDateFormat]
      [java.util UUID])))

(defn header
  [^bytes bs]
  (when-not (zero? (aget bs 0x30))
    (throw (#?(:clj Exception.
               :cljs js/Error.) "Reader only handles data DIMs")))
  (let [id (bytes/uint16-at bs 0x32)
        year (bytes/uint16-at bs 0x36)
        month (bytes/uint16-at bs 0x38)
        day (bytes/uint16-at bs 0x3a)
        date-string (format "%04d-%02d-%02d"
                            (+ year 2000) month day)]
    {:dim/id id
     :dim/descriptor (bytes/bytes->string
                      (bytes/selection bs 0x10 0x30))
     :dim/type (bytes/uint16-at bs 0x30)
     :dim/date (->> (.parse #?(:clj (SimpleDateFormat. "yyyy-MM-dd")
                               :cljs js/Date)
                            date-string)
                    #?(:cljs (new js/Date)))
     :dim/revision (bytes/uint16-at bs 0x3c)
     #_#_:dim/header-signature (UUID/nameUUIDFromBytes
                                (bytes/selection bs 0x40 0x60))
     #_#_:dim/sprite-signature (UUID/nameUUIDFromBytes
                                (bytes/selection bs 0x1010 0x1030))
     #_#_:dim/location-at-0x8F (bit-and 0xFF (aget bs 0x8F))}))
