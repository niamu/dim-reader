(ns dim.content.header
  (:require
   [dim.helpers.bytes :as helper.bytes])
  (:import
   [java.text SimpleDateFormat]
   [java.util UUID]))

(defn header
  [^bytes bs]
  (when-not (zero? (aget bs 0x30))
    (throw (Exception. "Reader only handles data DIMs")))
  (let [id (helper.bytes/uint16-at bs 0x32)
        year (helper.bytes/uint16-at bs 0x36)
        month (helper.bytes/uint16-at bs 0x38)
        day (helper.bytes/uint16-at bs 0x3a)]
    {:dim/id id
     :dim/descriptor (String. (helper.bytes/selection bs 0x10 0x30))
     :dim/type (helper.bytes/uint16-at bs 0x30)
     :dim/date (.parse (SimpleDateFormat. "yy/MM/dd")
                       (format "%02d/%02d/%02d"
                               year month day))
     :dim/revision (helper.bytes/uint16-at bs 0x3c)
     #_#_:dim/header-signature (UUID/nameUUIDFromBytes
                                (helper.bytes/selection bs 0x40 0x60))
     #_#_:dim/sprite-signature (UUID/nameUUIDFromBytes
                                (helper.bytes/selection bs 0x1010 0x1030))
     #_#_:dim/location-at-0x8F (bit-and 0xFF (aget bs 0x8F))}))
