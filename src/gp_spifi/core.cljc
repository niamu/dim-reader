(ns gp-spifi.core
  (:require
   [util.bytes :as bytes]))

(def group-type
  {0xFF :unknown
   0x00 :speech
   0x01 :melody
   0x02 :image
   0x03 :movie
   0x80 :equation})

(defn parse-at
  [bs addr]
  (let [gp-spifi-header (-> (bytes/selection bs
                                             addr
                                             (+ addr 0x08))
                            bytes/bytes->string)
        ;; 0x08..0xFF has always been the following so far:
        ;; 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00
        maybe-version (bytes/selection bs
                                       (+ addr 0x08)
                                       (+ addr 0x0F))
        ;; 0x10..0x16 seems to be 0 padded and therefore not parsed
        address-base (bytes/uint16-at bs (+ addr 0x16))
        group-types (->> (bytes/selection bs
                                          (+ addr 0x18)
                                          (+ addr 0x1C))
                         bytes/->uint32-buffer
                         (map (fn [offset]
                                (-> (bytes/uint32-at bs (+ addr offset))
                                    (bit-and 0xFF)
                                    group-type))))
        ;; 0x1C..0x20 is 0 padded
        ;; custom-32byte-header is typically always 0 filled
        custom-32byte-header (bytes/selection bs
                                              (+ addr 0x20)
                                              (+ addr 0x40))
        group-count (bytes/uint32-at bs (+ addr 0x40))
        group-header-offsets (->> (bytes/selection bs
                                                   (+ addr 0x44)
                                                   (+ addr
                                                      0x44
                                                      (* group-count
                                                         4)))
                                  bytes/->uint32-buffer
                                  (into []))]
    (when (not= gp-spifi-header "GP_SPIFI")
      (throw (new #?(:clj Exception
                     :cljs js/Error) "'GP_SPIFI' should be starting bytes")))
    (when (not= address-base 1)
      (throw (new #?(:clj Exception
                     :cljs js/Error) "Address Base was expected to be 1")))
    (when (not (every? zero? custom-32byte-header))
      (throw (new #?(:clj Exception
                     :cljs js/Error) "Custom 32 byte header was not all zeroes")))
    (loop [group-idx 0
           result {:gp-spifi/custom-32byte-header custom-32byte-header
                   :gp-spifi/address-base address-base}]
      (if (< group-idx (count group-header-offsets))
        (let [header-offset (get group-header-offsets group-idx)
              group-type (nth group-types group-idx)
              file-count (bytes/uint32-at bs (+ addr header-offset))
              group-offset (bytes/uint32-at bs (+ addr
                                                  header-offset
                                                  4))
              file-addrs (->> (bytes/selection bs
                                               (+ addr
                                                  header-offset
                                                  8)
                                               (+ addr
                                                  header-offset
                                                  8
                                                  (* file-count 4)))
                              bytes/->uint32-buffer
                              (reduce conj [group-offset])
                              (partition 2 1))
              files (map (fn [[start end]]
                           (bytes/selection bs
                                            (+ addr start)
                                            (+ addr end)))
                         file-addrs)]
          (recur (inc group-idx)
                 (update result
                         :gp-spifi/groups
                         conj
                         {:group/type group-type
                          :group/files files})))
        result))))
