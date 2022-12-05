(ns util.config
  #?(:clj
     (:require
      [clojure.java.io :as io]
      [clojure.tools.reader.edn :as edn]))
  #?(:cljs (:require-macros
            [util.config :refer [config*]]))
  #?(:clj
     (:import
      [java.io PushbackReader])))

#?(:clj
   (defmacro config*
     []
     (with-open [r (io/reader (io/resource "config.edn"))]
       (edn/read (PushbackReader. r)))))

(def config (config*))
