(ns transducer-tut.canon-d2
  (:require [overtone.core :as o]
            [overtone.synth.stringed :as inst]
            [overtone.config.store :as oconf]
            [clojure.core.async :as a]))

(comment
  (o/connect-external-server)
  (o/server-status)
  (o/stop-all)
  (o/kill-server)
  )


(def g (inst/guitar))

(defn play-guitar [rf a e]
  (cond
    (= (type e) clojure.lang.PersistentVector)
    (apply inst/guitar-pick g e)

    (= (type e) clojure.lang.Keyword)
    (inst/guitar-strum g e :down 1))
  (rf a e))

(defn guitar-xf [rf]
  (completing (partial play-guitar rf)))

(defn string-num-translate [rf a e]
  (if (= (type e) clojure.lang.PersistentVector)
    (rf a [(Math/abs (- 6 (first e))) (second e)])
       (rf a e)))

(defn string-translate-xf [rf]
  (completing (partial string-num-translate rf)))

(defn round [rf a e]
  (let [previous-notes (filter #(= (count %) 2) a)
        c (count previous-notes)]
    (if (< c 8)
      (rf a e)
      (let [current-note-played (rf a e)
            previous-note (nth previous-notes (- c 8))
            previous-note (update previous-note 1 (partial + 5))
            delay-time  (+ (rand-nth [600 700 500]) (.getTime (new java.util.Date)))
            round-note (conj previous-note delay-time)]
        (rf current-note-played round-note)))))

(defn round-xf [rf]
  (completing (partial round rf)))

(def input-channel (a/chan 100))
(def output-channel
  (a/transduce (comp string-translate-xf round-xf guitar-xf) conj [] input-channel))
(a/go (println :played (a/<! output-channel)))

(a/>!! input-channel [5 3])
(a/>!! input-channel [6 3])
(a/>!! input-channel [5 0])
(a/>!! input-channel [6 0])
(a/>!! input-channel [6 1])
(a/>!! input-channel [5 3])
(a/>!! input-channel [6 1])
(a/>!! input-channel [6 3])
(a/>!! input-channel :C)
(a/>!! input-channel :G)
(a/>!! input-channel :Am)
(a/>!! input-channel :Em)
(a/>!! input-channel :F)

(a/go (a/close! input-channel))

