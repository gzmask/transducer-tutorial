(ns transducer-tut.canon-d2
  (:require [overtone.core :as o]
            [overtone.synth.stringed :as inst]
            [overtone.config.store :as oconf]
            [clojure.core.async :as a]))

(comment
  (o/connect-external-server)
  (o/boot-internal-server)
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
        offset 5
        c (count previous-notes)]
    (if (< c offset)
      (rf a e)
      (let [current-note-played (rf a e)
            [string-index fret-index :as previous-note] (nth previous-notes (- c offset))
            previous-note [(+ 2 string-index) (+ 2 fret-index)]
            delay-time  (+ (rand-nth [600 700 500]) (.getTime (new java.util.Date)))
            round-note (conj previous-note delay-time)]
        (rf current-note-played round-note)))))

(defn round-xf [rf]
  (completing (partial round rf)))

(def input-channel (a/chan 100))
(def output-channel
  (a/transduce (comp string-translate-xf round-xf guitar-xf) conj [] input-channel))
(a/go (println :played (a/<! output-channel)))

(a/>!! input-channel :C)
(a/>!! input-channel :G)
(a/>!! input-channel :Am)
(a/>!! input-channel :Em)
(a/>!! input-channel :F)

;(take 2 (cycle [1 2 3]))
;(drop 2 (cycle [1 2 3]))

(def melody (atom (cycle [[5 3]
                          [6 3]
                          [5 0]
                          [6 0]
                          [6 1]
                          [5 3]
                          [6 1]
                          [6 3]])))
(defn next-note []
  (a/>!! input-channel (first @melody))
  (swap! melody rest)
  nil)



(next-note)


(a/go (a/close! input-channel))

