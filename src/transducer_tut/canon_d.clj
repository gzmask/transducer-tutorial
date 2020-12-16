(ns transducer-tut.canon-d
  (:require [overtone.core :as o]
            [overtone.synth.stringed :as inst]
            [clojure.core.async :as a]))

(o/boot-internal-server)
(o/kill-server)

(def g (inst/guitar))

(inst/guitar-strum g :C :down (rand))
(inst/guitar-pick g 4 0)

(def bpm 120)
(def tick-time (/ (* 1000 60) bpm))

(defn play-chord [chord t]
  (inst/guitar-strum g chord (rand-nth [:up :down]) (rand-nth [0.25 0.5 0.75 0.6 0.7 0.4 0.9]) (+ t (.getTime (new java.util.Date)))))

(play-chord :C tick-time)

(defn chord-xf [rf]
  (fn
    ([] (rf)) ; never used, just to line up with 0-arity reducers.
    ([aggregation] (rf aggregation)) ; final aggregation value is passed.
    ([aggregation element]
     (when (keyword? element)
       (play-chord element (+ tick-time (:time aggregation))))
     (rf (update aggregation :time + tick-time) element))))

(defn play-melody [[string-index fret] t]
  (inst/guitar-pick g string-index fret (+ (rand 20) t (.getTime (new java.util.Date)))))

(play-melody [5 0] tick-time)

(defn melody-xf [rf]
  (fn
    ([] (rf))
    ([a] (rf a))
    ([a e]
     (when (vector? e)
       (play-melody e (+ (/ tick-time 3) (:time a))))
     (rf (update a :time + (/ tick-time 3)) e))))

(transduce (comp chord-xf melody-xf)
           (completing (fn [a e] a))
           {:time 0} ; played info
           [:C [4 0] [4 1] [2 2]
            :G [0 -1] [3 2] [4 0]
            :Am [4 0] [4 1] [5 0]
            :Em [5 0] [5 3] [5 5]
            :F [5 0] [4 3] [5 1]
            :C [4 3] [4 1] [4 0]
            :F [3 0] [2 2] [4 1]
            :G [2 2] [4 1] [4 0]
            :C])

;; for async channel realtime xform, stateful transducer is needed as reducer is not provided.
(defn reactive-melody-xf [rf]
  (let [state (volatile! {:time 0
                          :C [[[4 0] [4 1] [2 2]]
                              [[4 3] [4 1] [4 0]]
                              [[4 0] [4 1] [2 2]]
                              [[4 3] [4 1] [4 0]]
                              [[5 0] [5 1] [5 3] [5 0] [5 1]]
                              [[2 3] [2 2] [2 3] [3 0] [3 2] [4 0] [4 1]]
                              [[0 -1]]
                              ]
                          :G [[[0 -1] [3 2] [4 0]]
                              [[2 2] [4 1] [4 0]]
                              [[0 -1] [3 2] [4 0]]
                              [[2 2] [4 1] [4 0]]
                              [[3 0] [3 2] [4 0] [4 1] [4 3 [5 0] [5 1]]]
                              [[3 2] [4 0] [4 1] [4 3] [5 0] [5 1] [5 3]]
                              ]
                          :Em [[[5 0] [5 3] [5 5]]
                               [[5 0] [5 3] [5 5]]
                               [[3 2] [3 0] [2 3] [3 0] [4 1] [4 0] [4 1]]
                               ]
                          :F [[[5 0] [4 3] [5 1]]
                              [[3 0] [2 2] [4 1]]
                              [[5 0] [4 3] [5 1]]
                              [[3 0] [2 2] [4 1]]
                              [[4 1] [4 0] [3 2] [3 0] [2 3]]
                              [[4 1] [4 0] [4 1] [4 0] [4 1]]
                              ]
                          :Am [[[4 0] [4 1] [5 0]]
                               [[4 0] [4 1] [5 0]]
                               [[4 1] [4 3] [5 0] [2 2] [2 3]]
                               ]})]
    (fn
      ([] (rf))
      ([a] (rf a))
      ([a e]
       (let [melody (first (e @state))
             note-time (/ (* 3 tick-time) (count melody))]
         (play-chord e (:time @state))
         (dorun (map-indexed #(play-melody %2 (+ (* (inc %1) note-time) (:time @state))) melody))
         (vreset! state (assoc @state
                               :time (+ (:time @state) (* 4 tick-time))
                               e (rest (e @state))))
         (rf a e))))))

(sequence reactive-melody-xf [:C :G :Am :Em :F :C :F :G
                              :C :G :Am :Em :F :C :F :G
                              :C :G :Am :Em :F :C :F :G
                              :C])


(def chord-channel (a/chan 100 reactive-melody-xf))

(a/>!! chord-channel :C)
(a/>!! chord-channel :G)
(a/>!! chord-channel :Am)
(a/>!! chord-channel :Em)
(a/>!! chord-channel :F)

;;TODO add go block consummer to make sure realtimeness

;;TODO random melody transducer

