;;     _,aaaaaaaaaaaaaaaaaaa,_                _,aaaaaaaaaaaaaaaaaaa,_
;;   ,P"                     "Y,            ,P"                     "Y,
;;  d'    ,aaaaaaaaaaaaaaa,    `b          d'    ,aaaaaaaaaaaaaaa,    `b
;; d'   ,d"            ,aaabaaaa8aaaaaaaaaa8aaaadaaa,            "b,   `b
;; I    I              I                            I      ,adba,  I    I
;; Y,   `Y,            `aaaaaaaaaaaaaaaaaaaaaaaaaaaa'      I    I,P'   ,P
;;  Y,   `baaaaaaaaaaaaaaad'   ,P          Y,   `baaaaaaaaaI    Id'   ,P
;;   `b,                     ,d'            `b,            I    I   ,d'
;;     `baaaaaaaaaaaaaaaaaaad'                `baaaaaaaaaaaI    Iaad'
;;                                                         I    I
;;                                Transducers              I    I
;;                                                         I    I
;;     _,aaaaaaaaaaaaaaaaaaa,_                _,aaaaaaaaaaaI    Iaa,_
;;   ,P"                     "Y,            ,P"            I    I   "Y,
;;  d'    ,aaaaaaaaaaaaaaa,    `b          d'    ,aaaaaaaaaI    I,    `b
;; d'   ,d"            ,aaabaaaa8aaaaaaaaaa8aaaadaaa,      I    I"b,   `b
;; I    I  ,adba,      I                            I      `"YP"'  I    I
;; Y,   `Y,I    I      `aaaaaaaaaaaaaaaaaaaaaaaaaaaa'            ,P'   ,P
;;  Y,   `bI    Iaaaaaaaaad'   ,P          Y,   `baaaaaaaaaaaaaaad'   ,P
;;   `b,   I    I            ,d'            `b,                     ,d'
;;     `baaI    Iaaaaaaaaaaad'                `baaaaaaaaaaaaaaaaaaad'
;;         I    I
;;         I    I                 -- An Introductory
;;         I    I
;;     _,aaI    Iaaaaaaaaaaa,_                _,aaaaaaaaaaaaaaaaaaa,_
;;   ,P"   I    I            "Y,            ,P"                     "Y,
;;  d'    ,I    Iaaaaaaaaa,    `b          d'    ,aaaaaaaaaaaaaaa,    `b
;; d'   ,d"I    I      ,aaabaaaa8aaaaaaaaaa8aaaadaaa,            "b,   `b
;; I    I  `"YP"'      I                            I              I    I
;; Y,   `Y,            `aaaaaaaaaaaaaaaaaaaaaaaaaaaa'            ,P'   ,P
;;  Y,   `baaaaaaaaaaaaaaad'   ,P          Y,   `baaaaaaaaaaaaaaad'   ,P
;;   `b,                     ,d'            `b,                     ,d'
;;     `baaaaaaaaaaaaaaaaaaad'                `baaaaaaaaaaaaaaaaaaad'
(ns transducer-tut.tran)

;; conjoin returns a collection with element added.
(conj [] 1)

;; conjoin is a reducer
(reduce conj [] (range 10))

;; a conjoin is a reducer that adds element into a collection
(defn reducer-f [aggregation element]
  (conj aggregation element))

(reducer-f [] 1)

(def zero-to-nine (range 10))

(reduce reducer-f [] zero-to-nine)

;; a simplified transducer that adds one to every element
(defn transducer-f [rf]
  (fn [aggregation element]
    (rf aggregation (inc element))))

(def new-reducer
  (transducer-f reducer-f))

(new-reducer [] 1)

(reduce new-reducer (list) zero-to-nine)

;; Multi-arity functions
(defn afn
  ([] 0)
  ([i] 1)
  ([i e] 2))

(afn)
(afn :a)
(afn :a :a)

;; a completed transducer
(defn transducer-f [rf]
  (fn
    ([] (rf [])) ; <--- zero arity added
    ([aggregation] (rf aggregation)) ; <--- single arity added
    ([aggregation element]
     (rf aggregation (inc element)))))

(transduce transducer-f conj zero-to-nine)

;; Built-in transducers
(def inc-xf (map inc))

(transduce inc-xf conj zero-to-nine)
(transduce (filter odd?) conj zero-to-nine)
(transduce (take 10) conj (range))
(transduce (partition-all 2) conj zero-to-nine)
(transduce (mapcat (partial repeat 2)) conj zero-to-nine) ; <--- any taker?

;; combining transducers
(def inc-even-xf
  (comp (map inc)
        (filter even?)))

(transduce inc-even-xf conj zero-to-nine)
(transduce (comp (mapcat (partial repeat 2))
                 (distinct)) conj zero-to-nine) ; <--- more fun!

;; Other ways to use transducers

(into (list) inc-even-xf zero-to-nine) ; <--- specific container type

(def a-lazy-seq
  (sequence (filter even?) (range))) ; <--- only for sequence types
(nth a-lazy-seq 9999)

; And multiple seqs
(sequence (comp (map +)
                (take 5))
          (range)
          (range))

(def a-reducible
  (eduction (filter even?) (range)))
(transduce (take 5) conj a-reducible)


;; Space complexity
(let [xs (doall  (map inc zero-to-nine)); <--- intermediate step adds O(n) space 
      xs (filter odd? xs)]
  (doall xs)); <--- O(2n) at this point


;lazy-seq: let realization section be c
(->> zero-to-nine
     (map inc); <--- intermediate step adds O(c) space 
     (filter odd?)); <--- O(2c) at this point

; wherein transducers, always O(c) space no matter the intermediate steps.
(sequence (comp (map inc)
                 (filter odd?))
           zero-to-nine); <--- on realization c, this will be O(c)


;; Make your own reducible/transducibles
(defn gen
  "An generator that constantly produces the input element in random batches."
  [e]
  (reify
    clojure.lang.IReduceInit
    (reduce [_ rf init]
      (loop [generated-result (repeat (rand-int 10) e)
             aggregation (or init (rf))]
        (if (reduced? aggregation); <--- early termination
          @aggregation
          (let [next-generated-result (repeat (rand-int 10)e)]
            (recur next-generated-result
                   (rf aggregation generated-result))))))))

(transduce (take 3) conj (gen :SAPConcur))


;; Use case Example

;; As any live coding thing goes, there shall be a guitar...
(require '[overtone.core :as o])
;;this needs internal microphone
(o/boot-internal-server)
(require '[overtone.synth.stringed :as inst])

(def g (inst/guitar))
(comment
  (inst/guitar-strum g :C :down 0.25)
  (inst/guitar-pick g 1 3)
  (inst/guitar-strum g :F :down 0.25)
  (inst/guitar-strum g :Gm :down 0.25)

  (inst/guitar-strum g [0 -1 -1 -1 -1 0] :down 0.1)
  (inst/guitar-strum g [2 -1 -1 -1 -1 2] :down 0.1)
  (inst/guitar-strum g [3 -1 -1 -1 -1 3] :down 0.1)
  )

;; a transducer that plays these nices chords
(def guitar-xf
  (comp
   (map clojure.string/upper-case)
   (map keyword)
   (map (fn play-chord [c]
          (inst/guitar-strum g c :down 0.25)))))



;; a core.async channel hooked to the transducer
(require '[clojure.core.async :as a])
(def some-external-input (a/chan 1 guitar-xf))

(comment
  (a/>!! some-external-input "C")
  (a/>!! some-external-input "D")
  (a/>!! some-external-input "E")
  (a/>!! some-external-input "A")
  )

;; then a seasaw UI that puts keystrokes to core.async channel
(require '[seesaw.core :as s])
(defn -main [& args]
  (let [handler (fn [e]
                  (println :yoooo!!!!)
                  (s/alert "pressed key!")
                  ;(a/>!! some-external-input (.getKeyChar e))
                  )
        f (s/frame :title "whatdacat"
                   ;:content (s/text :text "presskey here")
                   )]
  (a/go-loop []
    (let [x (a/<! some-external-input)])
    (recur))
  (s/listen f :key-pressed handler)
  (s/show! f)))

(comment
  (-main)
  )
