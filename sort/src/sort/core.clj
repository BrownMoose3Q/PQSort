(ns sort.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defmacro bench
  "Times the execution of forms, discarding their output and returning
a long in nanoseconds."
  ([& forms]
    `(let [start# (System/nanoTime)]
       ~@forms
       (- (System/nanoTime) start#))))
(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn get-seq
  [path]
  (map parse-int (str/split (slurp path) #"\r\n")))


(defn even-index
  [[fir sec & rest]]
  (if sec
    (into [sec] (even-index rest))
    []))

(defn odd-index
  [[fir sec & rest]]
  (if sec
    (into [fir] (odd-index rest))
    []))

(defn time-qs [coll]
  (time (qsort2 (flatten coll)))
  nil)

(defn fsdone? [fs]
  (if (some false? (map future-done? fs))
    (do
      (Thread/sleep 400)
      (fsdone? fs))
    nil))

(defn qsort2 [[pivot & xs]]
  (when pivot
    (let [smaller #(< % pivot)]
      (lazy-cat (qsort2 (filter smaller xs))
		[pivot]
		(qsort2 (remove smaller xs))))))
(defn break [[fir & rest]]
  ;;(println "BFIR:" fir "BREST:" rest)
  (if fir
    (into [(odd-index fir) 
           (even-index fir)]
          (break rest))
    []))
(defn swap
  "(o <-> o o <-> o)"
  [[up down & rest]]
  ;;(println "UP:" up "DOWN:" down "REST:" rest)
  (if down
    (into [(into (first (deref up))
                 (first (deref down)))
           (into (second (deref up))
                 (second (deref down)))]
          (swap rest))
    []))

(defn swap-all [coll]
  ;;(println "COLL:" coll)
  (map swap coll))

(defn step-sort [pivot xs]
  (when pivot
    (let [smaller #(<= % pivot)]
      (vector (filter smaller xs)
                (remove smaller xs)))))

(defn fdivide 
  "only powers of 2"
  [parts coll]
  (let [amount (int (Math/ceil (/ (count coll) parts)))]
    [(partition amount amount nil coll)]))

(defn apply-big-les [coll]
  ;;(println "fir:" (first coll) "rest:" (rest coll))
  (if (not (empty? (first coll))) ;;"( [(..)(..)] [(..)(..)] )"
    (into [(map #(future (step-sort (first (first (first coll))) %)) (first coll))]
            (apply-big-les (rest coll)))))
(defn divide
  [coll] ;;"( [(..)(..)] [(..)(..)] )"
  ;;(println "DFIR:" (first coll) "DREST:" (rest coll))
  (when (not (empty? (first coll)))
    (if (second (first coll))
      (divide (break (swap-all (apply-big-les coll))))
      (into [(future (qsort2 (flatten (first coll))))] 
            (divide (rest coll))))))

(defn hqsort
  [path parts]
  (let [in (get-seq path)]
    (time (fsdone? (divide (fdivide parts in))))
    nil))
