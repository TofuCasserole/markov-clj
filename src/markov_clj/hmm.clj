(ns markov-clj.hmm
  (:require [clojure.core.matrix :refer :all]))

(defrecord MatrixHMM [states observables init-p trans-p emit-p])

(defn norm-prob-vec [v]
  (array (map (partial * (/ (esum v))) v)))

(defn fb-reduction [prev-prob xform]
  (norm-prob-vec (mmul prev-prob xform)))

(defn emission-matrix [emit-p observation]
  (diagonal-matrix (get-row emit-p observation)))

(defn initial-prob [init-p emit-p observation]
  (norm-prob-vec (mmul (emission-matrix emit-p observation) init-p)))

(defn create-xform [trans-p emit-p]
  #(mmul trans-p (emission-matrix emit-p %)))

(defn fb-process [init-p trans-p emit-p observations]
  (reductions fb-reduction
              (initial-prob init-p emit-p (first observations))
              (map (create-xform trans-p emit-p) (rest observations))))

(defn forward-prob
  [model observations]
  (fb-process (:init-p model) (:trans-p model) (:emit-p model) observations))

(defn backward-prob
  [model observations]
  (reverse
   (fb-process (array (repeat (count (:observables model)) 1))
               (transpose (:trans-p model))
               (:emit-p model)
               (rseq observations))))

(defn forward-backward
  [model observations]
  (let [fwd  (forward-prob model observations)
        bkwd (backward-prob model observations)]
    (map (comp norm-prob-vec mul) fwd bkwd)))

(def example-hmm (->MatrixHMM [0 1]
                              [0 1]
                              [0.5 0.5]
                              [[0.7 0.3]
                               [0.3 0.7]]
                              [[0.9 0.2]
                               [0.1 0.8]]))

(forward-backward example-hmm [0 0 1 0 0])
