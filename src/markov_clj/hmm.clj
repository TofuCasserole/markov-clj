(ns markov-clj.hmm
  (:require [clojure.core.matrix :refer :all]))

(defrecord MatrixHMM [init-p trans-p emit-p])

(defn norm-prob-vec [v]
  (array (map (partial * (/ (esum v))) v)))

(defn- fb-reduction [prev-prob xform]
  (norm-prob-vec (mmul xform prev-prob)))

(defn- emission-matrix [emit-p observation]
  (diagonal-matrix (get-row emit-p observation)))

(defn- initial-prob [init-p emit-p observation]
  (norm-prob-vec (mmul (emission-matrix emit-p observation) init-p)))

(defn- create-xform [trans-p emit-p obs]
  (mmul (emission-matrix emit-p obs) trans-p))

(defn- fb-process [init-p trans-p emit-p observations]
  (reductions fb-reduction
              (initial-prob init-p emit-p (first observations))
              (map (partial create-xform trans-p emit-p) (rest observations))))

(defn- forward-prob
  [model observations]
  (fb-process (:init-p model) (:trans-p model) (:emit-p model) observations))

(defn- backward-prob
  [model observations]
  (reverse
   (fb-process (array (repeat (column-count (:emit-p model)) 1))
               (transpose (:trans-p model))
               (:emit-p model)
               (rseq observations))))

(defn forward-backward
  [model observations]
  (let [fwd  (forward-prob model observations)
        bkwd (backward-prob model observations)]
    (map (comp norm-prob-vec mul) fwd bkwd)))
