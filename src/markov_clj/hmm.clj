(ns markov-clj.hmm
  (:require [clojure.core.matrix :refer :all]))

(defprotocol HiddenMarkovModel
  (forward-prob [model observations]))

(defn- norm-prob-vec [v]
  (map (partial * (/ (esum v))) v))

(defn- create-xform [model observation]
  (mmul (:trans-p model)
        (diagonal-matrix (get-row (:emit-p model) observation))))

(defrecord MatrixHMM [init-p trans-p emit-p]
  HiddenMarkovModel
  (forward-prob
   [model observations]
   (reduce (comp norm-prob-vec mmul)
           (:init-p model)
           (map (partial create-xform model)
                observations))))
