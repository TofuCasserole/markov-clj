(ns markov-clj.hmm
  (:require [clojure.core.matrix :refer :all]))

(defprotocol HiddenMarkovModel
  (forward-prob [model observations]))

(defn- norm-prob-vec [v]
  (let [vecsum (esum v)]
    ))

(defrecord MatrixHMM [states observables init-p trans-p emit-p]
  HiddenMarkovModel
  (forward-prob
   [model observations]
   (reduce (fn [prev-prob ])
           (:init-p model)
           (map (fn [obs]
                  (* (diagonal-matrix (get-row (:emit-p model) obs))
                     (:trans-p model)))
                observations))))
