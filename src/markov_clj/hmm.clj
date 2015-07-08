(ns markov-clj.hmm
  (:require [clojure.core.matrix :refer :all]))

(defprotocol HiddenMarkovModel
  (forward-prob [model observations]))
