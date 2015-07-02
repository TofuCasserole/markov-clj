(ns markov-clj.core
  (:require [markov-clj.hmm :as hmm]))

(def model
  "an example hidden markov model"
  (hmm/->HiddenMarkovModel #{"Healthy" "Fever"}
                                    #{"Normal" "Cold" "Dizzy"}
                                    {"Healthy" 0.6
                                     "Fever"   0.4}
                                    {"Healthy" {"Healthy" 0.7
                                                "Fever"   0.3}
                                     "Fever"   {"Healthy" 0.4
                                                "Fever"   0.6}}
                                    {"Healthy" {"Normal" 0.5
                                                "Cold"   0.4
                                                "Dizzy"  0.1}
                                     "Fever"   {"Normal" 0.1
                                                "Cold"   0.3
                                                "Dizzy"  0.6}}))

(hmm/viterbi model ["Normal" "Cold" "Dizzy"])
