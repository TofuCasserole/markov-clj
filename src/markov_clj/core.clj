(ns markov-clj.core)

(defrecord HiddenMarkovModel [states observables init-p trans-p emit-p])

(def model (HiddenMarkovModel. #{"Healthy" "Fever"}
                               #{"Normal" "Cold" "Dizzy"}
                               {"Healthy" 0.6
                                "Fever"   0.4}
                               {"Healthy" {"Healthy" 0.7
                                           "Fever"   0.3}
                                "Fever"   {"Healthy" 0.4
                                           "Fever"   0.6}}
                               {"Healthy" {"Normal"  0.5
                                           "Cold"    0.4
                                           "Dizzy"   0.1}
                                "Fever"   {"Normal"  0.1
                                           "Cold"    0.3
                                           "Dizzy"   0.6}}))

(defn fmap
  [f coll]
  (into {} (map (fn [x] {x (f x)}) coll)))

(defn prev-state-prob
  [model prev-prob observation state prev-state]
  (* (prev-prob prev-state)
     (get (get (:trans-p model) prev-state) state)
     (get (get (:emit-p model) state) observation)))

(defn most-likely-previous-state
  [model observation prev-prob state]
  (let [calc-prev-prob (partial prev-state-prob
                                model prev-prob observation state)
        max-state      (apply max-key calc-prev-prob (:states model))]
    [max-state (calc-prev-prob max-state)]))

(defn calc-next-states
  [model observation prev-prob]
  (fmap (fn [x] (nth (most-likely-previous-state model observation prev-prob x) 1))
        (:states model)))

(defn viterbi-step
  [model observation prev-prob path]
  (let [next-state-probs (calc-next-states model observation prev-prob)]
    [next-state-probs (conj path (apply max-key next-state-probs (:states model)))]))

(defn viterbi
  [model observations]
  (if (empty? observations)
    nil
    (let [init-state-prob (fmap (fn [x]
                                (* (get (:init-p model) x)
                                   (get (get (:emit-p model) x) (first observations))))
                              (:states model))]
      (loop [obs (rest observations)
             prev-prob init-state-prob
             path [(apply max-key init-state-prob (:states model))]]
        (if (empty? obs)
          [prev-prob path]
          (let [[new-prob new-path] (viterbi-step model (first obs) prev-prob path)]
            (recur (rest obs) new-prob new-path)))))))

(viterbi model ["Normal" "Cold" "Dizzy"])
