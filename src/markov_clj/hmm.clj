(ns markov-clj.hmm)

(defrecord HiddenMarkovModel [states observables init-p trans-p emit-p])

(defn for-states
  [model f]
  (into {} (map (fn [state] {state (f state)}) (:states model))))

(defn max-state
  [model f]
  (apply max-key f (:states model)))

(defn prev-state-prob
  [model prev-prob observation state prev-state]
  (* (prev-prob prev-state)
     (get (get (:trans-p model) prev-state) state)
     (get (get (:emit-p model) state) observation)))

(defn most-likely-previous-state
  [model observation prev-prob state]
    (apply max (map (partial prev-state-prob model prev-prob observation state)
                    (:states model))))

(defn calc-next-states
  [model observation prev-prob]
  (for-states model (fn [x] (most-likely-previous-state model observation prev-prob x))))

(defn viterbi-step
  [model observation prev-prob path]
  (let [next-state-probs (calc-next-states model observation prev-prob)]
    {:prob next-state-probs
     :path (conj path (max-state model next-state-probs))}))

(defn viterbi-recur
  [model obs {prev-prob :prob path :path}]
  (if (empty? obs)
    {:prob prev-prob
     :path path}
    (let [next-step (viterbi-step model (first obs) prev-prob path)]
      (recur model (rest obs) next-step))))

(defn viterbi
  "find the most likely sequence of hidden states for a given observed sequence"
  [model observations]
  (if (empty? observations)
    nil
    (let [init-state-prob (for-states model (fn [x] (* (get (:init-p model) x)
                                                       (get (get (:emit-p model) x) (first observations)))))
          {prob :prob path :path} (viterbi-recur model (rest observations)
                                                 {:prob init-state-prob
                                                  :path [(max-state model init-state-prob)]})]
      {:prob (prob (peek path))
       :path path})))
