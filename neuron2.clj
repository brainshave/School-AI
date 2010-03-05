(ns
    #^{:author "Szymon Witamborski"
       :doc "Second, cleaner attempt to neuron program."}
  neuron2
  (:use (clojure.contrib [str-utils2 :only (split-lines)]
			 [duck-streams :only (spit)])))

(defstruct neuron :threshold :factors)

(defn read-neuron
  "Reads neuron spec from file f"
  [f]
  (let [nums (map #(Double/valueOf %)
		  (split-lines (slurp f)))
	th (first nums) ; first line is threshold
	n (second nums) ; second line is nodes number
	facs (take n (drop 2 nums))] ; rest lines are factors
    (struct neuron th facs)))
	    
(defn read-inputs
  "Reads inputs from file"
  [f]
  (for [line (split-lines (slurp f))]
    (map #(Integer/valueOf(str %)) line)))

(defn process-inputs
  "Processes inputs ins for neuron nrn or give fac[tor]s and th[reshold] separetly"
  ([th facs ins])
  ([nrn ins] (process-inputs (:threshold nrn) (:factors ins))))

(defn generate-neuron
  "Generate neuron with th threshold, n nodes randomized from fmax to fmin."
  [th n fmin fmax]
  (let [range (- fmax fmin)
	facs (take n (repeatedly #(+ (rand range) fmin)))]
    (struct neuron th facs)))

(defn generate-tests
  "Generate n tests for neuron nrn."
  ([n nrn]))

(defn save-neuron&tests
  "Save neuron and tests to f[ile]."
  [nrn tests f])