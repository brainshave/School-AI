(ns
    #^{:author "Szymon Witamborski (santamon)"
       :doc "Second, cleaner attempt to neuron program with gui."}
  neuron2
  (:use (clojure.contrib [str-utils2 :only (split-lines)]
			 [duck-streams :only (spit)])))

(defstruct neuron :threshold :factors)

;; READING & PROCESSING

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
  ([th facs ins]
     (let [-th (- th)]
       (loop
	   [; Multiplies every row by its factor:
	    m-ins (map (fn [fac in] (map #(* fac %) in))
		       facs ins)
	    results []] 
	 (if (not-any? empty? m-ins)
	   (recur (map rest m-ins)
		  (conj results
			; if sum of a row -threshold
			; is >= 0 then output gives 1
			(if (>= (reduce + -th (map first m-ins))
			       0)
			  1 0)))
	   results))))
  ([nrn ins] (process-inputs (:threshold nrn) (:factors nrn) ins)))

;; GENERATION

(defn generate-neuron
  "Generate neuron with th threshold, n nodes randomized from fmax to fmin."
  [th n fmin fmax]
  (let [range (- fmax fmin)
	facs (take n (repeatedly #(+ (rand range) fmin)))]
    (struct neuron th facs)))

(defn generate-tests
  "Generate n tests for neuron nrn."
  [nrn n]
  (take (count (:factors nrn))
	(repeatedly
	 (fn [] (take n (repeatedly
			 #(rand-int 2)))))))

;; SAVING

(defn save-neuron&tests
  "Save neuron and t[ests] to files: f.cfg and f.in"
  [nrn t f]
  (let [th (:threshold nrn)
	factors (:factors nrn)
	n (count factors)
	nums (concat [th n] factors)]
    (spit (str f ".cfg")
	  (reduce #(str %1 \newline %2) nums)))
  (spit (str f ".in")
	(reduce #(str %1 \newline %2) (map #(reduce str %) t))))

(defn save-ouput
  "Save output to f.out."
  [o f]
  (spit (str f ".out")
	(reduce str o)))

;; GUI

(defn main []
  (println "main")
  (doseq [arg *command-line-args*]
    (println arg)))

;; running:
;; java -cp "C:/Java/Clojure/clojure.jar;C:/Java/Clojure-Contrib/clojure-contrib.jar;." clojure.main -e "(require 'neuron2)(neuron2/main)" neuron2.clj arg1 arg2 arg3

;; TESTS

(defn test-generation-and-reading
  "Test generation, saving and reading of .cfg and .in
   returns [cfg-write&read-equal? in-write&read-equal?]"
  []
  (let [f "asdf-qwer"
	nrn (generate-neuron 3 50 -100 100)
	t (generate-tests nrn 100)]
    (save-neuron&tests nrn t f)
    [(= nrn (read-neuron (str f ".cfg")))
     (= t (read-inputs (str f ".in")))]))