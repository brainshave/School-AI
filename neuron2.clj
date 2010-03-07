(ns #^{:author "Szymon Witamborski (santamon)"
       :doc "Second, cleaner attempt to neuron program with Swing gui."}
  neuron2
  (:use (clojure.contrib [str-utils2 :only (split-lines)]
			 [duck-streams :only (spit)]))
  (:import (java.awt BorderLayout
		     Dimension
		     Font)
	   (javax.swing JFrame
			JSpinner
			SpinnerNumberModel
			JLabel
			JPanel
			BoxLayout
			JButton
			JTextArea
			JScrollPane)
	   (java.awt.event ActionListener
			   ActionEvent)))

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

(defn str-factors [fac]
  (reduce #(str %1 \newline %2) fac))

(defn str-neuron [nrn]
  (let [th (:threshold nrn)
	factors (:factors nrn)
	n (count factors)
	nums (concat [th n] factors)]
    (reduce #(str %1 \newline %2) nums)))

(defn str-tests [t]
  (reduce #(str %1 \newline %2) (map #(reduce str %) t)))

(defn save-neuron&tests
  "Save neuron and t[ests] to files: f.cfg and f.in"
  [nrn t f]
  (spit (str f ".cfg")
	(str-neuron nrn))
  (spit (str f ".in")
	(str-tests t)))

(defn save-ouput
  "Save output to f.out."
  [o f]
  (spit (str f ".out")
	(reduce str o)))

;; GUI

(defn make-spinner [name val step]
  (let [k (keyword name)
	k-label (keyword (str name "-label"))]
    (sorted-map k-label (JLabel. (str " " name ":"))
		k (let [d (Dimension. 60 30)]
		    (doto (JSpinner. (SpinnerNumberModel. val nil nil step))
		      (.setMinimumSize d)
		      (.setPreferredSize d))))))
(defn make-button [name f]
  (doto (JButton. name)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [#^ActionEvent e]
			(f))))))

(defn make-tarea [] (doto (JTextArea.)
		       (.setFont (Font. Font/MONOSPACED Font/PLAIN 14))))
		    
(defn with-scrollbars [p]
  (let [d (Dimension. 200 200)]
  (doto (JScrollPane. p)
    (.setMinimumSize d)
    (.setPreferredSize d))))

(defn main
  "Main function, if exit? then after closing window application will exit."
  ([exit?]
     (let [act-nrn (atom nil)
	   act-tests (atom nil)
	   spinners (reduce conj (map #(apply make-spinner %)
				      [["threshold" 4 1]
				       ["n" 10 1]
				       ["t" 80 1]
				       ["fmin" -5.0 0.1]
				       ["fmax" 5.0 0.1]]))
     	   data-area (make-tarea)
	   fac-area (make-tarea)
	   run-button (make-button "Run!" #(println "run"))
	   load-button (make-button "Load" #(println "load"))
	   save-button (make-button "Save" #(println "save"))
	   gen-button (make-button "Generuj"
				   (fn []
				     (let [nrn (apply generate-neuron
						      (map #(.getValue (% spinners))
							   [:threshold :n :fmin :fmax]))
					   t (generate-tests
					      nrn (.getValue (:t spinners)))]
				       (.setText fac-area
						 (str-factors (:factors nrn)))
				       (.setText data-area (str-tests t))
				       (reset! act-nrn nrn)
				       (reset! act-tests t))))
	   spinners-panel (doto (JPanel.)
			    (.add run-button)
			    (.add load-button)
			    (.add gen-button))
	   frame (doto (proxy [JFrame] ["ASDF"])
		   (.setDefaultCloseOperation (if exit?
						JFrame/EXIT_ON_CLOSE
						JFrame/DISPOSE_ON_CLOSE))
		   (.setSize 800 600))
	   cpane (doto (.getContentPane frame)
		   (.setLayout (BorderLayout. 5 5))
		   (.add spinners-panel BorderLayout/NORTH)
		   (.add (with-scrollbars fac-area) BorderLayout/WEST)
		   (.add (with-scrollbars data-area) BorderLayout/CENTER))]
       (doseq [s (reverse (vals spinners))]
	 (.add spinners-panel s))
       (.setVisible frame true)))
    ([] (main true)))

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