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
			JScrollPane
			JTextField
			SwingConstants
			JSeparator)
	   (java.awt.event ActionListener
			   ActionEvent)))

(defstruct neuron :threshold :factors)

;; READING & PROCESSING

(defn read-neuron-str
  [s]
  (struct neuron nil (map #(Double/valueOf %)
			  (split-lines s))))

(defn read-neuron
  "Reads neuron spec from file f"
  [f]
  (let [nums (map #(Double/valueOf %)
		  (split-lines (slurp f)))
	th (first nums) ; first line is threshold
	n (second nums) ; second line is nodes number
	facs (take n (drop 2 nums))] ; rest lines are factors
    (struct neuron th facs)))

(defn read-inputs-str
  [s]
  (for [line (split-lines s)]
    (map #(Integer/valueOf(str %)) line)))

(defn read-inputs
  "Reads inputs from file"
  [f] (read-inputs-str (slurp f)))

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
			(comment if sum of a row -threshold
				 is >= 0 then output gives 1)
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

(defn save-output
  "Save output to f.out."
  [o f]
  (spit (str f ".out")
	(reduce str o)))

;; GUI

(defn make-spinner
  ([name val step tooltip]
     (let [k (keyword name)
	   k-label (keyword (str name "-label"))]
       (sorted-map k-label (JLabel. (str " " name ":"))
		   k (let [d (Dimension. 60 30)]
		       (doto (JSpinner. (SpinnerNumberModel. val nil nil step))
			 (.setMinimumSize d)
			 (.setPreferredSize d)
			 (.setToolTipText tooltip))))))
  ([name val step] (make-spinner name val step nil)))

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
     (try 
      (doseq [laf (javax.swing.UIManager/getInstalledLookAndFeels)]
	(when (= (.getName laf) "Nimbus")
	  (javax.swing.UIManager/setLookAndFeel (.getClassName laf))))
      (catch Exception e))
     (let [spinners (reduce conj (map #(apply make-spinner %)
				      [["threshold" 4 1]
				       ["n" 10 1 "number of input nodes"]
				       ["t" 80 1 "number of tests"]
				       ["fmin" -5.0 0.1 "mininum node factor"]
				       ["fmax" 5.0 0.1 "maximum node factor"]]))
	   data-area (make-tarea)
	   fac-area (make-tarea)
	   file-field (JTextField. "neuron" 5)
	   buttons [(make-button "|" (fn []
				       (let [t (.getText data-area)
					     conv-t
					     (reduce str (map #(condp = %
								 \0 \space,
								 \1 \|,
								 %) t))]
					 (.setText data-area conv-t))))
		    (make-button "01" (fn []
					(let [t (.getText data-area)
					      conv-t
					      (reduce str (map #(condp = %
								  \space \0,
								  \| \1,
								  %) t))]
					  (.setText data-area conv-t))))
		    (make-button "Run!" (fn []
					  (let [fac (:factors (read-neuron-str
							       (.getText fac-area)))
						th (.getValue (:threshold spinners))
						t (read-inputs-str
						   (.getText data-area))]
					    (.append data-area
						     (reduce str \newline
							     (process-inputs th fac t))))))
		    (make-button "Save" (fn []
					  (let [nrn (assoc (read-neuron-str
							    (.getText fac-area))
						      :threshold (.getValue (:threshold spinners)))
						nums (read-inputs-str
						      (.getText data-area))
						c (count (:factors nrn))
						ins (take c nums)
						out (if (> (count nums) c)
						      (last nums))
						name (.getText file-field)]
					    (save-neuron&tests nrn ins name)
					    (if out (save-output out name)))))
		    (make-button "Load" #(println "load"))
		    file-field
		    (JLabel. "{.cfg,.in} ")
		    (make-button "Generate"
				 (fn []
				   (let [nrn (apply generate-neuron
						    (map #(.getValue (% spinners))
							 [:threshold :n :fmin :fmax]))
					 t (generate-tests
					    nrn (.getValue (:t spinners)))]
				     (.setText fac-area
					       (str-factors (:factors nrn)))
				     (.setText data-area (str-tests t)))))]
	   
	   spinners-panel (JPanel.)
	   frame (doto (proxy [JFrame] ["Neuron Model by Szymon Witaborski"])
		   (.setDefaultCloseOperation (if exit?
						JFrame/EXIT_ON_CLOSE
						JFrame/DISPOSE_ON_CLOSE))
		   (.setSize 1000 500))
	   cpane (doto (.getContentPane frame)
		   (.setLayout (BorderLayout. 5 5))
		   (.add spinners-panel BorderLayout/NORTH)
		   (.add (with-scrollbars fac-area) BorderLayout/WEST)
		   (.add (with-scrollbars data-area) BorderLayout/CENTER))]
       (doseq [s (concat buttons (reverse (vals spinners)))]
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