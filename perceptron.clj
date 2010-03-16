(ns #^{:author "Szymon Witamborski (santamon)"
       :doc "Perceptron program"}
  perceptron
  (:use (clojure.contrib [string :only (split-lines split)]
			 [io :only (spit)]))
  (:import (java.awt BorderLayout
		     Dimension
		     Font)
	   (java.awt.event ActionListener
			   ActionEvent)
	   (javax.swing JFrame
			JToolBar
			JLabel
			JPanel
			BoxLayout
			JButton
			JTextArea
			JScrollPane
			JSplitPane
			SwingConstants
			JOptionPane
			JDialog
			JFileChooser
			JSpinner
			SpinnerNumberModel)
	   (javax.swing.text JTextComponent)))

(defstruct neuron :threshold :factors)

;; READING AND SAVING, CONVERSION STRING <=> NEURONS
(defn read-neuron
  "Read neuron from line"
  [line]
  (let [nums (map #(Double/valueOf %) (split #"\s+" line))]
    (struct neuron (first nums) (rest nums))))

(defn read-neurons
  "Read neurons, each spec in separate line"
  [s]
  (map read-neuron (split-lines s)))

(defn slurp-neurons
  "Read neurons from file"
  [f]
  (read-neurons (slurp f)))

(defn str-neuron
  "Convert neuron spec to string"
  [nrn]
  (reduce #(str %1 \space %2) (:threshold nrn) (:factors nrn)))

(defn str-neurons
  "Convert neurons to string, each spec in separate line"
  [nrns]
  (reduce #(str %1 \newline %2) (map str-neuron nrns)))

(defn spit-neurons
  "Spit neurons to file"
  [f nrns]
  (spit f (str-neurons nrns)))

(defn read-tests
  "Read tests from string. Inputs for each node in separate line"
  [s]
  (for [line (split-lines s)]
    (map #(if (or (= \0 %) (= \space %))
	    0 1) line)))

(defn str-tests
  "Make string of t. Inputs for each node in separate line.
Can be used for any two-dimentional collection."
  [t]
  (apply str (interleave (map #(apply str %) t)
			 (repeat \newline))))

(defn spit-tests
  [f t]
  (spit f (str-tests t)))

(defn toggle-01-pipe
  "Changes 0 to space and 1 to | and vice-versa"
  [s]
  (map #(condp = %
	  \space \0
	  \0 \space
	  \| \1
	  \1 \|
	  %)
       s))

;; PROCESSING TESTS
(defn process-neuron
  "Processes inputs ins for neuron nrn or for fac[tor]s and th[reshold] given separetly"
  ([th facs ins]
     (let [-th (- th)]
       (loop [; Multiplies every row by its factor:
	      m-ins (map (fn [fac in] (map #(* fac %) in))
			 facs ins)
	      results []]
	 (if (not-any? empty? m-ins)
	   (recur (map rest m-ins)
		  (conj results
			; if sum of a row -threshold is >= 0 then output gives 1
			(if (>= (reduce + -th (map first m-ins))
				0)
			  1 0)))
	   results))))
  ([nrn ins] (process-inputs (:threshold nrn) (:factors nrn) ins)))

(defn process-neurons
  [nrns t]
  (map #(process-neuron % t) nrns))

;; GENRATING NEURONS
(defn gen-neuron
  "Generate neuron with th threshold, n nodes randomized from fmax to fmin."
  [th n fmin fmax]
  (let [range (- fmax fmin)
	facs (take n (repeatedly #(+ (rand range) fmin)))]
    (struct neuron th facs)))

(defn gen-neurons
  "Generate k neurons like in gen-neuron"
  [k th n fmin fmax]
  (take k (repeatedly #(gen-neuron th n fmin fmax))))

(defn gen-tests
  "Generate n tests for neuron nrn."
  [nrn n]
  (take (count (:factors nrn))
	(repeatedly
	 (fn [] (take n (repeatedly
			 #(rand-int 2)))))))

;; GUI
(defn make-spinner
  "Create spinner with label, return map with key (keyword name) for spinner
  and key (keyword (str name \"-label\")) for label)
  Spinner gets model made of val and stepSize step without max and min"
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

(defn make-spinners []
  (reduce conj (map #(apply make-spinner %)
		    [["threshold" 1 1]
		     ["nrns" 3 1 "number of neurons"]
		     ["n" 10 1 "number of input nodes"]
		     ["t" 80 1 "number of tests"]
		     ["fmin" -2.0 0.1 "mininum node factor"]
		     ["fmax" 2.0 0.1 "maximum node factor"]])))

(defn make-button
  "Create button with label name and function f called upon click"
  [name f]
  (doto (JButton. name)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [#^ActionEvent e]
			(f))))))

(defn make-buttons
  [& kvals]
  (map #(apply make-button %) (partition 2 kvals)))

(defn make-tarea
  "Create JTextArea with monospaced font"
  []
  (doto (JTextArea.)
    (.setFont (Font. Font/MONOSPACED Font/PLAIN 14))))
  
(defn with-scrollbars
  "Wraps panel with scrollbars, sets minimum size to 200x200"
  [p]
  (let [d (Dimension. 200 200)]
    (doto (JScrollPane. p)
      (.setMinimumSize d)
      (.setPreferredSize d))))

(defn pipe-fn
  "Create function that takes text from in, decodes it,
calculates f for that value and puts in out
Every paramater can be nil"
  [#^JTextComponent in decoder f encoder #^JTextComponent out]
  (let [dummy-f (fn [x] (if x x (fn [a] a)))]
    (fn []
      (let [in-result (if in
			(f ((dummy-f decoder) (.getText in)))
			(f))]
	(if out (.setText out ((dummy-f encoder) in-result)))))))

(defn pipe-map-fn
  [#^JTextComponent out encoder f & dec-ins]
  (pipe-fn nil nil
	   (fn [] (apply f (map (fn [[decoder in]] (decoder (.getText in)))
				(partition 2 dec-ins))))
	   encoder out))

(defn pipe-fn-multi
  [f & areas]
  (fn [] (doseq [a areas]
	   ((pipe-fn a nil f #(apply str %) a)))))

(defn make-split
  [vertical? & areas]
  (let [sp (doto (JSplitPane. (if vertical?
				JSplitPane/VERTICAL_SPLIT
				JSplitPane/HORIZONTAL_SPLIT))
	     (.setResizeWeight (float (/ 1 (count areas)))))]
    (dorun (map #(.add sp (with-scrollbars %)) areas))
    sp))

(defn make-gen-fn
  [spinners nrns-area test-area]
  (fn [] (let [specs (map #(.getValue (% spinners)) [:threshold :n :fmin :fmax])
	       nrns (apply gen-neurons (.getValue (:nrns spinners)) specs)
	       t (gen-tests (first nrns)
			    (.getValue (:t spinners)))]
	   (.setText nrns-area (str-neurons nrns))
	   (.setText test-area (str-tests t)))))

(def chooser (JFileChooser.))

(defn choose-file
  [parent open?]
  (let [;;chooser (JFileChooser.)
	status (if open?
		 (.showOpenDialog chooser parent)
		 (.showSaveDialog chooser parent))]
    (if (= status JFileChooser/APPROVE_OPTION)
      (.getAbsolutePath (.getSelectedFile chooser)))))

(defn make-open-fn
  [parent tarea]
  #(if-let [fpath (choose-file parent true)]
     (.setText tarea (slurp fpath))))

(defn make-save-fn
  [parent tarea]
  #(if-let [fpath (choose-file parent false)]
     (spit fpath (.getText tarea))))

(defn main
  "Main function, if exit? then after closing window application will exit."
  ([exit?]
     (try 
      (doseq [laf (javax.swing.UIManager/getInstalledLookAndFeels)]
	(when (= (.getName laf) "Nimbus")
	  (javax.swing.UIManager/setLookAndFeel (.getClassName laf))))
      (catch Exception e))
     (let [frame (doto (JFrame. "Perceptron by Szymon Witaborski")
		   (.setDefaultCloseOperation (if exit?
						JFrame/EXIT_ON_CLOSE
						JFrame/DISPOSE_ON_CLOSE))
		   (.setSize 800 600))
	   nrns-area (make-tarea)
	   test-area (make-tarea)
	   result-area (make-tarea)
	   test&result-split (make-split true test-area result-area)
	   main-split (make-split false nrns-area test&result-split)
	   spinners (make-spinners)
	   buttons (make-buttons "Run" (pipe-map-fn result-area str-tests process-neurons
						    read-neurons nrns-area
						    read-tests test-area)
				 "Gen" (make-gen-fn spinners nrns-area test-area)
				 "Save Spec" (make-save-fn frame nrns-area)
				 "Load Spec" (make-open-fn frame nrns-area)
				 "Save Test" (make-save-fn frame test-area)
				 "Load Test" (make-open-fn frame test-area)
				 "|01" (pipe-fn-multi toggle-01-pipe test-area result-area))
	   toolbar (doto (JToolBar.)
		     (.setFloatable false))
	   cpane (doto (.getContentPane frame)
		   (.setLayout (BorderLayout. 0 0))
		   (.add toolbar BorderLayout/NORTH)
		   (.add main-split BorderLayout/CENTER))]
       (dorun (map #(.add toolbar %)
		   (concat buttons (reverse (vals spinners)))))
       (.setVisible frame true)))
  ([] (main true)))
	   
	   