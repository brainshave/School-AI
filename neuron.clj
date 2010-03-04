(require 'clojure.contrib.string)

(defstruct neuron :threshold :factors)

(defn read-config [config-file-name]
  (let [nums (map #(Double/valueOf %)
		  (clojure.contrib.string/split-lines (slurp config-file-name)))]
    (struct neuron
	    (second nums)
	    (take (int (first nums)) (rest (rest nums))))))

(defn read-input [file-name]
  (for [line (clojure.contrib.string/split-lines (slurp file-name))]
    (map #(Integer/valueOf (str %)) line)))

(defn write-file [file-name content]
  (with-open [file-out (new java.io.FileWriter file-name)]
    (.write file-out
	    (with-out-str (print content)))))

(defn generate-input [colnum rownum]
  (take rownum (repeatedly (fn [] (take colnum (repeatedly #(rand-int 2)))))))

(defn lists2d-to-string [lst]
  (reduce #(str %1 \newline %2) (map #(reduce str %) lst)))

(defn write-num-lists [file-name lists]
  (write-file file-name (lists2d-to-string lists)))

(defn generate-neuron-cfg
  [name threshold & factors]
  (write-file (str "neuron-" name ".cfg")
	      (reduce #(str %1 \newline %2)
		      (str (count factors) \newline threshold)
		      factors))
  (struct neuron threshold factors))


(defn generate-tests [file-name rows cols]
  (write-num-lists file-name (generate-input cols rows)))

(defn generate-neuron-rand
  [name threshold num num-tests fac-min fac-max]
  (generate-tests (str "neuron-" name ".in") num num-tests)
  (apply generate-neuron-cfg
	 name threshold (take num (repeatedly #(let [fac-range (- fac-max fac-min)]
				       (+ (rand fac-range) fac-min))))))

(defn calc-input-node [factor node-nums]
  (map #(* factor %) node-nums))

(defn calc-nodes [factors nodes]
  (map calc-input-node factors nodes))

(defn calc-outputs [threshold calced-nodes]
  (loop [cols calced-nodes, sums []]
    (if (not-any? empty? cols)
      (recur (map rest cols) (conj sums (reduce + (- threshold) (map first cols))))
      sums)))

(defn calc-1 [col]
  (map #(if (< % 0) 0 1) col))

(defn calc-neuron-outputs [name]
  (let [neu (read-config (str "neuron-" name ".cfg"))
	input (read-input (str "neuron-" name ".in"))
	output (reduce str (calc-1 (calc-outputs (:threshold neu)
						 (calc-nodes (:factors neu) input))))]
    (write-file (str "neuron-" name ".out")
		(reduce str output))
    output))
