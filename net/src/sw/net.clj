(ns sw.net
  (:use (clojure.contrib combinatorics)
	(clojure.contrib pprint))
  (:require (sw [gui :as gui]))
  (:import (java.awt Font)
	   (javax.swing JSpinner SpinnerNumberModel
			JToggleButton
			Box BoxLayout)))

;; neuron0 to tak naprawde threshold!

(defmacro defkeyfn [n] 
  (let [sname (name n)]
    `(defn ~(-> (str sname "-key") symbol)
       [a#]
       (-> ~sname (str a#) keyword))))
 
(defkeyfn row) ;; funkcja do tworzenia kluczy wiersza
(defkeyfn neuron) ;; funkcja do tworzenia kluczy neuronu w wierszu
(defkeyfn node)

(defn new-node
  "wejscie neuronu"
  []
  [:spinner {:groups []
	     :model [SpinnerNumberModel {:params [(rand 10) -20.0 20.0 0.1] }]}])

(defn new-neuron
  [number node-amount]
  (concat
   [Box {:params [BoxLayout/X_AXIS]
	 :groups [(neuron-key number)]
	 :constraint (str "neuron " number)}
    [:label {:text "Th: "}]
    (new-node)
    [:label {:text "in: "}]]
   (for [node (range (dec node-amount))]
     (new-node))))

(defn new-row
  [row neuron-amount node-amount]
  (concat
   [:tabbed-pane {:id (row-key row)}]
   (for [neuron-number (range neuron-amount)]
     (new-neuron neuron-number node-amount))))

(defn take-keys-prefix [prefix m]
  (->> m keys (map name)
       (filter #(= (seq prefix) (take (count prefix) %)))
       (map str)))

(defn add-row [e this ids groups & _]
  (let [row-num (->> @ids
		     (take-keys-prefix "row")
		     (map #(->> % (drop 3) (apply str)
				Integer/valueOf)) ; obcina row i wyciaga cyfre
		     sort last inc)] ; wyciaga ostani numer wiersza
    (-> @ids :neurons-area
	(.add (:root (gui/parse-gui (new-row row-num 10 11)
				    ids groups))))
    (-> @ids :scrolling-area .validate)))

(defstruct neuron :threshold :factors)


(defn get-neuron [box]
  (let [[threshold & factors]
	(->> box .getComponents
	     (filter #(instance? JSpinner %))
	     (map #(.getValue %)))]
    (struct neuron threshold factors)))

(defn get-row [row]
  (->> row .getComponents (map get-neuron)))

(defn get-net [box]
  (->> box .getComponents (map get-row)))

(defn get-toggle-buttons [box]
  (->> box .getComponents (filter #(instance? JToggleButton %))))

(defn get-inputs [box]
  (->> box get-toggle-buttons
       (map #(-> % .isSelected (if 1 0)))))

(defn get-outputs [box]
  (->> box get-toggle-buttons))

(defn process-neurons [inputs neurons]
  (for [{:keys [threshold factors]} neurons]
    (if (->> (map * factors inputs)
	     (reduce +)
	     (- threshold)
	     (> 0))
      1 0)))    

(defn fire [e this ids groups & _]
  (let [net (-> @ids :neurons-area get-net)
	inputs (-> @ids :input-row get-inputs)
	outputs (-> @ids :output-row get-outputs)]
    (->>
     (reduce process-neurons inputs net)
     (map #(.setSelected %1 (== 1 %2)) outputs)
     doall)))


(defkeyfn input)

(defn input-row [inputs]
  (concat
   [Box {:id :input-row
	 :constraint "North"
	 :params [BoxLayout/X_AXIS]}
    [:label {:text "Wejścia: "}]]
   (for [in (range inputs)]
     [:toggle-button {:text " "
		      :groups [:inputs]}])
   
   [[:button {:text "Odpal"
	      :onmcc fire}]]))

(defkeyfn output)

(defn output-row [outputs]
  (concat
   [Box { :id :output-row
	 :constraint "South"
	 :params [BoxLayout/X_AXIS]}
    [:label {:text "Wyjścia: "}]]
   (for [out (range outputs)]
     [:toggle-button {:text " "
		      :groups [:outputs]}])
   [[:button {:text "Dodaj wastwę"
	      :onmcc add-row}]]))

(def main-frame
     [:frame {:title "Sieć neuronowa by S/W"
	      :size [800 550]
	      :visible true}
      (input-row 10)
      [:scroll-pane {:id :scrolling-area
		     :params [[Box {:params [BoxLayout/Y_AXIS]
				    :id :neurons-area}
			       (new-row 0 10 11)]]
		     :constraint "Center"}]
      (output-row 10)])