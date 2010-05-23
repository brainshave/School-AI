(ns sw.net
  (:use (clojure.contrib combinatorics)
	(clojure.contrib pprint))
  (:require (sw [gui :as gui]))
  (:import (java.awt Font)
	   (javax.swing SpinnerNumberModel
			Box BoxLayout)))

(defmacro defkeyfn [n] 
  (let [sname (name n)]
    `(defn ~(-> (str sname "-key") symbol)
       [a#]
       (-> ~sname (str a#) keyword))))
 
(defkeyfn row) ;; funkcja do tworzenia kluczy wiersza
(defkeyfn neuron) ;; funkcja do tworzenia kluczy neuronu w wierszu

(defn new-node
  "wejscie neuronu neuronu"
  [row neuron-num]
  [:spinner {:groups [(row-key row) (neuron-key neuron-num)]
	     :model [SpinnerNumberModel {:params [(rand) -20.0 20.0 0.1] }]}])

(defn new-neuron
  [row number node-amount]
  (concat
   [Box {:params [BoxLayout/X_AXIS]
	 :constraint (str "n" number)}]
   (for [node (range node-amount)]
     (new-node row number))))

(defn new-row
  [row neuron-amount node-amount]
  (concat
   [:tabbed-pane]
   (for [neuron-number (range neuron-amount)]
     (new-neuron row neuron-number node-amount))))

(defkeyfn input)

(defn input-row [inputs]
  (concat
   [Box {:constraint "South"
	 :params [BoxLayout/X_AXIS]}
    [:label {:text "Wejścia: "}]]
   (for [in (range inputs)]
     [:toggle-button {:id (input-key in)
		      :text " "
		      :groups [:inputs]}])))

(defn add-row [e this ids groups & _]
  (let [row-num (->> @groups keys 
		     (map name)
		     (filter #(= (seq "row") (take 3 %)))
		     (map #(->> % (drop 3) (apply str)
				Integer/valueOf))
		     sort last inc)] ; wyciaga ostani numer wiersza
    (-> @ids :neurons-area
	(.add (:root (gui/parse-gui (new-row row-num 10 10)
					ids groups))))
    (-> @ids :scrolling-area .validate)))

(defn fire [e this ids groups & _])

(defkeyfn output)

(defn output-row [outputs]
  (concat
   [Box {:constraint "North"
	 :params [BoxLayout/X_AXIS]}
    [:label {:text "Wyjścia: "}]]
   (for [out (range outputs)]
     [:toggle-button {:id (output-key out)
		      :text " "
		      :groups [:outputs]}])
   [[:button {:text "Dodaj wastwę"
	      :onmcc add-row}]
    [:button {:text "Odpal"
	      :onmcc fire}]]))

(def main-frame
     [:frame {:title "Sieć neuronowa by S/W"
	      :size [600 400]
	      :visible true}
      (output-row 10)
      [:scroll-pane {:id :scrolling-area
		     :params [[Box {:params [BoxLayout/Y_AXIS]
				    :id :neurons-area}
			       (new-row 0 10 10)]]
		     :constraint "Center"}]
      (input-row 10)])