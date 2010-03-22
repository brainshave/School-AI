(ns #^{:author "Szymon Witamborski"
       :doc "Neurotic Network"}
  neuronet
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
()

;; proste makro zwracajace funkcje ze stringa
(defmacro callOn [s]
  `(fn [x#] (~(symbol s) x#)))


(defmacro method-str [s]
  `(fn [obj# param#] (~(symbol s) obj# param#) obj#))
(defn method-str2 [s]
  (fn [obj param] ((resolve (symbol s))
		   obj param)))
(defn setter [p]
  (let [cs (rest (seq (str p)))
	fletter (Character/toUpperCase (first cs))
	s (reduce str  (list ".set" fletter (reduce str (rest cs))))]
    (method-str s)))