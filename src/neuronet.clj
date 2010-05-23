(ns #^{:author "Szymon Witamborski"
       :doc "Neurotic Network"}
  neuronet
  (:use (perceptron))
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

