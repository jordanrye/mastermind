;; Subject:  	CSCI337 - Organisation of Programming Languages
;; Task:     	Assignment 3 - Common Lisp
;;
;; Author:	Jordan Rye (5061039)
;; Email:	jr656@uowmail.edu.au
;; Created:	29/04/2017
;; Modified:	07/05/2017

;; Splash message
(defun Mastermind ()
		(format t "Welcome to Mastermind (press ENTER to continue): ")
		(setf cont (read-line))
		(Start))

;; Initialise game and print information
(defun Start ()
		(format t "~%How many pegs should I use: ")
		(setf *numPegs* (parse-integer (read-line)))
		(format t "How many colours should I use: ")
		(setf *numColours* (parse-integer (read-line)))
		(format t "~%Instructions:")
		(format t "~%	- Write your guesses in the form (2 2 3 4 1)")
		(format t "~%	- You will receive hints in the form (C - W W C)")
		(format t "~%		- 'C' means the correct peg is in the correct position.")
		(format t "~%		- 'W' means the correct peg is in the wrong position.")
		(format t "~%		- '-' means that the peg is wrong in its entirety.")
		(format t "~%	- Try to break the code in as few guesses as possible!~%")
		(Game *numPegs* *numColours* (setf *totalGuesses* 1) (Solution *numPegs* *numColours*) nil))

;; Main controller
(defun Game (numPegs numColours totalGuesses Solution analysis)
		(format t "~{~A~%~}~%" analysis)
		(let ((guess (input numPegs)))
 				(cond ((equal Solution guess) (format t "Congratulations! You guessed it in ~d goes.~%~%" totalGuesses))
				(t (Game numPegs numColours (+ totalGuesses 1) Solution (cons (cons guess (Comparisons guess Solution)) nil))))))

;; Generate solution
(setf *random-state* (make-random-state t))
(defun Solution (numPegs numColours)
		(loop repeat numPegs collect (+ (random numColours) 1)))

;; Prompt user for guess
(defun Input (numPegs)
		(format t "Enter your guess: ")
		(let ((guess (ignore-errors (read-from-string (read-line)))))
		  	(cond ((not (listp guess)) (progn (format t "Please enter your guess as a list, i.e. '(2 2 3 4)'.~%")
						(Input numPegs)))
				((= numPegs (length guess)) guess) (t (progn (format t "The current difficulty requires you guess ~d pegs.~%" numPegs)
			 			(Input numPegs))))))

;; Generate list of comparisons between guess and solution
(defun Comparisons (guess Solution)
		(maplist #'(lambda (GuessXSol) (Compare (car GuessXSol) (- (length Solution) (length GuessXSol)) Solution)) guess))

;; Compare guess to solution
(defun Compare (Element pos Solution)
		(cond ((eql Element (nth pos Solution)) 'C)	;; Correct colour, correct position
		((member Element Solution) 'W)							;; Correct colour, invalid position
		(t '-)))																		;; Invalid colour
