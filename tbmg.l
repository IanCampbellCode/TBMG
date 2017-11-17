;;File tbmg.l
;;Written by Ian W. Campbell
;;Version 1.00
;;Last Updated 4/4/2017

;;;;;;;;;;;;;;;;;;;Macros;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;This macro is from Paul Graham's On Lisp
;;This allows for recursive lambda functions
;:This is used to make the mutate-measure method cleaner, as well as the crossover-measures method
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
	   #'self))

;;;;;;;;;;;;;;;;Global Variables;;;;;;;;;;;;;;;;;;;;;;;;;

;;This is used in task 1 to convert measures
;;Utilized in convert-note function
(setf *note-map* (list 'a 'b 'c 'd 'e 'f 'g) )

;;This is used in task 5 to compute fitness
;;This could be made localy in fitness-note but it may need tweaking
;;By keeping it here I can easily change it to find what scores I like best
(setf *spread-to-score* '(15 15 10 35 35 10 10))

;;;Used in task 6,7, and 8
;;;The input is used in fitness, so it is only reasonable to set it ahead of time
(setf *input-measure* '(1 2 3 4))

;;;Used in task 7
;;;Top is considered top 50 and bottom is bottom 50
;;;Note, no measure is crossed or mutated twice in one iteration
(setf *maj-mut-rate* '50)		;;Mutate 50 out of the top 150
(setf *bottom-mut-rate* '30)	;;Mutate 30 of the bottom 50
(setf *top-cross-rate* '10)		;;Perform 10 crossovers among top 50
(setf *maj-cross-rate* '30)		;;Perform 30 crossovers on bottom 150

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Basic List Processing Code;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This Code does not work on the genetic algorithm
;;This is just helpful for list manipulation and construction


;;returns list with x as last element
( defun snoc ( x lst )
	( cond
		( ( NULL lst ) ( list x ) )
		( ( singleton-p lst ) ( list ( car lst ) x ) )
		( (cons ( car lst ) ( snoc x (cdr lst) ) ) )
	)
)


;;precondition, x must be an integer greater than 0
;;Returns list of intergers from 1 to n
( defun gen-numbered-list ( n )
  ( cond
    ( ( = n 0 ) NIL)
    ( T ( snoc n ( gen-numbered-list ( - n 1 ) ) ) )
  )
)


;;selects a random element from the given list
( defun pick (lst)
	( cond
		(( NULL lst ) NIL )
		( T ( nth  ( random ( length lst ) ) lst ) )
	)
)

;; returns true when list has length 1
( defun singleton-p ( lst )
	( cond
		( ( NULL lst ) NIL )
		( ( NULL ( cdr lst ) ) T )
		( T NIL)
	)
)


;;precondition, x must be an integer greater than 0
;;Returns list of intergers from 1 to n
( defun gen-numbered-list ( n )
  ( cond
    ( ( = n 0 ) NIL)
    ( T ( snoc n ( gen-numbered-list ( - n 1 ) ) ) )
  )
)

(defun first-n (lst n)
	(cond
		((OR (NULL lst) (= 0 n)) nil)
		( T (cons (car lst) (first-n (cdr lst) (- n 1))))
	)
)

;;returns random permutation of list
( defun random-permutation ( lst &aux element remainder )
  	( cond 
		( ( NULL lst ) NIL )
		( T
			( setf element ( pick lst ) )
			( setf remainder ( remove element lst :count 1 ) )
			( snoc element ( random-permutation remainder ) )
 		)
	)
)

;;Precondition: n is a positive integer
;;returns list of length n comprised of randomly selected ints from 1 to 7
( defun create-random-measure (n)
  (cond
    ( ( = n 0 ) NIL )
    ( T (snoc ( pick ( gen-numbered-list 7 ) ) ( create-random-measure ( - n 1 ) ) ) )
  )
)

;;Precondition: n is an integer between 1 and 7, inclusive
;;Converts a note from its integer representation to its lexical representation
( defun convert-note ( n )
  ( nth (- n 1 ) *note-map* )
)

;;Precondition: measure is a list containing integers between 1 and 7, inclusive
;;Converts a measure from its integer representation to its lexical representation
( defun convert-measure ( measure )
  (cond
    ( ( NULL measure ) nil )
    ( T ( cons (convert-note ( car measure ) ) (convert-measure ( cdr measure ) ) ) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;Task 2;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task involves mutating a measure by change 1 or more notes to any other note


;;Destructive mutation method
;;Recursively changes 1 more notes of a measure. Will never change every note of a measure
(defun mutate-measure (measure &aux builder worker)
	(setf builder (mapcar #'(lambda (x) (- x 1)) (gen-numbered-list  (length measure))))
	(setf worker (first-n (random-permutation builder) (+ (random (-(length measure) 1)) 1 ) ) )
	(funcall 
		(alambda (measure worker)
			(cond
				((NULL worker) measure)
				(T 
					(setf (nth (car worker) measure) (mutate-note (nth (car worker) measure)))
					(self measure (cdr worker))
				)
			)
		)
	measure worker)
)

;;Used by mutate measure
;;Takes an integer from 1 to 7 and returns a different integer from 1-7
(defun mutate-note ( note &aux new-note )
	(setf new-note (+ note (random 7) 1 ) )
	(when (> new-note 7) (setf new-note (- new-note 7) ) )
	new-note
)

;;This method is just for demoing Task 2
;;This is not used in any serious sense at any point
(defun demo-mutate (&aux m1)
	(format t "Beginning demo: assigning value to m1:~%")
	(format t "~s~%"(convert-measure (setf m1 (create-random-measure 8))))
	(format t "Mutating m1:~%")
	(format t "~s~%"(convert-measure (mutate-measure m1)))
	(format t "Mutating m1:~%")
	(format t "~s~%"(convert-measure (mutate-measure m1)))
	(format t "Mutating m1:~%")
	(format t "~s~%"(convert-measure (mutate-measure m1)))
	T
)




;;;;;;;;;;;;;;;;;;;;;;;;Task 3;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task involves crossing over two measures by exchanging 1 or more notes
;;Crossover does not guarentee that the notes are different


;;Destructive crossover method
;;Recursively crossesover 1 more notes between two measures. Will never swap entire measures, as that is redundant
;;Precondition: m1 and m2 must be the same length
;;Note: returns list of crossed m1 and m2 out of symmetry with mutate. This list is not useful and should not be used in most cases
(defun crossover-measures (m1 m2 &aux builder worker)
	(setf builder (mapcar #'(lambda (x) (- x 1)) (gen-numbered-list  (length m1))))
	(setf worker (first-n (random-permutation builder) (+ (random (-(length m1) 1)) 1 ) ) )
	(funcall 
		(alambda (m1 m2 worker)
			(cond
				((NULL worker) nil)
				(T 
					(crossover-notes (car worker) m1 m2)
					(self m1 m2 (cdr worker))
				)
			)
		)
	m1 m2 worker)
	(list m1 m2)
)


;;Destructive Crossover Method
;;Used by crossover-measures
;;Takes an integer and two lists. Swaps the notes in the position n between lists
;;Precondition: m1 and m2 must both be at least length n+1
(defun crossover-notes ( n m1 m2 &aux temp )
	(setf temp (nth n m1))
	(setf (nth n m1) (nth n m2))
	(setf (nth n m2) temp)
	nil
)

;;This method is just for demoing Task 3
;;This is not used in any serious sense at any point
(defun demo-crossover (&aux m1 m2)
	(progn
		(format t "Beginning demo: assigning value to m1:~%")
		(format t "~s~%"(convert-measure (setf m1 (create-random-measure 8))))
		(format t "Beginning demo: assigning value to m2:~%")
		(format t "~s~%"(convert-measure (setf m2 (create-random-measure 8))))
		(format t "Crossing m1 and m2:~%")
		(crossover-measures m1 m2)
		(format t "m1: ~s~%m2: ~s~%"(convert-measure m1)(convert-measure m2))
		(format t "Crossing m1 and m2:~%")
		(crossover-measures m1 m2)
		(format t "m1: ~s~%m2: ~s~%"(convert-measure m1)(convert-measure m2))
		(format t "Crossing m1 and m2:~%")
		(crossover-measures m1 m2)
		(format t "m1: ~s~%m2: ~s~%"(convert-measure m1)(convert-measure m2))
		(format t "Crossing m1 and m2:~%")
		(crossover-measures m1 m2)
		(format t "m1: ~s~%m2: ~s~%"(convert-measure m1)(convert-measure m2))
		(format t "Crossing m1 and m2:~%")
		(crossover-measures m1 m2)
		(format t "m1: ~s~%m2: ~s~%"(convert-measure m1)(convert-measure m2))
		(format t "Crossing m1 and m2:~%")
		(crossover-measures m1 m2)
		(format t "m1: ~s~%m2: ~s~%"(convert-measure m1)(convert-measure m2))
		T
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;Task 4;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task involves playing a measure out loud
;;;This task will be by calling my custom c program. Very complicated. very pretty.
;;;This solution only works on win32 systems and only where Beep in the windows.h file is supported
;;;#don't sweat the small stufff

(defun play-measure (lst)
	(cond
		((NULL lst) nil)
		( T (play-note (car lst)) (play-measure (cdr lst)))
	)
)

(defun play-note (n)
	(ext:run-program 'tbmgsnd.exe :arguments (list (write-to-string n)) )
)

(defun play-a-to-a (&aux n)
	(setf n 1)
	(funcall 
		(alambda (n)
			(cond
				((= n 9) nil)
				(T 
					(ext:run-program 'tbmgsnd.exe :arguments (list (write-to-string n)) )
					(self (+ 1 n))
				)
			)
		)
	n)
)

(defun demo-song-zst (&aux zst)
	(setf zst '(1 5 1 5 1 5 6 1 5 1 4 3))
	(play-measure zst)
)


;;;;;;;;;;;;;;;;;;;;;;;;Task 5;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This is where we introduce our fitness metric

(defun fitness-measure (base variant)
	(apply '+ (mapcar #'fitness-note base variant))
)

(defun fitness-note (base variant)
	(nth ( - (max base variant) (min base variant) ) *spread-to-score*)
)

(defun demo-measure-fitness ()
	(format t "Creating a base measure to work with ~%:")
	(format t "m1: ~s~%" (convert-measure (setf m1 (create-random-measure 4))))
	(format t "any note measured against itself is given a fitness of ~s~%" (nth 0 *spread-to-score*))
	(format t "comparing m1 to itself results in: ~s~%" (fitness-measure m1 m1))
	(format t "Creating random measure:~%")
	(format t "m2: ~S~%" (convert-measure (setf m2 (create-random-measure 4))))
	(format t "m1 compared to m2 results in: ~s~%" (fitness-measure m1 m2))
	T
)



;;;;;;;;;;;;;;;;;;;;;;;;Task 6;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task involves the creation of an individual class
;;;Individuals are just wrappers around measures that also keep track of their fitness


;;The individual class has the following slots: rbg-string, fitness and number
( defclass individual ()
	(
		(measure :accessor individual-measure :initarg :measure)
		( fitness :accessor individual-fitness :initarg :fitness )
	)
)

(defun create-individual (n &optional m)
	(if (NULL m) (setf m (create-random-measure n)))
	(make-instance 'individual
		:measure m
		:fitness (fitness-measure *input-measure* m)
	)
)

(defmethod print-individual ( (i individual) )
	(format t "~s : ~s~%" (convert-measure (individual-measure i)) (individual-fitness i))
)
(defun print-individual-string-return ( (i individual) )
	(format nil "~s : ~s" (convert-measure (individual-measure i)) (individual-fitness i))
)

(defun mutate-individual ( (i individual) )
	(mutate-measure (individual-measure i))
)

(defun crossover-individuals ( (i individual) (j individual) )
	(crossover-measures (individual-measure i) (individual-measure j))
)

(defun update-individual-fitness ( (i individual) )
	(setf (individual-fitness i) (fitness-measure (individual-measure i) *input-measure*))
)

(defun demo-individual (&aux ind1  ind2)
 	(format t "Demoing individual functionality~%")
	(format t "Creating and printing individual~%")
	(setf ind1 (create-individual 4))
	(format t "ind1 : ~s~%" (print-individual-string-return ind1))
	(format t "mutating individual ind1~%")
	(mutate-individual ind1)
	(update-individual-fitness ind1)
	(format t "ind1 : ~s~%" (print-individual-string-return ind1))
	(format t "Creating and printing individual~%")
	(setf ind2 (create-individual 4))
	(format t "ind2 : ~s~%" (print-individual-string-return ind2))
	(format t "Crossing over ind1 and ind2~%")
	(format t "Before crossover:~%")
	(format t "ind1: ~s~%" (print-individual-string-return ind1))
	(format t "ind2: ~s~%" (print-individual-string-return ind2))
	(crossover-individuals ind1 ind2)
	(format t "after crossover:~%")
	(update-individual-fitness ind1)
	(update-individual-fitness ind2)
	(format t "ind1: ~s~%" (print-individual-string-return ind1))
	(format t "ind2: ~s~%" (print-individual-string-return ind2))
)


;;;;;;;;;;;;;;;;;;;;;;;;Task 7;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task involves the creation of an Population
;;;The population class will be used to keep track of our individuals
;;;All the functions applied to our individuals are applied to populations as well

;;The individual class has the following slots: rbg-string, fitness and number
( defclass population ()
	(
		(individuals :accessor population-individuals :initarg :individuals)
		(age :accessor population-age :initarg :age)
	)
)

(defun create-individual-list (n len)
	(cond
		((= n 0) nil)
		(T (cons (create-individual len) ( create-individual-list (- n 1) len)))
	)
)

(defun create-population (len)
	 (sort-population (make-instance 'population
		:individuals (create-individual-list 200 len)
		:age 0
	))
)

(defmethod print-population ( (p population) &aux inds)
	(format t "Printing Individuals in population:~%")
	(print-population-individuals (population-individuals p))
	(print-population-stats p)
)

(defmethod print-population-individuals (individuals)
	(if (NOT (NULL individuals))
		(progn
			(print-individual (car individuals))
			(print-population-individuals (cdr individuals))
		)
	)
)

(defmethod print-population-stats ((p population))
	(format t "Age: ~s~%" (population-age p))
	(format t "Third Quartile fitness: ~s~%" (population-third-quartile-fitness p))
	(format t "Average fitness:        ~s~%" (population-average-fitness p))
	(format t "First Quartile fitness: ~s~%" (population-first-quartile-fitness p))
	(format t "Median Fitness:         ~s~%" (population-median-fitness p))
	(format t "Most fit individual:~%~s~%" (print-individual-string-return(most-fit-individual p)))
	(format t "Least fit individual:~%~s~%" (print-individual-string-return (least-fit-individual p)))
)

(defmethod population-average-fitness ( (p population) &aux inds sum)
	(setf inds (population-individuals p))
	(setf sum ( apply #'+ (mapcar #'individual-fitness inds)))
	(float (/ sum (length inds)))
)

(defmethod population-median-fitness ( (p population) )
	(individual-fitness (nth (multiple-value-bind (q r) (floor (length (population-individuals p)) 2) q) (population-individuals p)))
)

(defmethod most-fit-individual ( (p population) )
	(car (population-individuals p))
)

(defmethod least-fit-individual ( (p population) )
	(nth 199 (population-individuals p))
)

(defmethod population-third-quartile-fitness ( (p population) &aux worker sum inds amended-list)
	(setf worker (mapcar #'(lambda (x) (- x 1)) (gen-numbered-list  100)))
	(setf inds (population-individuals p))
	(setf amended-list 
		(funcall
			(alambda (worker inds)
				(cond
					((NULL worker) nil)
					( T (cons (nth (car worker) inds) (self (cdr worker) inds)))
				)
			)
		worker inds)
	)
	(setf sum ( apply #'+ (mapcar #'individual-fitness amended-list)))
	(float (/ sum 100))
)

(defmethod population-first-quartile-fitness ( (p population) &aux worker sum inds amended-list)
	(setf worker (mapcar #'(lambda (x) (+ x 99)) (gen-numbered-list  100)))
	(setf inds (population-individuals p))
	(setf amended-list 
		(funcall
			(alambda (worker inds)
				(cond
					((NULL worker) nil)
					( T (cons (nth (car worker) inds) (self (cdr worker) inds)))
				)
			)
		worker inds)
	)
	(setf sum ( apply #'+ (mapcar #'individual-fitness amended-list)))
	(float (/ sum 100))
)

;;;This method is destructive
(defmethod sort-population ( (p population) &aux inds-copy)
	(setf inds-copy (population-individuals p))
	(setf (population-individuals p) (sort inds-copy #'> :key #'individual-fitness))
	p
)

(defmethod age-population ( (p population) )
	(setf (population-age p) (+ (population-age p) 1 ) )
	(mutate-population p)
	(crossover-population p)
	(mapcar #'update-individual-fitness (population-individuals p))
	(sort-population p)
)

(defmethod mutate-population ( ( p population ) &aux builder worker)
	;;Does Majority Mutations here
	(setf builder (mapcar #'(lambda (x) (- x 1)) (gen-numbered-list  150)))
	(setf worker (first-n (random-permutation builder) *maj-mut-rate* ))
	(funcall 
		(alambda ( (p population) worker )
			(cond
				((NULL worker) nil)
				(T 
					(mutate-individual (nth (car worker) (population-individuals p)))
					(self p (cdr worker))
				)
			)
		)
	p worker)
	;;Does more aggressive mutations on less fit individuals
	(setf builder (mapcar #'(lambda (x) (+ x 149)) (gen-numbered-list  50)))
	(setf worker (first-n (random-permutation builder) *bottom-mut-rate* ))
	(funcall 
		(alambda ( (p population) worker )
			(cond
				((NULL worker) nil)
				(T 
					(mutate-individual (nth (car worker) (population-individuals p)))
					(self p (cdr worker))
				)
			)
		)
	p worker)
)


(defmethod crossover-population ( ( p population ) &aux builder worker)
	;;Does top crossovers here
	(setf builder (mapcar #'(lambda (x) (- x 1)) (gen-numbered-list  50)))
	(setf worker (first-n (random-permutation builder) (* 2 *top-cross-rate*)))
	(funcall 
		(alambda ( (p population) worker )
			(cond
				((NULL worker) nil)
				(T 
					(crossover-individuals (nth (car worker) (population-individuals p))(nth (car(cdr worker)) (population-individuals p)))
					(self p (cddr worker))
				)
			)
		)
	p worker)
	;;Does majority crossovers here
	(setf builder (mapcar #'(lambda (x) (+ x 49)) (gen-numbered-list  150)))
	(setf worker (first-n (random-permutation builder) (* 2 *maj-cross-rate*)))
	(funcall 
		(alambda ( (p population) worker )
			(cond
				((NULL worker) nil)
				(T 
					(crossover-individuals (nth (car worker) (population-individuals p))(nth (car(cdr worker)) (population-individuals p)))
					(self p (cddr worker))
				)
			)
		)
	p worker)
)

(defun update-population-fitness ((p population))
	(mapcar #'update-individual-fitness (population-individuals p))
	(sort-population p)
	nil
)

(defun population-demo (&aux pop)
	(format t "Creating random population with measure length 4~%")
	(setf pop (create-population 4))
	(format t "printing population:~%")
	(print-population pop)
	(format t "performing mutations on population 10 times~%")
	(dotimes (count 10)(mutate-population pop))
	(update-population-fitness pop)
	(format t "printing mutated population~%")
	(print-population pop)
	(format t "performing crossovers on population 10 times~%")
	(dotimes (count 10)(crossover-population pop))
	(update-population-fitness pop)
	(format t "printing crossed population~%")
	(print-population pop)
)

;;;;;;;;;;;;;;;;;;;;;;;;Task 8;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task involves moving between generations and gathering stats to use later
;;;As well as printing out different stats

(defun reverse-note-map (note)
	(cond
		((eq note 'A) 1)
		((eq note 'B) 2)
		((eq note 'C) 3)
		((eq note 'D) 4)
		((eq note 'E) 5)
		((eq note 'F) 6)
		((eq note 'G) 7)
	)
)

(defun letters-to-numbers (lst)
	(cond
		((NULL lst) nil)
		(T (cons (reverse-note-map (car lst)) (letters-to-numbers (cdr lst))))
	)
)

(defun tbmg-core (lst &optional m &aux pop initpop)
	(setf *user-input* lst)
	(setf *input-measure* (letters-to-numbers *user-input*))
	(setf pop (create-population (length *user-input*)))
	(setf initpop (deep-copy-population pop))
	(format t "Printing initial population:~%")
	(print-population pop)
	(funcall
		(alambda ((pop population) n)
			(cond
				((NOT(= 0 n))
					(age-population pop)
					(if (= 0 (mod (population-age pop) 25)) (print-population pop))
					(self pop (- n 1))
				)
			)
		)
	pop 200)
	(format t "~%~%~%Initial Stats:~%")
	(print-population-stats initpop)
	(format t "~%~%~%Final Stats:~%")
	(print-population-stats pop)
	(if (NULL m ) (list initpop pop))
)

(defun deep-copy-population ((p population) &aux newpop)
	(setf newpop (create-population 1))
	(setf (population-individuals newpop) '())
	(funcall
		(alambda (individuals newpop)
			(cond
				((NOT(NULL individuals))
					(setf (population-individuals newpop) (snoc (create-individual (length (individual-measure (car individuals))) (copy-list (individual-measure (car individuals))))(population-individuals newpop) ))
					(self (cdr individuals) newpop)
				)
			)
		)
	(population-individuals p) newpop)
	(mapcar #'update-individual-fitness (population-individuals newpop))
	(sort-population newpop)
	newpop
)

(defun tbmg-core-demo (&aux base-measue)
	(format t "Creating random measure to use as input~%")
	(format t "~s~%" (setf base-measure (convert-measure (create-random-measure 9))))
	(read-line)
	(tbmg-core base-measure 'T)
)


;;;;;;;;;;;;;;;;;;;;;;;;Task 9;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task composes a song from a population.
;;;The song utilizes a fixed super structure as follows:
;;;[user input measure] [most fit measure] [least fit measure] [most fit measure reversed] [user input measure reversed]
;;;I've chosen to treat a song as one long measure for the sake of making playing and writing it easy

(defun compose-song ((p population))
	(append *input-measure* (individual-measure (most-fit-individual p)) (individual-measure (least-fit-individual p)) (reverse (individual-measure (most-fit-individual p))) (reverse *input-measure*)) 
)

;;This is a trivial method, but having it helps conceptually as a song is equivalent to a measure
(defun print-song (lst)
	(convert-measure lst)
)

;;This is a trivial method, but having it helps conceptually as a song is equivalent to a measure
(defun play-song (lst)
	(play-measure lst)
)

(defun compose-song-demo (&aux lst song)
	(format t "Using the following as input: (F B C D)~%")
	(format t "Press enter to play input song~%")
	(read-line)
	(play-song '(7 2 3 4))
	(format t "Press enter to run genetic algorithm~%")
	(read-line)
	(setf lst (tbmg-core '(7 2 3 4)))
	(format t "Composing song from algorithm:~%")
	(setf song (compose-song (cadr lst)))
	(format t "~s~%" (print-song song))
	(format t "Press enter to play song~%")
	(read-line)
	(play-song song)
)


;;;;;;;;;;;;;;;;;;;;;;;;Task 10;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This task runs the genetic algorithm as far as the user is concerned
;;;This task also saves stats regarding the last run of the program
;;;This task also allows the user to play their own song and replay songs from an old stat file

(defun tbmg(&aux populations input song)
	(format t "Please enter your base measure~%")
	(format t "Enter input as letters A through F, with a space between each letter~%")
	(setf input (read-from-string (concatenate 'string "(" (read-line) ")")))
	(format t "Beginning genetic algorithm~%")
	(setf populations (tbmg-core input))
	(setf song (compose-song (cadr populations)))
	(format t "The song has been composed:~%")
	(format t "~s~%" (print-song song))
	(format t "Press enter to play song~%")
	(read-line)
	(play-song song)
	(format t "Would you like to save the stats from this run? y or n~%")
	(setf input (read-line))
	(if (string-equal input "y") 
		(progn
			(format t "Please enter file name, including extension:~%")
			(setf input (read-line))
			(format t "Writing to file...~%")
			(write-to-file input populations song)
			(format t "Save Complete~%")
		)
	)
	T
)



(defun write-to-file ((name String) populations song)
	(with-open-file (*standard-output* name
        :direction :output
        :if-exists :append
        :if-does-not-exist :create)
		(format t "~s~%" (print-song song))
		(format t "Initial user input:~%~s~%" *user-input*)
		(format t "Initial Population:~%" )
		(print-population (car populations))
		(format t "~%~%~%Final Population:~%" )
		(print-population (cadr populations))
		(format t "~%end of stats")
  )
)


#||

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Associated C Source Code;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This is C code, meant to be compiled with
;;gcc -std=c99 -o tbgmsnd.exe
;;This code is not portable and only runs on windows computers

#include <windows.h>

int main(int argc, char *argv[])
{
	char c = argv[1][0];
	int n = c - '0';
	int l = 500;
	if(n == 1){
		//A4
		Beep(440, l);
	} else if (n == 2){
		//B4
		Beep(493.88, l);
	} else if (n == 3){
		//C5
		Beep(523.25, l);
	} else if (n == 4){
		//D5
		Beep(587.33, l);
	} else if (n == 5){
		//E5
		Beep(659.25, l);
	} else if (n == 6){
		//F5
		Beep(698.46, l);
	} else if (n == 7){
		//G5
		Beep(783.99, l);
	} else if(n == 8) {
		//A5
		Beep(880,500);
	}else if(n == 0) {
		//Rest
		Sleep(500);
	}
return 0;
}

||#
