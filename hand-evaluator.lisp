;;;; hand-evaluator.lisp

(in-package #:hand-evaluator)

(defun best-poker-hand (raw-cards)
  "Accepts 1 or more cards (which should be two character strings,
e.g., \"As\") and identifies the best poker hand among them. Returns a
list of the value of the hand, the hand name, and the cards making the
hand."
  (cond
    ((null raw-cards) nil)
    ((> (length raw-cards) 5)
     (let ((source (combinations 5 raw-cards))
	   (res nil))
      (dolist (hand source)
	(push (append (eval-hand hand) (list hand)) res))
      (car (sort res #'> :key #'(lambda (x) (car x))))))
    (t (append (eval-hand raw-cards) (list raw-cards)))))

(defun eval-hand (hand)
  (cond
    ((straight-flush-p hand) (list (straight-flush-rank hand) 'straight-flush))
    ((quads-p hand) (list (quads-rank (pack-by-rank hand)) 'quads))
    ((house-p hand) (list (house-rank hand) 'house))
    ((flush-p hand) (list (flush-rank hand) 'flush))
    ((straight-p hand) (list (straight-rank hand) 'straight))
    ((trips-p hand) (list (trips-rank hand) 'trips))
    ((two-pair-p hand)
     (list (two-pair-rank hand) 'two-pair))
    ((pair-p hand)
     (list (pair-rank hand) 'pair))
    (t (list (high-card-rank hand) 'high-card))))

;;;; hand tests

(defun straight-p (cards)
  (let ((sorted (sort (mapcar #'card-rank-values cards) #'<)))
    (cond
      ((equal sorted '(2 3 4 5 14)) t)
      ((equal sorted '(2 3 4 5 6)) t)
      ((equal sorted '(3 4 5 6 7)) t)
      ((equal sorted '(4 5 6 7 8)) t)
      ((equal sorted '(5 6 7 8 9)) t)
      ((equal sorted '(6 7 8 9 10)) t)
      ((equal sorted '(7 8 9 10 11)) t)
      ((equal sorted '(8 9 10 11 12)) t)
      ((equal sorted '(9 10 11 12 13)) t)
      ((equal sorted '(10 11 12 13 14)) t))))

(defun flush-p (cards)
  "If there are 5 cards, all of the same suit, return t; else nil"
  (let* ((suits (mapcar #'(lambda (x) (subseq x 1 2)) cards))
	 (tar (first suits))
	 (res t))
    (when (= (length suits) 5)
      (dolist (suit suits)
	(when (not (string= suit tar))
	  (setf res nil)))
      res)))

(defun straight-flush-p (cards)
  (when (and (straight-p cards) (flush-p cards)) t))

(defun quads-p (cards)
  (let* ((packed (pack-by-rank cards))
	(lists (remove-if-not #'listp packed)))
    (when (and (= 1 (length lists)) (= 4 (length (car lists))))
      t)))

(defun house-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= 2 (length lists))
	       (or (= 3 (length (car lists)))
		   (= 3 (length (car (last lists)))))
	       (or (= 2 (length (car lists)))
		   (= 2 (length (car (last lists))))))
      t)))

(defun trips-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= 1 (length lists))
	       (= 3 (length (car lists))))
      t)))

(defun two-pair-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= (length lists) 2)
	       (= (length (car lists)) 2)
	       (= (length (car (last lists))) 2))
      t)))

(defun pair-p (cards)
  (let* ((packed (pack-by-rank cards))
	 (lists (remove-if-not #'listp packed)))
    (when (and (= (length lists) 1)
	       (= (length (car lists)) 2))
      t)))

;;;; rankers

(defun high-card-rank (hand)
  "Return the high-card-rank of a given hand."
  (car (card-rank-values hand)))

(defun get-kickers-value (pair kickers kicker-type)
  (let ((val (position
		  (sort (mapcar #'card-rank-values kickers) #'>)
		  (remove-kickers (card-rank-values (car pair)) kicker-type)
		  :test #'equal)))
    (if val val 0)))

(defun pair-rank (hand)
  (let* ((packed (pack-by-rank hand))
	 (pair (car (remove-if-not #'listp packed)))
	 (kickers (remove-if #'listp packed)))
    (if kickers
	(progn
	  (+ (get-pair-value (card-rank-values (car pair)))
	     (1+ (get-kickers-value pair kickers *pair-kickers*))))
	(progn
	  (get-pair-value (card-rank-values (car pair)))))))

(defun two-pair-rank (cards)
  (let ((packed (sort-packed (pack-by-rank cards))))
    (if (= (length packed) 3)
	(progn
	  (+ (get-two-pair-values
	      (card-rank-values (car (nth 0 packed)))
	      (card-rank-values (car (nth 1 packed))))
	     (card-rank-values (car (last packed)))))
	(progn
	  (get-two-pair-values
	   (card-rank-values (car (nth 0 packed)))
	   (card-rank-values (car (nth 1 packed))))))))

(defun trips-rank (cards)
  (let* ((packed (pack-by-rank cards))
	 (trips (car
		 (remove-if
		  #'(lambda (x) (not (and (listp x) (= (length x) 3))))
		  packed)))
	 (kickers (remove-if #'listp packed)))
    (if (= (length kickers) 2)
	(progn
	  (+ (get-trips-value (card-rank-values (car trips)))
	     (1+ (position
		  (sort (mapcar #'card-rank-values kickers) #'>)
		  (remove-kickers (card-rank-values (car trips)) *trips-kickers*)
		  :test #'equal))))
	(progn
	  (get-trips-value (card-rank-values (car trips)))))))

(defun straight-rank (cards)
  (let ((sorted (sort (mapcar #'card-rank-values
			      cards) #'<)))
    (cond
      ((equal sorted '(2 3 4 5 14)) 4496)
      ((equal sorted '(2 3 4 5 6)) 4497)
      ((equal sorted '(3 4 5 6 7)) 4498)
      ((equal sorted '(4 5 6 7 8)) 4499)
      ((equal sorted '(5 6 7 8 9)) 4500)
      ((equal sorted '(6 7 8 9 10)) 4501)
      ((equal sorted '(7 8 9 10 11)) 4502)
      ((equal sorted '(8 9 10 11 12)) 4503)
      ((equal sorted '(9 10 11 12 13)) 4504)
      ((equal sorted '(10 11 12 13 14)) 4505))))

(defun flush-rank (flush)
  (+ 4506 (car (sort (mapcar #'card-rank-values flush) #'>))))

(defun house-rank (hand)
  (let ((ahand (sort-packed (pack-by-rank hand))))
    (labels ((get-rank (trip-rank pair-rank)
	       (cond
		 ((and (= trip-rank 2) (= pair-rank 3)) 4521)
		 ((> pair-rank 2) (1+ (get-rank trip-rank (1- pair-rank))))
		 ((= pair-rank 2) (1+ (get-rank (1- trip-rank) 14))))))
      (get-rank
       (card-rank-values (caar ahand))
       (card-rank-values (car (cadr ahand)))))))

(defun quads-rank (hand)
  (labels ((get-rank (quad-rank kicker-rank)
	     (cond
	       ((and (= quad-rank 2) (= kicker-rank 3)) 4688)
	       ((> kicker-rank 2) (1+ (get-rank quad-rank (1- kicker-rank))))
	       ((= kicker-rank 2) (1+ (get-rank (1- quad-rank) 14))))))
    (if (cadr hand)
	(get-rank (card-rank-values (caar hand)) (card-rank-values (cadr hand)))
	(get-rank (card-rank-values (caar hand)) 3))))

(defun straight-flush-rank (str-fl)
  (+ 192 (straight-rank str-fl)))

;;;; helpers

(defun sort-packed (packed)
  (let ((lists
	 (sort (remove-if-not #'listp packed)
	       #'>
	       :key #'(lambda (x) (card-rank-values (car x)))))
	(cards (remove-if #'listp packed)))
    (append lists cards)))

(defun pack-by-rank (ranks)
  (cond
    ((null ranks) nil)
    ((not (member (get-rank (car ranks))
    		  (mapcar #'get-rank (cdr ranks))
		  :test #'string=))
     (cons (car ranks) (pack-by-rank (cdr ranks))))
    (t
     (cons
      (cons (car ranks) (remove-if
			 #'(lambda (c)
			     (not
			      (string=
			       (get-rank c)
			       (get-rank (car ranks)))))
			 (cdr ranks)))
      (pack-by-rank (remove-if
		     #'(lambda (x)
			 (string=
			  (get-rank x)
			  (get-rank (car ranks))))
		     (cdr ranks)))))))

(defun get-rank (card)
  (subseq card 0 1))

(defun card-rank-values (item)
  "Convert a card or a list of cards to their rank values."
  (let ((vals '(("2" . 2) ("3" . 3) ("4" . 4) ("5" . 5) ("6" . 6)
		("7" . 7) ("8" . 8) ("9" . 9) ("T" . 10) ("J" . 11)
		("Q" . 12) ("K" . 13) ("A" . 14))))
    (cond
      ((stringp item)
       (cdr (assoc (subseq item 0 1) vals :test #'string=)))
      ((and (listp item) (> (length item) 0))
       (sort (mapcar #'card-rank-values item) #'>))
      (t (error "Cannot apply rank-value to ~a" item)))))

(defun get-pair-value (pair-rank)
"There are 220 possible sets of kickers that can be combined with any
given pair."
  (if (> pair-rank 2)
      (+ 221 (get-pair-value (1- pair-rank)))
      2))

(defun get-two-pair-values (pair1-rank pair2-rank)
  (cond
    ((and (= pair1-rank 2) (= pair2-rank 3)) 2875)
    ((> pair2-rank 2) (1+ (get-two-pair-values pair1-rank (1- pair2-rank))))
    (t (1+ (get-two-pair-values (1- pair1-rank) 14)))))

(defun get-trips-value (trips-rank)
  (if (> trips-rank 2)
      (+ 67 (get-trips-value (1- trips-rank)))
      3625))

(defun remove-kickers (rank from)
  (remove-if #'(lambda (x) (member rank x)) from))

(defun combinations (m list)
  "Find all m-sized combinations from list. Nicked this from
somewhere, but I can't recall where...:/"
  (let ((res nil))
    (labels ((comb1 (l c m)
	       (when (>= (length l) m)
		 (if (zerop m) (return-from comb1 (setq res (cons c res))))
		 (comb1 (cdr l) c m)
		 (comb1 (cdr l) (cons (first l) c) (1- m)))))
      (comb1 list nil m))
    res))

(defparameter *trips-kickers*
  (sort (combinations 2 '(2 3 4 5 6 7 8 9 10 11 12 13 14))
	#'< :key #'(lambda (x) (car x))))

(defparameter *pair-kickers*
  (sort (combinations 3 '(2 3 4 5 6 7 8 9 10 11 12 13 14))
	#'< :key #'(lambda (x) (car x))))
