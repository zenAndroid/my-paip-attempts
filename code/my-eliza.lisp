;; Allright folks, let's go.
;; I hope good things happen to us.
(load "paip-helpers.lisp")

(defun variable-p (x)
  "Is X a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list. "
  (cons (cons var val)
	      ;; Once we add a "real" binding,
	      ;; we can get rid of the dummy no-bindings
	      (if (eq bindings no-bindings)
	          nil
	          bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	        ((equal input (binding-val binding)) bindings)
	        (t fail))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	      ((variable-p pattern) (match-variable pattern input bindings))
	      ((eql pattern input) bindings)
	      ((segment-pattern-p pattern)   #| *** |# (segment-match pattern input bindings))    ; ***
	      ((and (consp pattern) (consp input))
	       (pat-match (rest pattern) (rest input)
		                (pat-match (first pattern) (first input)
				                       bindings)))
	      (t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

;; > (pat-match '((?* ?p) need (?* ?x))
;;              '(Mr Hulot and I need a vacation))
;; ((?P MR HULOT AND I) (?X A VACATION))

;; > (pat-match '((?* ?x) is a (?* ?y)) '(what he is is a fool))
;; ((?X WHAT HE IS) (?Y FOOL))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
	      (pat (rest pattern)))
    (if (null pat) ; The case where this is the last variable
	      (match-variable var input bindings) ; Match the rest of the input
	      ;; We assume that pat starts with a constant
	      ;; In other words, a pattern can't have 2 consecutive vars
	      (let ((pos (position (first pat) input :start start :test #'equal)))
          ;; Basically, this finds the 'need' symbol in the input (example below)
          ;; > (pat-match '((?* ?p) need (?* ?x))
          ;;              '(Mr Hulot and I need a vacation))
          ;; ((?P MR HULOT AND I) (?X A VACATION))
	        (if (null pos)
	            fail ; If you cant find it, then return the fail constant
	            (let ((b2 (pat-match #| NEW PART; adds the binding before trying to match the rest |#
			                   pat (subseq input pos)
			                   (match-variable var (subseq input 0 pos) bindings))))
		            ;; If this match failed, try another longer one
		            (if (eq b2 fail)
		                (segment-match pattern input bindings (+ pos 1))
		                b2)))))))


;; 0: (PAT-MATCH ((?* ?X) A B (?* ?X)) (1 2 A B A B 1 2 A B))
;;  1: (SEGMENT-MATCH ((?* ?X) A B (?* ?X)) (1 2 A B A B 1 2 A B) ((T . T)))
;;   2: (PAT-MATCH (A B (?* ?X)) (A B A B 1 2 A B) ((T . T)))
;;    3: (PAT-MATCH A A ((T . T)))
;;    3: PAT-MATCH returned ((T . T))
;;    3: (PAT-MATCH (B (?* ?X)) (B A B 1 2 A B) ((T . T)))
;;     4: (PAT-MATCH B B ((T . T)))
;;     4: PAT-MATCH returned ((T . T))
;;     4: (PAT-MATCH ((?* ?X)) (A B 1 2 A B) ((T . T)))
;;      5: (SEGMENT-MATCH ((?* ?X)) (A B 1 2 A B) ((T . T)))
;;      5: SEGMENT-MATCH returned ((?X A B 1 2 A B))
;;     4: PAT-MATCH returned ((?X A B 1 2 A B))
;;    3: PAT-MATCH returned ((?X A B 1 2 A B))
;;   2: PAT-MATCH returned ((?X A B 1 2 A B))
;;  1: SEGMENT-MATCH returned NIL
;; 0: PAT-MATCH returned NIL

;; So, basically, the older version did this; it matched the rest of the pattern *WITHOUT* informing the rest of the binding of the ?x variable, whereas the newer version of the pattern matcher calls match-variable *before* doing so ...

;;   0: (pat-match ((?* ?x) a b (?* ?x)) (1 2 a b a b 1 2 a b))
;;     1: (segment-match ((?* ?x) a b (?* ?x)) (1 2 a b a b 1 2 a b) ((t . t)))
;;       2: (pat-match (a b (?* ?x)) (a b a b 1 2 a b) ((?x 1 2)))
;;         3: (pat-match a a ((?x 1 2)))
;;         3: pat-match returned ((?x 1 2))
;;         3: (pat-match (b (?* ?x)) (b a b 1 2 a b) ((?x 1 2)))
;;           4: (pat-match b b ((?x 1 2)))
;;           4: pat-match returned ((?x 1 2))
;;           4: (pat-match ((?* ?x)) (a b 1 2 a b) ((?x 1 2)))
;;             5: (segment-match ((?* ?x)) (a b 1 2 a b) ((?x 1 2)))
;;             5: segment-match returned nil
;;           4: pat-match returned nil
;;         3: pat-match returned nil
;;       2: pat-match returned nil
;;       2: (segment-match ((?* ?x) a b (?* ?x)) (1 2 a b a b 1 2 a b) ((t . t)) 3)
;;         3: (pat-match (a b (?* ?x)) (a b 1 2 a b) ((?x 1 2 a b)))
;;           4: (pat-match a a ((?x 1 2 a b)))
;;           4: pat-match returned ((?x 1 2 a b))
;;           4: (pat-match (b (?* ?x)) (b 1 2 a b) ((?x 1 2 a b)))
;;             5: (pat-match b b ((?x 1 2 a b)))
;;             5: pat-match returned ((?x 1 2 a b))
;;             5: (pat-match ((?* ?x)) (1 2 a b) ((?x 1 2 a b)))
;;               6: (segment-match ((?* ?x)) (1 2 a b) ((?x 1 2 a b)))
;;               6: segment-match returned ((?x 1 2 a b))
;;             5: pat-match returned ((?x 1 2 a b))
;;           4: pat-match returned ((?x 1 2 a b))
;;         3: pat-match returned ((?x 1 2 a b))
;;       2: segment-match returned ((?x 1 2 a b))
;;     1: segment-match returned ((?x 1 2 a b))
;;   0: pat-match returned ((?x 1 2 a b))
;; ((?x 1 2 a b))

;; This is the newer version, notice how it retried the match at position 3.

(defun rule-pattern (rule) (first rule))

(defun rule-responses (rule) (rest rule))


;; (defun eliza ()
;;   "Respond to user input using pattern matching rules."
;;   (loop
;;     (print 'eliza>)
;;     (write (flatten (use-eliza-rules (read))) :pretty t)))
;; This is the old version, naive reader.

;; I have also seen that the reader isn't safe? cringe ...
;; Need to look into that.

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (let ((input (read-line)))
      (if (exit-sequence-p input)
          (return-from eliza 'DONE)
          (format t "~{~a~^ ~}" (flatten
                                 (use-eliza-rules
                                  (read-from-string
                                   (concatenate
                                    'string
                                    "(" (substitute-if #\Space #'punctp input) ")")))))))))

(defun exit-sequence-p (arg)
  (equalp arg "goodbye"))

(defun punctp (seq) (find seq ".,!'$%^&*"))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
	          (let ((result (pat-match (rule-pattern rule) input)))
	            (if (not (eq result fail))
		              (sublis (switch-viewpoint result)
			                    (random-elt (rule-responses rule))))))
	      *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))
