;;; go-gtp.el --- translate between sgf and GTP

;; Commentary:

;; This file should be useful for translating between sgf and GTP (Go
;; Text Protocol) see http://www.lysator.liu.se/~gunnar/gtp/

;; TODO:

;; - for now only working on GTP to allow playing against gnugo
;; 
;; - the GMP command set may get implemented (if I find a need for
;;   it)
;;   
;; - and while the IGS commands
;;   (http://www.pandanet.co.jp/English/commands/term/Summary.html)
;;   would certainly be usefull they may make more sense as a seperate
;;   package (need to see how much overlap there would be)

;; CODE:
(defvar go-gtp-letter-number-conversions
  '((?A . 1) (?a . 1) (?B . 2) (?b . 2) (?C . 3) (?c . 3) (?D . 4)
    (?d . 4) (?E . 5) (?e . 5) (?F . 6) (?f . 6) (?G . 7) (?g . 7)
    (?H . 8) (?h . 8) (?J . 9) (?j . 9) (?K . 10) (?k . 10)
    (?L . 11) (?l . 11) (?M . 12) (?m . 12) (?N . 13) (?n . 13)
    (?O . 14) (?o . 14) (?P . 15) (?p . 15) (?Q . 16) (?q . 16)
    (?R . 17) (?r . 17) (?S . 18) (?s . 18) (?T . 19) (?t . 19))
  "Table of letter <-> number conversions for gtp-sgf conversion")

(defvar go-gtp-mapping
  '(("B" "black")
    ("W" "white")
    ("SZ" "boardsize")
    ("KM" "komi"))
  "alist of sgf elements and their gtp counterparts")

(defun go-gtp-move-to-gtp (move)
  (format "%s %s" (case (car move) (:X "black") (:O "white")) (cdr move)))

(defun go-gtp-letter-to-number (letter)
  (unless (characterp letter (setf letter (string-to-char letter))))
  (cdr (assoc letter go-gtp-letter-number-conversions)))

(defun go-gtp-number-to-letter (number)
  (let ((result (car (rassoc number go-gtp-letter-number-conversions))))
    (if result
	(unless (stringp result)
	  (char-to-string result)))))

(defun go-gtp-point-to-gtp (point-string)
  (format "%s%d"
	  (substring point-string 0 1)
	  (go-gtp-letter-to-number (elt point-string 1))))

(defun go-gtp-point-to-sgf (point-string)
  (format "%s%s"
	  (substring point-string 0 1)
	  (go-gtp-letter-to-number (elt point-string 1))))

(defun go-gtp-element-to-gtp (element)
  "Convert an sgf element to a gtp command."
  (let ((key (car element))
	(value (cadr element)))
    ))

(defun go-gtp-command-to-sgf (command)
  "Convert a gtp command to an sgf element"
  (interactive)
  (unless (listp node)
    (error "node is not a cons cell"))
  (let ((symbol (car node))
	(value (cdr node)))
    (if (listp symbol) ; recurse
	(flatten (delq nil (mapcar 'go-gtp-node-to-gtp node)))
      (if (symbolp symbol)
	  (list
	   (case symbol
	    (':B
	     (format "black %s" (go-gtp-point-to-gtp-point value)))
	    (':W
	     (format "white %s" (go-gtp-point-to-gtp-point value)))
	    (':SZ
	     (format "boardsize %s" value))
	    (':KM
	     (format "komi %s" value))
	    (t
	     nil)))
	(error "%S is not a symbol" symbol)))))

(provide 'go-gtp)
;;; go-gtp.el ends here