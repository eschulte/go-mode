;;; taken directly from gnugo.el
;;
;; Here is their copyright
;;
;; Copyright (C) 1999,2000,2002,2003,2004,2005,2006 Thien-Thi Nguyen
;; This file is part of ttn's personal elisp library, released under
;; GNU GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for
;; details.
;;
;;
;; TODO: Should probably rework all of this so that it runs off of
;; sgftree-nodes
;;

;;; Code:

(require 'sgf-sgf)
(require 'sgf-gnugo)
(require 'sgf-board)
(require 'sgf-gtp)

;; from http://cogsci.uwaterloo.ca/CoherenceCode/COHERE/utilities.lisp.html
(defun flatten (lis)
  "Removes nestings from a list."
  (cond ((atom lis) lis)
	((listp (car lis))
	 (append (flatten (car lis)) (flatten (cdr lis))))
	(t (append (list (car lis)) (flatten (cdr lis))))))

(defvar sgf-mode-hook nil)

(defvar sgf-mode-map
  (let ((sgf-mode-map (make-sparse-keymap)))
    (define-key sgf-mode-map "\C-cd"    'sgf-lookup-property)
    (define-key sgf-mode-map "\C-cn"    'sgf-next-turn)
    (define-key sgf-mode-map "\C-cp"    'sgf-previous-turn)
    (define-key sgf-mode-map "\M-n"     'sgf-next-variant)
    (define-key sgf-mode-map "\M-p"     'sgf-previous-variant)
    ;; (define-key sgf-mode-map "\M-l"     'sgf-set-turn)
    (define-key sgf-mode-map "\C-ct"    'sgf-switch/view-turn)
    (define-key sgf-mode-map "\C-c\C-g" 'sgf-gnugo-command)
    (define-key sgf-mode-map "\C-c\C-c" 'sgf-update-gnugo)
    sgf-mode-map)
  "Keymap for SGF major mode")

(defvar sgf-font-lock-keywords nil
  "font lock keywords for `sgf-mode'")

(let ((sgf-keywords (concat "\\("
			    (regexp-opt
			     (mapcar (lambda (full)
				       (format "%s" (car full)))
				     sgf-properties))
			    "\\)"
			    "\\[")))
  (setq sgf-font-lock-keywords-1
	(list
	 (list sgf-keywords 1 'font-lock-keyword-face)))
  (setq sgf-font-lock-keywords-2 sgf-font-lock-keywords-1)
  (setq sgf-font-lock-keywords-3 sgf-font-lock-keywords-2))

(defvar sgf-mode-syntax-table
  (make-syntax-table)
  "Syntax table used in `sgf-mode' buffers.")

(add-to-list 'auto-mode-alist '("\\.sgf\\'" . sgf-mode))

(defvar sgf-mode-current-game
  nil
  "This should be set as a `make-local-variable' local variable,
  in the current sgf buffer.  It is set to the game resulting
  from `sgf-read-file'")

(defvar sgf-mode-current-turn
  -1
  "This should be set buffer-local, to track the current turn being displayed")

;; sgf functions

(defun sgf-lookup-property ()
  "return the meaning of the property at the point"
  (interactive)
  (let ((property (thing-at-point 'word)))
    (message (format "%s: %s"
		     property
		     (cadr (assq (read property)
				 sgf-properties))))))

(defun sgf-next-turn ()
  "view the next turn in this game"
  (interactive)
  (sgf-switch/view-turn 1)
  (sgf-update-gnugo))

(defun sgf-previous-turn ()
  "view the next turn in this game"
  (interactive)
  (sgf-switch/view-turn -1)
  (sgf-gnugo-command "undo"))

(defun sgf-set-turn ()
  (interactive)
  (let ((turn (sgf-turn-of-point)))
    (message "turn %d" turn)
    (setf sgf-mode-current-turn turn)))

(defun sgf-get-current-turn ()
  "return the elisp representation of the current turn"
  (interactive)
  (elt (car sgf-mode-current-game) sgf-mode-current-turn))

(defun sgf-print-current-turn ()
  "return the elisp representation of the current turn"
  (interactive)
  (message "%S" (elt (car sgf-mode-current-game) sgf-mode-current-turn)))

(defun sgf-switch/view-turn (num)
  "adjust the number of turns by num, then switch to that turn"
  (interactive "nturn number: ")
  (setq sgf-mode-current-turn (+ sgf-mode-current-turn num))
  (goto-char (sgf-point-of-turn sgf-mode-current-turn))
  (message "%S" (sgf-get-current-turn)))

(defun sgf-point-of-turn (number)
  "return the point in the buffer of the NUMBER'th turn"
  (interactive "nWhich Turn?: ")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[( \n];" nil t (+ 1 number))
    (point)))

(defun sgf-turn-of-point (&optional place)
  "return the turn number of the current `point' in the sgf-file"
  (interactive)
  (save-excursion
    (let ((place (or place (point)))
	  (counter 0)
	  (still t))
      (goto-char (point-min))
      (while (and still (< (point) place))
	(setq still (re-search-forward "[( \n];" nil t))
	(setf counter (+ 1 counter)))
      (point)
      (- counter 1))))

(defun sgf-update-gnugo ()
  "pass the current turn information to gnugo"
  (interactive)
  (message "%S"
	   (mapcar 'sgf-send-line-to-gnugo
		   (sgf-node-to-gtp (sgf-get-current-turn)))))

(defun sgf-gnugo-command (command)
  (interactive "scommand: ")
  (message "%s" (sgf-send-line-to-gnugo command)))

;;;###autoload
(defun sgf-mode ()
  "a VERY simple major mode for editing sgf files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sgf-mode-syntax-table)
  (setq major-mode 'sgf-mode
	mode-name "SGF")
  (use-local-map sgf-mode-map)
  ;; font locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((sgf-font-lock-keywords-1
                              sgf-font-lock-keywords-2
                              sgf-font-lock-keywords-3)))
  ;; make local variables
  (make-local-variable 'sgf-mode-current-game)
  (make-local-variable 'sgf-mode-current-turn)
  (make-local-variable 'sgf-gnugo-kept)
  (make-local-variable 'sgf-gnugo-program)
  (make-local-variable 'sgf-gnugo-process)
  ;; set local variables
  (set 'sgf-mode-current-game (sgf-read-file (buffer-file-name))) ; parse file
  (set 'sgf-gnugo-process (sgf-start-gnugo-process)) ; gnugo process
  (run-mode-hooks 'sgf-mode-hook))

;;   ;; read in the file
;;   (sgf-read-file (buffer-file-name))
;;   ;; start up gnugo
;;   (setf sgf-gnugo-process (sgf-start-gnugo-process))

(provide 'sgf-mode)
