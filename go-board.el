;;; gb.el --- Provide a Go Board interface to live and recorded GO Games

;; Copyright (C) 2008 Eric Schulte

;;; Liscence:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentrary:

;;; Code:
(let ((this-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path this-dir))
(require 'cl)
(require 'go-gtp)
(require 'go-gnugo)

(defvar gb-players
  '((:X :user) (:O :gnugo))
  "An alist indicating who/what is playing X and O.")

(defvar gb-whos-turn
  :X
  "which player is to go next")

(defvar gb-buffer-name
  "*gb*"
  "name for the go buffer")

(defvar gb-default-save-dir
  "~/docs/games/go/"
  "default directory in which to save go games")

(defvar gb-cols
  '(?A ?B ?C ?D ?E ?F ?G ?H ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T))

(defvar gb-rows
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"))

(defun gb-whos-turn ()
  (interactive)
  (message (format "%S" (cadr (assoc gb-whos-turn gb-players)))))

(defun current-line-length ()
  "Length of a the line at point."
  (save-excursion (end-of-line) (current-column)))

(defun gb-row-at-point (&optional point)
  (interactive)
  (let ((point (or point (point))))
    (save-excursion
    (move-beginning-of-line 1)
    (if (equal (char-after) 32) (forward-char 1))
    (thing-at-point 'word))))

(defun gb-col-at-point (&optional point)
  (interactive)
  (let* ((point (or point (point)))
	 (col (save-excursion (goto-char point) (current-column))))
    (save-excursion (goto-char (point-max))
		    (move-to-column col)
		    (if (char-after) (format "%c" (char-after))))))

(defun gb-point-to-stone (&optional point)
  "Convert point or the current point to a stone in the GO board."
  (interactive)
  (let* ((point (or point (point)))
	 (col (gb-col-at-point point))
	 (row (gb-row-at-point point))
	 (stone (if (and row col) (concat col row))))
    (when (interactive-p) (message stone))
    stone))

(defun gb-stone-to-point (stone)
  "Convert STONE to a buffer point"
  (interactive)
  (set-buffer gb-buffer-name)
  (let ((col (and (string-match "\\([[:alpha:]]+\\)[[:digit:]]+" stone)
		  (match-string 1 stone)))
	(row (and (string-match "[[:alpha:]]+\\([[:digit:]]+\\)" stone)
		  (match-string 1 stone))))
    (save-excursion
      (goto-char (point-min))
      (while (not (equal row (gb-row-at-point (point))))
	(forward-line 1))
      (while (not (equal col (gb-col-at-point (point))))
	(forward-char 1))
      (point))))

(defun gb-refresh ()
  "Display a GO board depicting the current state of the game."
  (interactive)
  (set-buffer gb-buffer-name)
  (let* ((return-p (point))
	 (new-board (go-gnugo-command-to-string "showboard"))
	 (board-height (length (split-string new-board "\n")))
	 (height (window-height)))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (dotimes (n (/ (- height board-height) 2)) (insert "\n"))
    (insert new-board)
    (forward-line (- (/ board-height 2)))
    (recenter)
    (gb-paint)
    (goto-char return-p)))

(defun gb-make-move (&optional color stone)
  "Make a move on the board and save it to the buffer"
  (interactive)
  (let* ((color (or color gb-whos-turn))
	 (stone (or stone (gb-point-to-stone)))
	 (gtp-move (go-gtp-move-to-gtp (cons color stone)))
	 white-move)
    (message gtp-move)
    (go-gnugo-input-command gtp-move)
    (setf white-move (go-gnugo-command-to-string "genmove_white"))
    (gb-refresh)
    (message (format "white %s" white-move))
    (gb-highlight-stone white-move)))

(defun gb-pass ()
  (interactive)
  (gb-make-move nil "PASS"))

(defun gb-undo (&optional times)
  (interactive "p")
  (let ((times (* 2 (or times 1))))
    (dotimes (var times)
      (go-gnugo-input-command "undo")))
  (gb-refresh)
  (gb-highlight-last-move))

(defun gb-highlight-stone (move)
  "Highlight the stone indicated by MOVE"
  (let ((point (gb-stone-to-point move)))
    (overlay-put (make-overlay point (+ 1 point)) 'face 'highlight)))

(defun gb-highlight-last-move ()
  (interactive)
  (let* ((last-move (go-gnugo-command-to-string "last_move"))
	 (stone (and (string-match "[blackwhite]+ \\([[:alpha:]]+[[:digit:]]+\\)" last-move)
		      (match-string 1 last-move))))
    (message (or stone last-move))
    (if stone (gb-highlight-stone stone))))

(defun gb-paint-point (&optional point whos-territory)
  "Optional argument WHOS-TERRITORY can be used to tint the
background according to the owning player."
  (interactive)
  (let ((point (or point (point)))
	(faces (mapcar
		(lambda (el)
		  (concat "gb-" (if whos-territory (concat whos-territory  "-territory-")) el))
		'("background" "hoshi" "white" "black"))))
    (case (char-after point)
      ((?  ?.) (overlay-put (make-overlay point (+ 1 point)) 'face (first faces)))
      (?+ (overlay-put (make-overlay point (+ 1 point)) 'face (second faces)))
      (?O (overlay-put (make-overlay point (+ 1 point)) 'face (third faces)))
      (?X (overlay-put (make-overlay point (+ 1 point)) 'face (fourth faces))))))

(defun gb-paint ()
  (interactive)
  (save-excursion
    (set-buffer gb-buffer-name)
    (goto-char (gb-stone-to-point "A19"))
    (dotimes (row 19)
      (dotimes (col 37)
	(gb-paint-point)
	(forward-char 1))
      (forward-line 1)
      (forward-char 3))))

(defun gb-paint-final ()
  "Estimate the final status of the board and display"
  (interactive)
  (save-excursion
    (set-buffer gb-buffer-name)
    (let ((alive (mapcar 'gb-stone-to-point
			 (split-string (go-gnugo-command-to-string "final_status_list alive"))))
	  (dead (mapcar 'gb-stone-to-point
			(split-string (go-gnugo-command-to-string "final_status_list dead"))))
	  (white (mapcar 'gb-stone-to-point
			 (split-string (go-gnugo-command-to-string "list_stones white"))))
	  (black (mapcar 'gb-stone-to-point
			 (split-string (go-gnugo-command-to-string "list_stones black"))))
	  territory)
      (dolist (color '("white" "black"))
	(setf territory
	      (union (intersection (eval (intern color)) alive)
		     (intersection (eval (intern (if (string= "white" color) "black" "white"))) dead)))
	(setf territory
	      (union territory (mapcar 'gb-stone-to-point
				       (split-string (go-gnugo-command-to-string
						      (format "final_status_list %s_territory" color))))))
	(mapcar (lambda (el) (gb-paint-point el color) (gb-paint-point (+ 1 el) color)) territory))
      (message (go-gnugo-command-to-string "estimate_score")))))

(defun gb-dragon (&optional arg)
  (interactive "P")
  "Indicate the stones in MOVE's dragon"
  (let ((move (gb-point-to-stone (point)))
	dragon)
    (if arg (setf move (read-from-minibuffer "move: " move)))
    (setf dragon (go-gnugo-command-to-string (format "dragon_stones %s" move)))
    (mapcar 'gb-highlight-stone (split-string dragon))
    (message dragon)))

(defun gb-attack-stone (&optional stone)
  (interactive)
  "suggest a move for attacking STONE"
  (let* ((stone (or stone
		    (gb-point-to-stone (point))
		    (read-from-minibuffer "point? ")))
	 (response (go-gnugo-command-to-string (concat "attack " stone))))
    (message (if (string= response "0")
		 "no suggestion"
	       response))))

(defun gb-defend-stone (&optional stone)
  (interactive)
  "suggest a move for defending STONE"
  (let* ((stone (or stone
		    (gb-point-to-stone (point))
		    (read-from-minibuffer "point? ")))
	 (response (go-gnugo-command-to-string (concat "defend " stone))))
    (message (if (string= response "0")
		 "no suggestion"
	       response))))

(defun gb-estimate-score ()
  (interactive)
  (gb-gtp-command "estimate_score"))

(defun gb-gtp-command (&optional command)
  (interactive)
  (message (go-gnugo-command-to-string
	    (or command
		(let ((base (completing-read "command: " (go-gnugo-gtp-commands))))
		  (read-from-minibuffer "command: " base))))))

(defun gb-save (&optional file)
  (interactive)
  (with-temp-file (or file (read-from-minibuffer ;; TODO filename completion
			    "save to: " (or gb-default-save-dir
					    default-directory)))
    (insert (go-gnugo-command-to-string "printsgf"))))

(defun gb-quit ()
  "quit the current game, prompting for a save"
  (interactive)
  (if (y-or-n-p "Save Game?") (gb-save))
  (message "goodbye")
  (kill-buffer "*gnugo*")
  (kill-buffer gb-buffer-name))

(defun gb-suspend ()
  "Quickly drop down the go board"
  (bury-buffer))

(defun gb-move-point (direction)
  "Move point one stone in DIRECTION.  DIRECTION can be 'left
'right 'up or 'down."
  )

;;-------------------------------------------------------------------------------
;;; go board mode
(defvar gb-mode-hook nil)
(defvar gb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'gb-make-move)
    (define-key map "a" 'gb-attack-stone)
    (define-key map "c" 'gb-gtp-command)
    (define-key map "d" 'gb-defend-stone)
    (define-key map "D" 'gb-dragon)
    (define-key map "e" 'gb-estimate-score)
    (define-key map "f" 'gb-paint-final)
    (define-key map "r" 'gb-refresh)
    (define-key map "w" 'gb-whos-turn)
    (define-key map "l" 'gb-highlight-last-move)
    (define-key map "m" 'gb-point-to-stone)
    (define-key map "p" 'gb-point-to-stone)
    (define-key map "q" 'gb-quit)
    (define-key map "s" 'gb-save)
    (define-key map "u" 'gb-undo)
    (define-key map "z" 'gb-suspend)
    map)
  "Keymap for `gb-mode'.")
(defconst gb-font-lock-keywords '()
  "Font lock keywords for `gb-mode'.")

(defface gb-background
  '((t (:background "#b36108" :foreground "#6f3c04")))
  "woodsy background")

(defface gb-hoshi
  '((t (:background "#b36108" :foreground "#6d3300")))
  "woodsy background with darker hoshi mark")

(defface gb-black
  '((t (:background "#b36108" :foreground "black")))
  "black piece on woodsy background")

(defface gb-white
  '((t (:background "#b36108" :foreground "white")))
  "white piece on woodsy background")

(defface gb-black-territory-background
  '((t (:background "#6a4014" :foreground "#6f3c04")))
  "woodsy background")

(defface gb-black-territory-hoshi
  '((t (:background "#6a4014" :foreground "#6d3300")))
  "woodsy background with darker hoshi mark")

(defface gb-black-territory-black
  '((t (:background "#6a4014" :foreground "black")))
  "black piece on black territory")

(defface gb-black-territory-white
  '((t (:background "#6a4014" :foreground "#6b6b6b")))
  "white piece on black territory")

(defface gb-white-territory-background
  '((t (:background "#cd9c67" :foreground "#6f3c04")))
  "white territory")

(defface gb-white-territory-hoshi
  '((t (:background "#cd9c67" :foreground "#6d3300")))
  "white territory with darker hoshi mark")

(defface gb-white-territory-black
  '((t (:background "#cd9c67" :foreground "#6b6b6b")))
  "black piece on white territory")

(defface gb-white-territory-white
  '((t (:background "#cd9c67" :foreground "white")))
  "white piece on white territory")

;;;###autoload
(defun gb-mode (&optional args)
  (interactive)
  "A major mode supporting a board interface to the game of GO."
  ;; local variables
  (kill-all-local-variables)
  (setq major-mode 'gb-mode
	mode-name "GO Board")
  (use-local-map gb-mode-map)
  (setf truncate-lines t)
  ;; font locking
  (set (make-local-variable 'font-lock-defaults)
       '(gb-font-lock-keywords t))
  ;; initialize the board
  (go-gnugo-start-process args)
  (gb-refresh)
  (run-mode-hooks 'gb-mode-hook))

;;--------------------------------------------------------------------------------
;; start up a game
(defun go-start-game (&optional arg)
  (interactive)
  (if (get-buffer gb-buffer-name) (kill-buffer gb-buffer-name))
  (let ((buffer (get-buffer-create gb-buffer-name))
	(args (if arg (read-from-minibuffer "args to gnugo: "))))
    (switch-to-buffer gb-buffer-name)
    (gb-mode args)))

(provide 'gb)
;;; gb.el ends here