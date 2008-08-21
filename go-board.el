;;; go-board.el --- Provide a Go Board interface to live and recorded GO Games

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

;; see the TODO file in this directory

;;; Code:
(require 'go-gtp)
(require 'go-gnugo)

(defvar go-board-players
  '((:X :user) (:O :gnugo))
  "An alist indicating who/what is playing X and O.")

(defvar go-board-whos-turn
  :X
  "which player is to go next")

(defun go-board-whos-turn-string ()
  (case go-board-whos-turn (:X "black") (:O "white")))

(defun go-board-which-players-turn ()
  (cadr (assoc go-board-whos-turn go-board-players)))

(defun go-board-toggle-turn ()
  "Update which player is to go next"
  (if (equal :X go-board-whos-turn)
      (setf go-board-whos-turn :O)
    (setf go-board-whos-turn :X))
  (if (equal (go-board-which-players-turn) :gnugo)
      (go-gnugo-send/return "genmove_white"))
  (go-board-refresh))

(defun go-board-whos-turn ()
  (interactive)
  (message (format "%S" (cadr (assoc go-board-next-turn go-board-players)))))

(defun go-board-row-at-point (&optional point)
  (interactive)
  (let ((point (or point (point))))
    (save-excursion
    (move-beginning-of-line 1)
    (if (equal (char-after) 32) (forward-char 1))
    (thing-at-point 'word))))

(defun current-line-length ()
  "Length of a the line at point."
  (save-excursion (end-of-line) (current-column)))

(defun go-board-col-at-point (&optional point)
  (interactive)
  (let* ((point (or point (point)))
	 (col (save-excursion (goto-char point) (current-column))))
    (save-excursion (goto-char (point-max))
		    (move-to-column col)
		    (format "%c" (char-after)))))

(defun go-board-point-to-vertex (&optional point)
  "Convert point or the current point to a vertex in the GO board."
  (interactive)
  (concat (go-board-col-at-point point) (go-board-row-at-point point)))

;; TODO: put emphasis on the last move
(defun go-board-refresh ()
  "Display a GO board depicting the current state of the game."
  (interactive)
  (let* ((return-p (point))
	 (new-board (go-gnugo-send/return "showboard"))
	 (board-height (length (split-string new-board "\n")))
	 (height (window-height)))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (dotimes (n (/ (- height board-height) 2)) (insert "\n"))
      (insert new-board)
      (forward-line (- (/ board-height 2)))
      (recenter)
      (goto-char return-p)))

(defun go-board-play-move (&optional color vertex)
  "Make a move on the board and save it to the buffer"
  (interactive)
  (let* ((color (or color go-board-next-turn))
	 (vertex (or vertex (go-board-point-to-vertex)))
	 (gtp-move (go-gtp-move-to-gtp (cons color vertex))))
    (message gtp-move)
    (go-gnugo-send/return gtp-move)
    (go-board-toggle-turn)))

(defun go-board-dragon-stones (stone)
  (interactive)
  "Indicate the stones in STONEs dragon")

(defun go-board-move-point (direction)
  "Move point one vertex in DIRECTION.  DIRECTION can be 'left
'right 'up or 'down."
  )

(defun go-board-highlight-last-move ()
  (go-board-highlight-move) ;; use the last_move gtp command
  )

(defun go-board-highlight-move (vertex)
  "Emphasize single move on the board."
  )

(defun go-board-highlight-group (verticies)
  "Emphasize a group of vertices on the board."
  )

;;-------------------------------------------------------------------------------
;;; go board mode
(defvar go-board-mode-hook nil)

(defvar go-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'go-board-play-move)
    (define-key map "g" 'go-gnugo-send/return)
    (define-key map "w" 'go-board-whos-turn)
    map)
  "Keymap for `go-board-mode'.")

(defface go-board-X
  '((t :bold t :background "#ecb86a" :foreground "Black"))
  "Face for black (X) pieces on the GO board."
  :group 'go-board)

(defface go-board-Y
  '((t :bold t :background "#ecb86a" :foreground "White"))
  "Face for white (O) pieces on the GO board."
  :group 'go-board)

(defface go-board-vertex
  '((t :bold t :background "#ecb86a" :foreground "Black"))
  "Face for black (X) pieces on the GO board."
  :group 'go-board)

(defface go-board-hoshi
  '((t :bold t :background "#ecb86a" :foreground "Black"))
  "Face for black (X) pieces on the GO board."
  :group 'go-board)

(defface go-board-background
  '((t :bold t :background "#ecb86a"))
  "Face for black (X) pieces on the GO board."
  :group 'go-board)

(defconst go-board-font-lock-keywords
  '(("X" . go-board-X)
    ("O" . go-board-O)
    ("." . go-board-vertex)
    ("+" . go-board-hoshi))
  "Font lock keywords for `go-board-mode'.")

;;
;; (add-to-list 'load-path "~/projects/go/go-mode/")
;; (require 'go-board)
;;

;;;###autoload
(defun go-board-mode ()
  (interactive)
  "A major mode supporting a board interface to the game of GO."
  ;; local variables
  (kill-all-local-variables)
  (setq major-mode 'go-board-mode
	mode-name "GO Board")
  (use-local-map go-board-mode-map)
  (setf truncate-lines t)
  ;; font locking
  (set (make-local-variable 'font-lock-defaults)
       '(go-board-font-lock-keywords t))
  ;; initialize the board
  (make-local-variable 'go-gnugo-process)
  (go-gnugo-start-process)
  (go-board-refresh)
  (run-mode-hooks 'go-board-mode-hook))

(provide 'go-board)
;;; go-board.el ends here