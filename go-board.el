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
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'go-gtp)
(require 'go-gnugo)

(defvar go-board-players
  '((:X :user) (:O :gnugo))
  "An alist indicating who/what is playing X and O.")

(defvar go-board-whos-turn
  :X
  "which player is to go next")

(defvar go-board-buffer-name "*go-board*"
  "name for the go buffer")

(defun go-board-whos-turn ()
  (interactive)
  (message (format "%S" (cadr (assoc go-board-whos-turn go-board-players)))))

(defun current-line-length ()
  "Length of a the line at point."
  (save-excursion (end-of-line) (current-column)))

(defun go-board-row-at-point (&optional point)
  (interactive)
  (let ((point (or point (point))))
    (save-excursion
    (move-beginning-of-line 1)
    (if (equal (char-after) 32) (forward-char 1))
    (thing-at-point 'word))))

(defun go-board-col-at-point (&optional point)
  (interactive)
  (let* ((point (or point (point)))
	 (col (save-excursion (goto-char point) (current-column))))
    (save-excursion (goto-char (point-max))
		    (move-to-column col)
		    (if (char-after) (format "%c" (char-after))))))

(defun go-board-point-to-vertex (&optional point)
  "Convert point or the current point to a vertex in the GO board."
  (interactive)
  (concat (go-board-col-at-point point) (go-board-row-at-point point)))

(defun go-board-vertex-to-point (vertex)
  "Convert VERTEX to a buffer point"
  (interactive)
  (let ((col (and (string-match "\\([[:alpha:]]+\\)[[:digit:]]+" vertex)
		  (match-string 1 vertex)))
	(row (and (string-match "[[:alpha:]]+\\([[:digit:]]+\\)" vertex)
		  (match-string 1 vertex))))
    (save-excursion
      (goto-char (point-min))
      (while (not (equal row (go-board-row-at-point (point))))
	(forward-line 1))
      (while (not (equal col (go-board-col-at-point (point))))
	(forward-char 1))
      (point))))

;; TODO: put emphasis on the last move
(defun go-board-refresh ()
  "Display a GO board depicting the current state of the game."
  (interactive)
  (set-buffer go-board-buffer-name)
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
    (goto-char return-p)))

(defun go-board-make-move (&optional color vertex)
  "Make a move on the board and save it to the buffer"
  (interactive)
  (let* ((color (or color go-board-whos-turn))
	 (vertex (or vertex (go-board-point-to-vertex)))
	 (gtp-move (go-gtp-move-to-gtp (cons color vertex)))
	 white-move)
    (message gtp-move)
    (go-gnugo-input-command gtp-move)
    (setf white-move (go-gnugo-command-to-string "genmove_white"))
    (go-board-refresh)
    (message (format "white %s" white-move))
    (go-board-highlight-move white-move)))

(defun go-board-undo (&optional times)
  (interactive "p")
  (let ((times (or times 1)))
    (dotimes (var times)
      (go-gnugo-input-command "undo")))
  (go-board-refresh))

(defun go-board-highlight-move (move)
  "Highlight the stone indicated by MOVE"
  ;; (info "(elisp)Overlay Properties")
  (let ((point (go-board-vertex-to-point move)))
    (overlay-put (make-overlay point (+ 1 point)) 'face 'highlight)))

(defun go-board-highlight-last-move ()
  (interactive)
  (let* ((last-move (go-gnugo-command-to-string "last_move"))
	 (vertex (and (string-match "[blackwhite]+ \\([[:alpha:]]+[[:digit:]]+\\)" last-move)
		      (match-string 1 last-move))))
    (go-board-highlight-move vertex)))

(defun go-board-dragon (&optional arg)
  (interactive "P")
  "Indicate the stones in MOVE's dragon"
  (let ((move (go-board-point-to-vertex (point)))
	dragon)
    (if arg (setf move (read-from-minibuffer "move: " move)))
    (setf dragon (go-gnugo-command-to-string (format "dragon_stones %s" move)))
    (mapcar 'go-board-highlight-move (split-string dragon))
    (message dragon)))

(defun go-board-gtp-command (&optional command)
  (interactive)
  (message (go-gnugo-command-to-string (or command
					   (completing-read "command: " (go-gnugo-gtp-commands))))))

(defun go-board-move-point (direction)
  "Move point one vertex in DIRECTION.  DIRECTION can be 'left
'right 'up or 'down."
  )

(defun go-board-highlight-group (verticies)
  "Emphasize a group of vertices on the board."
  )

;;-------------------------------------------------------------------------------
;;; go board mode
(defvar go-board-mode-hook nil)
(defvar go-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'go-board-make-move)
    (define-key map " " 'go-board-refresh)
    (define-key map "w" 'go-board-whos-turn)
    (define-key map "l" 'go-board-highlight-last-move)
    (define-key map "u" 'go-board-undo)
    (define-key map "d" 'go-board-dragon)
    (define-key map "c" 'go-board-gtp-command)
    map)
  "Keymap for `go-board-mode'.")
;; (defface go-board-X
;;   '((t :bold t :background "#ecb86a" :foreground "Black"))
;;   "Face for black (X) pieces on the GO board."
;;   :group 'go-board)
;; (defface go-board-Y
;;   '((t :bold t :background "#ecb86a" :foreground "White"))
;;   "Face for white (O) pieces on the GO board."
;;   :group 'go-board)
;; (defface go-board-vertex
;;   '((t :bold t :background "#ecb86a" :foreground "Black"))
;;   "Face for black (X) pieces on the GO board."
;;   :group 'go-board)
;; (defface go-board-hoshi
;;   '((t :bold t :background "#ecb86a" :foreground "Black"))
;;   "Face for black (X) pieces on the GO board."
;;   :group 'go-board)
(defface go-board-background
  '((t :bold t :background "#ecb86a"))
  "Face for black (X) pieces on the GO board."
  :group 'go-board)
(defconst go-board-font-lock-keywords
  '(
;;     ("X" . go-board-X)
;;     ("O" . go-board-O)
;;     ("." . go-board-vertex)
;;     ("+" . go-board-hoshi)
    )
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
  (go-gnugo-start-process)
  (go-board-refresh)
  (run-mode-hooks 'go-board-mode-hook))

;;--------------------------------------------------------------------------------
;; start up a game
(defun go-start-game ()
  (interactive)
  (if (get-buffer go-board-buffer-name) (kill-buffer go-board-buffer-name))
  (let ((buffer (get-buffer-create go-board-buffer-name)))
    (switch-to-buffer go-board-buffer-name)
    (go-board-mode)))

(provide 'go-board)
;;; go-board.el ends here