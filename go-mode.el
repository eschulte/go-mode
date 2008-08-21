;;; go-mode.el --- mode for playing, browsing, and editing go games

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

(require 'sgf-rw)
(require 'sgf-gnugo)
(require 'sgf-board)
(require 'sgf-gtp)

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

(defun sgf-update-gnugo ()
  "pass the current turn information to gnugo"
  (interactive)
  (message "%S"
	   (mapcar 'sgf-send-line-to-gnugo
		   (sgf-node-to-gtp (sgf-get-current-turn)))))

(defun sgf-gnugo-command (command)
  (interactive "scommand: ")
  (message "%s" (sgf-send-line-to-gnugo command)))

(provide 'go-mode)
;;; go-mode.el ends here