;;; go-gnugo.el --- functions for interactive with a gnugo process using gtp

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

;;; Comments:

;; Interaction with gnugo

;;; CODE:
(require 'comint)

(defun go-gnugo-gtp-commands ()
  "Return a list of the gnugo GTP commands."
  (split-string
   (substring
    (shell-command-to-string
     (format "echo list_commands | %s --mode gtp" go-gnugo-program))
    2 -2) "\n"))

(defvar go-gnugo-program
  "gnugo"
  "path to gnugo executable")

(defvar go-gnugo-process-name
  "gnugo"
  "name for the gnugo process")

(defvar go-gnugo-buffer nil
  "comint buffer holding the gnugo processes")

(defun go-gnugo-start-process ()
  (interactive)
  (unless (comint-check-proc go-gnugo-buffer)
    (setf go-gnugo-buffer
	  (make-comint go-gnugo-process-name go-gnugo-program nil "--mode" "gtp" "--quiet"))
    (set-buffer go-gnugo-buffer)
    (comint-mode)))

(defun go-gnugo-command-to-string (command)
  "Send command to gnugo process and return gnugo's results as a string"
  (interactive "sgnugo command: ")
  (go-gnugo-input-command command)
  (go-gnugo-last-output))

(defun go-gnugo-input-command (command)
  "Pass COMMAND to the gnugo process running in `go-gnugo-buffer'"
  (save-excursion
    (message (format "buffer-%s" go-gnugo-buffer))
    (set-buffer go-gnugo-buffer)
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert command)
    (comint-send-input)
    (go-gnugo-wait-for-output)))

(defun go-gnugo-wait-for-output ()
  "Wait until output arrives"
  (save-excursion
    (set-buffer go-gnugo-buffer)
    (while (progn
	     (goto-char comint-last-input-end)
	     (not (re-search-forward "^= *[^\000]+?\n\n" nil t)))
      (accept-process-output (get-buffer-process (current-buffer))))))

(defun go-gnugo-last-output ()
  (save-window-excursion
    (set-buffer go-gnugo-buffer)
    (comint-show-output)
    (buffer-substring (point) (point-max))))

(provide 'go-gnugo)
;;; go-gnugo.el ends here