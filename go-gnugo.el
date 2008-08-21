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

(defvar go-gnugo-kept
  ""
  "collects output from gnugo")

(defvar go-gnugo-process
  nil
  "the gnugo process object")

(defun go-gnugo-keep-output (process output)
  (setq go-gnugo-kept (concat go-gnugo-kept output)))

(defun go-gnugo-start-process ()
  (interactive)
  (let ((proc (apply 'start-process "gnugo" nil go-gnugo-program
		     "--mode" "gtp" "--quiet" nil)))
    (set-process-filter proc 'go-gnugo-keep-output)
    (setf go-gnugo-process proc)))

(defun go-gnugo-send/return (command)
  (interactive "sgnugo command: ")
  (if (not go-gnugo-process)
    (error "no active gnugo process")
    ;; reset result container
    (setq go-gnugo-kept nil)
    ;; send command
    (process-send-string go-gnugo-process command)
    (process-send-string go-gnugo-process "\n")
    ;; wait until gnugo has returned
    (while (not (and go-gnugo-kept
		     (equalp (substring go-gnugo-kept -2) "\n\n")))
      (accept-process-output go-gnugo-process))
    ;; return gnugo's responce
    (substring go-gnugo-kept 2 -2)))

(provide 'go-gnugo)
;;; go-gnugo.el ends here