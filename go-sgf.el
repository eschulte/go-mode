;;; go-sgf.el --- reading, writing, browsing of sgf files as lisp trees

;; Copyright (C) 2008 Eric Schulte

;; This file builds on sgftree.el by Shawn Betts
;; Copyright (C) 2005,2007  Shawn Betts

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

;;; Commentary:

;; Translate between text sgf files, and sgftree-node objects.  Only
;; tested on GO files.  Should be compiled

;;; Code:

(require 'cl)

(defstruct sgf-node
  children alist)

(defun go-sgf-clean ()
  "Some sgf files seem to have spurious ^M's in them."
  (save-excursion
    (while (search-forward "" nil t)
      (replace-match "" nil t))))

(defun go-sgf-looking-at (re &optional move-pt)
  "like looking-at, but when MOVE-PT is T, then move the point to the end of the match."
  (let ((ret (looking-at re)))
    (when (and ret move-pt)
      (goto-char (match-end 0)))
    ret))

(defun go-sgf-read-element ()
  (when (go-sgf-looking-at (rx (1+ (and "[" (0+ (not (any "]"))) "]" (0+ white)))) t)
    (butlast (cdr (split-string (match-string 0) (rx (1+ (or "]" "[" (and "]" (1+ "\n"))))))))))

(defun go-sgf-read-node ()
  ;; gobble node marker
  ;; Some sgf files have more than 1 ; in a row.
  (go-sgf-looking-at (rx (1+ ";" (0+ white))) t)
  (make-sgf-node :alist (loop while (go-sgf-looking-at "[A-Z]+[[:space:]]*" t)
				  collect (cons (match-string 0) (go-sgf-read-element))
				  do (go-sgf-looking-at (rx (0+ white)) t))
		     :children (loop while (looking-at "(")
				     collect (go-sgf-read-game-tree))))

(defun go-sgf-read-game-tree ()
  (go-sgf-looking-at (rx "(" (0+ white)) t)
  ;; some sgf writers don't seem to put a ; after the top level
  ;; (. So lets add it for them.
  (when (not (looking-at ";"))
    (insert ";")
    (backward-char))
  (prog1
      (loop while (looking-at ";")
	    collect (go-sgf-read-node)
	    do (go-sgf-looking-at (rx (0+ white)) t))
      (go-sgf-looking-at (rx ")" (0+ white)) t)))

(defun go-sgf-element-to-string (el)
  ;; return a sgf string representation of the alist of a `sgf-node'
  (concat (car el)
	  (apply 'concat 
		 (mapcar (lambda (el)
			   (format "[%s]" el)) (cdr el))) " "))

(defun go-sgf-node-to-string (node)
  ;; Return the sgf string representation of a `sgf-node'
  (if (sgf-node-p node)
      (concat
       "(;"
       (apply 'concat (mapcar 'go-sgf-element-to-string
			      (sgf-node-alist node)))
       (apply 'concat (mapcar (lambda (el)
			 (go-sgf-node-to-string (car el)))
		       (sgf-node-children node)))
       ")")))
  
;;;###autoload
(defun go-sgf-parse-file (file)
  "Parse an sgf file and return the contents in tree form."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (go-sgf-read-game-tree)))

;;;###autoload
(defun go-sgf-write-to-file (sgftree file)
  "Write the go game represented in SGFTREE to FILE in sgf format."
  (interactive)
  (with-temp-buffer
     (insert (apply 'concat (mapcar 'go-sgf-node-to-string sgftree)))
     (when (file-writable-p file)
       (write-region (point-min)
                     (point-max)
                     file))))

;;--------------------------------------------------------------------------------
;;; Browsing

(defvar go-sgf-current-node
  nil
  "The current node in the sgftree being browsed")

(defvar go-sgf-current-element-pointer
  0
  "The current element in the current sgf node")

(defvar go-sgf-current-parents
  nil
  "A stack of the parents which have been traversesd.")

(defvar go-sgf-current-siblings
  '(nil . nil)
  "A cons cell representing the siblings s.t. the car is the left
  siblings, and the cdr is the right siblings")

(defun go-sgf-browse-tree (tree)
  "Begin browsing TREE at it's first node"
  (setf go-sgf-current-element-pointer 0)
  (setf go-sgf-current-parents nil)
  (setf go-sgf-current-siblings '(nil . nil))
  (setf go-sgf-current-node
	(if (sgftree-node-p tree)
	    tree
	  (if (sgftree-node-p (car tree))
	      (car tree)
	    (error "TREE does not appear to be an sgftree.\n%S" tree)))))

(defun go-sgf-current-element ()
  (nth go-sgf-current-element-pointer (sgftree-node-alist go-sgf-current-node)))

(defun go-sgf-next-element ()
  (interactive)
  (setf go-sgf-current-element-pointer (1+ go-sgf-current-element-pointer))
  (or (go-sgf-current-element)
      (error "Final Element")))

(defun go-sgf-previous-element ()
  (interactive)
  (setf go-sgf-current-element-pointer (1- go-sgf-current-element-pointer))
  (or (go-sgf-current-element)
      (error "Final Element")))

(defun go-sgf-next-variation ()
  (interactive)
  ;; first look for right siblings
  (if (cdr go-sgf-current-siblings)
      (progn
	(push go-sgf-current-node (car go-sgf-current-siblings))
	(setf go-sgf-current-node (car (pop (cdr go-sgf-current-siblings)))))
    ;; then children
    (let ((siblings (sgftree-node-children go-sgf-current-node)))
      (if (not siblings)
	  (error "No more variations")
	;; update parents, and siblings pointers
	(push go-sgf-current-node go-sgf-current-parents)
	(setf go-sgf-current-siblings (copy-tree (sgftree-node-children go-sgf-current-node)))
	(setf go-sgf-current-node (pop (car go-sgf-current-siblings))))))
  (setf go-sgf-current-element-pointer 0)
  (go-sgf-current-element))

(defun go-sgf-previous-variation ()
  (interactive)
  ;; first check for left siblings
  (if (car go-sgf-current-siblings)
      (progn
	(push go-sgf-current-node (cdr go-sgf-current-siblings))
	(setf go-sgf-current-node (pop (car go-sgf-current-siblings))))
    ;; then parents
    (let ((parent (pop go-sgf-current-parents)))
      (if (not parent)
	  (error "No Previous Variations")
	;; update parents, and siblings pointers
	(message "updating parent and sibling pointers")
	(setf go-sgf-current-node parent)
	(setf go-sgf-current-siblings
	      (let ((grandparent (car go-sgf-current-parents)))
		;; if new node has siblings
		(if (and grandparent
			 (sgftree-node-children grandparent))
		    (cons nil (copy-tree (sgftree-node-children grandparent))))))
	;; scroll through children till we find our current node
	(if go-sgf-current-siblings
	    (loop
	     with child = nil
	     until (eq child go-sgf-current-node)
	     do
	     (if child
		 (push child (car go-sgf-current-siblings)))
	     (setf child (car (pop (cdr go-sgf-current-siblings)))))))))
  (setf go-sgf-current-element-pointer 0)
  (go-sgf-current-element))

(provide 'go-sgf)
;;; go-sgf.el ends here