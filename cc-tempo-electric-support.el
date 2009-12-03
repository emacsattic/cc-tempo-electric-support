;;; cc-tempo-electric-support.el --- provides support for insertion of
;; electric characters in cc-mode using tempo
;; $Revision: 1.1 $
;; $Date: 2000/02/17 12:49:19 $

;; This file is not part of Emacs

;; Author: Klaus Berndl <Klaus.Berndl@sdm.de>
;; Maintainer: This is a little unclear at the moment

;; Copyright (c) 2000 Klaus Berndl <Klaus.Berndl@sdm.de>

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  This package provides support for insertion of electric characters
;;  with the tempo package into cc-mode buffers.  The current problem
;;  with tempo is that it requires hard coding in the templates where
;;  braces, and such forth should go, whilst cc-mode is perfectly
;;  capable of making these decisions automatically.  This package
;;  allows tempo to insert these characters and have cc-mode behave
;;  appropriately.
;;
;;  In practice what this means is that instead of putting "{" into
;;  your tempo template, you put (i ?{) (this should NOT be quoted, as
;;  tempo needs to evaluate it!). If you have switched on the auto
;;  electric mode within cc-mode it will now newline and reindent this
;;  "{" depending on your indentation settings.


;; this is an example of a complete template where all electric charcters
;; of cc-mode (here: (){};) will be inserted with the new i-element of this
;;package.

;; old version without electric characters processing
;;(tempo-define-template "c-for-i"
;;                       '(> "for (" (p "variable: " var) " = 0; " (s var)
;;                           " < "(p "upper bound: " ub)"; " (s var) "++)" > n>
;;                           "{" > n> r> n
;;                           "}"
;;                           )
;;                       "fori"
;;                       "Insert a C for loop: for (x = 0; x < ..; x++)"
;;                       'c-tempo-tags)

;; new-version with electric characters processing
;;(tempo-define-template "c-for-i"
;;      '(> "for " (i ?\() (p "variable: " var) " = 0" (i ?\;) " " (s var)
;;          " < " (p "upper bound: " ub) (i ?\;) " " (s var) "++"
;;          (i ?\)) " " >
;;          (i ?{) r> n>
;;          (i ?})
;;          )
;;      "fori"
;;      "Insert a C for loop: for (x = 0; x < ..; x++)"
;;      'c-tempo-tags)


;;; Installation
;;
;;  Place this file into your load-path, and add this to your .emacs
;;  (require 'cc-tempo-electric-support)


;;; History:
;; 

(require 'tempo)
(require 'cc-mode)

;;; Code:
(defun cc-tempo-process-character-for-insert (character)
  "Check if CHARACTER is bound to `self-insert-command' and insert it if yes.
But if bound to any other function `last-command-char' will be set to
CHARACTER and the bounded function will be called with nil as argument or -
if this fails without any argument.  This is very usefull if you want to
insert charcters from a lisp-program and still having the key-bindung of
this character in current mode active.  For example you can insert
characters with it´s special electric meaning in a certain mode
(e.g. inserting a '{' with its electric behaviour)."
  (let* ((old-last-command-char last-command-char)
	 (defn (key-binding (char-to-string character)))
	 (is-self-insert (if (string-match "self-insert-command"
					   (symbol-name defn)) t nil))
	 (arglist (list 1)))
    (if is-self-insert
	(setq defn 'self-insert-command)
      (setq arglist (list nil)))
    (setq last-command-char character)
    (condition-case nil
	(apply defn arglist)
      ;; some commands (like newline-and-indent) must not be called
      ;; with any argument!
      (wrong-number-of-arguments
       (apply defn nil)))
    (setq last-command-char old-last-command-char)))



(defun cc-tempo-process-insert (elem)
  "Process the user element (i <something>).
The 'something' is one of the following: string: insert the string
integer: process the integer with `process-character-for-insert'
number: insert the number as string lisp-expression of a
function-call: Evaluate the function other: Insertion of \"CAN NOT
INSERT\".  Returns nil if ELEM != (i <something>) otherwise '(not t)
to avoid an endless recursion-loop (look at 'tempo-insert)"
  (if (and (consp elem)
	   (eq (car elem) 'i))
      (let ((insertion (car (cdr elem)))
	    (default-ins (concat comment-start "CAN NOT INSERT"
				 comment-end))
	    (ret-val '(not t)))
	(cond ((stringp insertion) (insert insertion))
	      ((integerp insertion) (cc-tempo-process-character-for-insert insertion))
	      ((numberp insertion) (insert (number-to-string insertion)))
	      ((and (symbolp insertion) (fboundp insertion))
	       (apply insertion (cddr insertion)))
	      (t (insert default-ins)
		 (setq ret-val 'n>)))
	ret-val)
    nil))
 
;; add to user-handlers for user-elements
(add-to-list 'tempo-user-elements 'cc-tempo-process-insert)

(provide 'cc-tempo-electric-support)

;;; cc-tempo-electric-support.el ends here
