;;; mrd-org-smartwrap.el --- Smart word-wrapping for org-mode
;; Matthew Dempsky
;;
;; Derived from org-indent.el:
;;
;;     Copyright (C) 2009 Free Software Foundation, Inc.
;;
;;     Author: Carsten Dominik <carsten at orgmode dot org>
;;     Keywords: outlines, hypermedia, calendar, wp
;;     Homepage: http://orgmode.org
;;     Version: 6.33x
;;
;;     This file is part of GNU Emacs.
;;
;;     GNU Emacs is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     GNU Emacs is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; XXX: Somehow re-enable truncate-lines for lines containing tables.

(require 'org-macs)
(require 'org-compat)
(require 'org)
(eval-when-compile
  (require 'cl))

(defconst mrd-org-smartwrap-max 40
  "Maximum indentation level")

(defvar mrd-org-smartwrap-strings nil
  "Vector with all prefix strings.
It will be set in `mrd-org-smartwrap-initialize'.")

(defun mrd-org-smartwrap-initialize ()
  "Initialize the prefix strings."
  (unless mrd-org-smartwrap-strings
    (setq mrd-org-smartwrap-strings
	  (make-vector (1+ mrd-org-smartwrap-max) nil))
    (loop for i from 1 to mrd-org-smartwrap-max do
	  (aset mrd-org-smartwrap-strings i (make-string i ?\ )))))

;;;###autoload
(define-minor-mode mrd-org-smartwrap-mode
  "When active, wrap text according to outline and list structure.

This mode is intended to be used with `visual-line-mode'.
Internally this works by adding `wrap-prefix' properties on
section headlines and plain list entries."
  nil " sw" nil
  (if (org-bound-and-true-p org-inhibit-startup)
      (setq mrd-org-smartwrap-mode nil)
    (if mrd-org-smartwrap-mode
	(progn
	  (mrd-org-smartwrap-initialize)
	  (make-local-variable 'buffer-substring-filters)
	  (add-hook 'buffer-substring-filters
		    'mrd-org-smartwrap-remove-properties-from-string nil t)
	  (add-hook 'after-change-functions
		    'mrd-org-smartwrap-after-change nil t)
	  (save-restriction
	    (widen)
	    (mrd-org-smartwrap-add-properties (point-min) (point-max))))
      (save-restriction
	(widen)
	(mrd-org-smartwrap-remove-properties (point-min) (point-max)))
      (remove-hook 'buffer-substring-filters
		   'mrd-org-smartwrap-remove-properties-from-string t)
      (remove-hook 'after-change-functions
		   'mrd-org-smartwrap-after-change t))))

;; The regexps here are based on org-outline-regexp and the regexps
;; from org-adaptive-fill-function.

(defconst mrd-org-smartwrap-outline-re
  "^\\(\\*+ \\|[ \t]*\\([-*+] \\|[0-9]+[.)] \\)\\)"
  "Outline or plain list regexp.")

(defun mrd-org-smartwrap-add-properties (beg end)
  "Add `wrap-prefix' properties between BEG and END.
Assumes that BEG is at the beginning of a line."
  (org-unmodified
   (save-excursion
     (save-match-data
       (goto-char beg)
       (while (re-search-forward mrd-org-smartwrap-outline-re end t)
	 (beginning-of-line)
	 ;; XXX: For definitions with terms that span multiple lines,
	 ;; the result is somewhat peculiar; namely, the rest of the
	 ;; term is indented 5 spaces as well.  It might make sense to
	 ;; indent all of the term (up to and including the " :: ") just
	 ;; past the bullet point, and then only indent the rest of the
	 ;; string any further.  Not sure.

	 ;; XXX: Doesn't support org-description-max-indent being
	 ;; ridiculously large... probably doesn't need to.

	 ;; XXX: Doesn't support numbered definition lists: this matches
	 ;; M-q's behavior, but not the syntax highlighting...
	 (let ((level (if (and (looking-at "[ \t]*\\([-*+] .*? :: \\)")
			       (> (- (match-end 1) (match-beginning 1))
				  org-description-max-indent))
			  (- (+ (match-beginning 1) 5) (match-beginning 0))
			(- (match-end 0) (match-beginning 0)))))
	   (set-text-properties
	    (point-at-bol) (point-at-eol)
	    `(wrap-prefix ,(aref mrd-org-smartwrap-strings level))))
	 (end-of-line))))))

(defun mrd-org-smartwrap-remove-properties (beg end)
  "Remove `wrap-prefix' properties between BEG and END."
  (org-unmodified
   (remove-text-properties beg end '(wrap-prefix nil))))

(defun mrd-org-smartwrap-remove-properties-from-string (string)
  "Remove `wrap-prefix' properties between BEG and END."
  (remove-text-properties 0 (length string)
			  '(wrap-prefix nil) string)
  string)

(defun mrd-org-smartwrap-refresh-region (beg end)
  "Refresh `wrap-prefix' properties between BEG and END.
Assumes that BEG is at the beginning of a line."
  (mrd-org-smartwrap-remove-properties beg end)
  (mrd-org-smartwrap-add-properties beg end))

(defun mrd-org-smartwrap-after-change (beg end len)
  "Refresh `wrap-prefix' properties between BEG and END."
  (mrd-org-smartwrap-refresh-region
   (save-excursion (goto-char beg) (point-at-bol))
   (save-excursion (goto-char end) (point-at-eol))))

(provide 'mrd-org-smartwrap)

;;; mrd-org-smartwrap.el ends here
