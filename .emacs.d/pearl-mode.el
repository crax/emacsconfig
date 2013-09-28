;;; pearl-mode-el -- Major mode for editing PEARL files

;; Author: Scott Andrew Borton <scott@pp.htv.fi>
;; Created: 25 Sep 2000
;; Keywords: PEARL major-mode

;; Copyright (C) 2000, 2003 Scott Andrew Borton <scott@pp.htv.fi>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;; 
;; This mode is an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html
;;(regexp-opt '("class" "evt" "fn" "package" "with" "evth") t)
;;; Code:
(defvar pearl-mode-hook nil)
(defvar pearl-mode-map
  (let ((pearl-mode-map (make-keymap)))
    (define-key pearl-mode-map "\C-j" 'newline-and-indent)
    pearl-mode-map)
  "Keymap for PEARL major mode")

(add-to-list 'auto-mode-alist '("\\(\\.\\(?:\\(?:id\\|pear\\)l\\)\\)" . pearl-mode))

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(defconst pearl-font-lock-keywords-1
  (list
																				; These define the beginning and end of each PEARL entity definition
																				; "PARTICIPANT" "END_PARTICIPANT" "MODEL" "END_MODEL" "WORKFLOW"
																				; "END_WORKFLOW" "ACTIVITY" "END_ACTIVITY" "TRANSITION"
																				; "END_TRANSITION" "APPLICATION" "END_APPLICATION" "DATA" "END_DATA"
																				; "TOOL_LIST" "END_TOOL_LIST"
   ""
   '("\\<\\(class\\|evth?\\|fn\\|package\\|var\\|with\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for PEARL mode.")

(defconst pearl-font-lock-keywords-2
  (append pearl-font-lock-keywords-1
					(list
																				; These are some possible attributes of PEARL entities
																				; "PEARL_VERSION" "VENDOR" "CREATED" "NAME" "DESCRIPTION"
																				; "AUTHOR" "STATUS" "EXTENDED_ATTRIBUTE" "TYPE" "TOOLNAME"
																				; "IN_PARAMETERS" "OUT_PARAMETERS" "DEFAULT_VALUE"
																				; "IMPLEMENTATION" "PERFORMER" "SPLIT" "CONDITION" "ROUTE"
																				; "JOIN" "OTHERWISE" "TO" "FROM"
					 '("\\<\\(bool\\|co\\(?:\\(?:mmen\\|ns\\)t\\)\\|f\\(?:alse\\|loat\\)\\|int\\|st\\(?:atic\\|ring\\)\\|true\\|void\\)\\>" . font-lock-keyword-face)
					 ))
  "Additional Keywords to highlight in PEARL mode.")

;; (defconst pearl-font-lock-keywords-3
;;   (append pearl-font-lock-keywords-2
;; 		  (list
;; 		 ; These are some possible built-in values for PEARL attributes
;; 			 ; "ROLE" "ORGANISATIONAL_UNIT" "STRING" "REFERENCE" "AND"
;; 			 ; "XOR" "WORKFLOW" "SYNCHR" "NO" "APPLICATIONS" "BOOLEAN"
;; 							 ; "INTEGER" "HUMAN" "UNDER_REVISION" "OR"

;; 		   '("\\<\\(AND)\\>" . font-lock-constant-face)))
;;   "Balls-out highlighting in PEARL mode.")


;; (setq tmp (regexp-opt '("class" "evt" "evth" "fn" "with" "package" "var") t))
;; (setq tmp2 (regexp-opt '("bool" "comment" "const" "static" "false" "true" "int" "string" "float" "void") t))
;; (setq tmp (regexp-opt '("Fuck" "For" "class" "nidaye") t))
;;(setq tmp (regexp-opt '("{") t))
;; (setq tmp2 3)
;; (message "SUCK %s" tmp2)
;; "\\(F\\(?:or\\|uck\\)\\|class\\|nidaye\\)"

(defvar pearl-font-lock-keywords pearl-font-lock-keywords-2
  "Default highlighting expressions for PEARL mode.")

(defvar foo-indent-offset 2
  "*Indentation offset for `pearl-mode'.")

(defun foo-indent-line ()
  "Indent current line for `pearl-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[({]")
              (setq indent-col (+ indent-col foo-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[])}]") (>= indent-col foo-indent-offset))
        (setq indent-col (- indent-col foo-indent-offset))))
    (indent-line-to indent-col)))

(defun pearl-indent-line ()
  "Indent current line as PEARL code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
			(indent-line-to 0)		   ; First line is always non-indented
		(let ((not-indented t) cur-indent)
			(if (looking-at "^[ \t]*}") ; If the line we are looking at is the end of a block, then decrease the indentation
					(progn
						(save-excursion
							(forward-line -1)
							(setq cur-indent (- (current-indentation) default-tab-width)))
						(if (< cur-indent 0) ; We can't indent past the left margin
								(setq cur-indent 0)))
				(save-excursion
					(while not-indented ; Iterate backwards until we find an indentation hint
						(forward-line -1)
						(if (looking-at "^[ \t]*}") ; This hint indicates that we need to indent at the level of the END_ token
								(progn
									(setq cur-indent (current-indentation))
									(setq not-indented nil))
							(if (looking-at "^[ \t]*\\(class\\|fn\\|package\\)") ; This hint indicates that we need to indent an extra level
									(progn
										(setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
										(setq not-indented nil))
								(if (bobp)
										(setq not-indented nil)))))))
			(if cur-indent
					(indent-line-to cur-indent)
				(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar pearl-mode-syntax-table
  (let ((pearl-mode-syntax-table (make-syntax-table)))
		
																				; This is added so entity names with underscores can be more easily parsed
		(modify-syntax-entry ?_ "w" pearl-mode-syntax-table)
		
																				; Comment styles are same as C++
		(modify-syntax-entry ?/ ". 124b" pearl-mode-syntax-table)
		(modify-syntax-entry ?* ". 23" pearl-mode-syntax-table)
		(modify-syntax-entry ?\n "> b" pearl-mode-syntax-table)
		(modify-syntax-entry ?# "< b" pearl-mode-syntax-table)
		;; (modify-syntax-entry ?\( ". w" pearl-mode-syntax-table)
		;; (modify-syntax-entry ?\) ". w" pearl-mode-syntax-table)
		;; (modify-syntax-entry ?\[ ". w" pearl-mode-syntax-table)
		;; (modify-syntax-entry ?\] ". w" pearl-mode-syntax-table)
		;; (modify-syntax-entry ?\{ ". w" pearl-mode-syntax-table)
		;; (modify-syntax-entry ?\} ". w" pearl-mode-syntax-table)
		pearl-mode-syntax-table)
  "Syntax table for pearl-mode")

(defun pearl-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map pearl-mode-map)
  (set-syntax-table pearl-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(pearl-font-lock-keywords))
  ;; Register our indentation function
	(set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'foo-indent-line)  
;;  (set (make-local-variable 'indent-line-function) 'pearl-indent-line)  
  (setq major-mode 'pearl-mode)
  (setq mode-name "PEARL")
  (run-hooks 'pearl-mode-hook))

(provide 'pearl-mode)

;;; pearl-mode.el ends here