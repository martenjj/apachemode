;;; apache-mode.el --- major mode for Apache configuration files   -*- mode:emacs-lisp -*-

;; Keywords:	languages, faces
;; Author:	Jonathan Marten  <jonathan.marten@uk.sun.com>

;; This file is an add-on for XEmacs or GNU Emacs.
;;
;; It is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; It is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; see the file COPYING.  If not,
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; There isn't really much to say.  The list of keywords was originally
;; derived from the documentation for Apache 1.3 and 2.4; there may be
;; some errors or omissions.
;;
;; There are currently no local keybindings defined, but the hooks are
;; there in the event that anyone gets around to adding any.
;;
;; If this mode is not built and installed together with Emacs, then
;; see the documentation of the `apache-set-file-patterns' function
;; in order to make it available.

;;; Change Log:
;;
;; November 2016               Jonathan Marten <jjm@keelhaul.me.uk>
;;   Attempted to unify the (at least) three versions of this mode.
;;   Generate the lists of keywords from input CSV files for ease of
;;     updating and merging.
;;   Generate autoloads for use if not built with Emacs.
;;
;; 2015-08-23                  David Maus <dmaus@ictsoc.de>
;;   Update list of directives for Apache 2.4
;;
;; 2005-06-29                  Kumar Appaiah <akumar_NOSPAM@ee.iitm.ac.in>
;;   Use syntax table instead of font-lock-keywords to highlight comments.
;;
;; 2004-09-12                  Karl Chen <quarl@nospam.quarl.org>
;;   Rewrote pretty much everything using define-derived-mode; added support
;;   for Apache 2.x; fixed highlighting in GNU Emacs; created indentation
;;   function
;;
;; Version 1.3, May 2002       Peter Brown <rendhalver@xemacs.org>
;;   Updated keywords to include new directives in Apache 2
;;
;; Version 1.2.1, May 2002     Peter Brown <rendhalver@xemacs.org>
;;   Separated Apache directives into sections for easier updating
;;
;; Version 1.2, April 2002     Peter Brown <rendhalver@xemacs.org>
;;   Added mod_ssl 2.8.8, Apache-SSL 1.47 and mod_perl 1.26 keywords.
;;
;; Version 1.1, April 2002     Peter Brown <rendhalver@xemacs.org>
;;   Changed variables to use customise
;;   Updated the keywords to apache 1.3.24
;;   Added apache-file-patterns to save having to hack auto-mode-alist,
;;     added autoloaded function to add apache-file-patterns to auto-mode-alist
;;
;; Version 1.0, October 1999   Jonathan Marten <jonathan.marten@uk.sun.com>
;;   First public release
;;


;;; Code:

;; Requires
(require 'font-lock)
(require 'regexp-opt)
(require 'custom)


;; Variables

;;;###autoload
(defgroup apache nil
  "Major mode for editing Apache configuration files."
  :prefix "apache-"
  :group 'languages)

(defcustom apache-manual-url "http://httpd.apache.org/"
  "*URL at which to find the Apache manual."
  :type 'string
  :group 'apache)

;;;###autoload
(defcustom apache-file-patterns
  (list "\\.htaccess\\(\\.default\\)?$" "httpd\\.conf\\(\\.default\\)?$"
	"srm\\.conf\\(\\.default\\)?$" "access\\.conf\\(\\.default\\)?$"
        "sites-\\(available\\|enabled\\)/")
  "*List of file patterns for which to automatically invoke `apache-mode'."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'apache)

(defcustom apache-mode-hook nil
  "*List of hook functions run by `apache-mode' (see `run-hooks')."
  :type 'hook
  :group 'apache)

(defcustom apache-indent-level (default-value 'tab-width)
  "*Number of spaces to indent per level"
  :type 'integer
  :group 'apache)

(defvar apache-mode-map nil
  "Keymap used in `apache-mode' buffers.")

(defvar apache-mode-syntax-table nil
  "Syntax table used in `apache-mode' buffers.")


;; Keyword lists
;;
;; These are inserted by the build script from the input CSV files.
;; They are kept as separate Lisp variable definitions from the
;; font lock data, even though that may lead to more memory use,
;; in order that they may be able to be used for other purposes
;; (e.g. completion).

;; @KEYWORDS@ apache-mode-sections sections.csv
;; @KEYWORDS@ apache-mode-keywords keywords.csv
;; @KEYWORDS@ apache-mode-values   values.csv


;; Font lock
(defconst apache-font-lock-keywords
  (purecopy
   (list
    ; syntax table is used for comment highlighting
    ;(list "^\\s-*#.*$" 0 'font-lock-comment-face t)

    (list (concat                                       ; sections
	   "^\\s-*</?\\("
           (regexp-opt apache-mode-sections)
           "\\)\\(>\\|\\s-\\)")
	  1 'font-lock-function-name-face)

    (list (concat                                       ; keywords
	   "^\\s-*\\("
           (regexp-opt apache-mode-keywords)
           "\\)\\>")
	  1 'font-lock-keyword-face)

    (list (concat                                       ; values
	   "\\<\\("
           (regexp-opt apache-mode-values)
           "\\)\\>")
	  1 'font-lock-type-face)))
  "Expressions to highlight in `apache-mode' buffers.")


;;;###autoload
(defun apache-mode ()
  "Major mode for editing Apache configuration files.

\\{apache-mode-map}

\\[apache-mode] runs the hook `apache-mode-hook'."
  (interactive)
  (kill-all-local-variables)

  (setq mode-name "Apache")
  (setq major-mode 'apache-mode)
  (use-local-map apache-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(apache-font-lock-keywords nil t
                                                       ((?_ . "w")
                                                        (?- . "w")
                                                        (?/ . "w"))
                                                       beginning-of-line))
  (unless apache-mode-syntax-table
    (setq apache-mode-syntax-table (copy-syntax-table nil))
    (modify-syntax-entry ?_   "_"     apache-mode-syntax-table)
    (modify-syntax-entry ?-   "_"     apache-mode-syntax-table)
    (modify-syntax-entry ?\(  "(\)"   apache-mode-syntax-table)
    (modify-syntax-entry ?\)  ")\("   apache-mode-syntax-table)
    (modify-syntax-entry ?\<  "(\>"   apache-mode-syntax-table)
    (modify-syntax-entry ?\>  ")\<"   apache-mode-syntax-table)
    (modify-syntax-entry ?\"  "\""    apache-mode-syntax-table)
    (modify-syntax-entry ?,   "."     apache-mode-syntax-table)
    (modify-syntax-entry ?#   "<"     apache-mode-syntax-table)
    (modify-syntax-entry ?\n  ">"     apache-mode-syntax-table))
  (set-syntax-table apache-mode-syntax-table)

  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#\\W*")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'apache-indent-line)

  (run-hooks 'apache-mode-hook))


;; Indentation
(defun apache-indent-line ()
   "Indent the current line of Apache code.
Uses `apache-indent-level' to set the indentation spacing."
   (interactive)
   (let ((savep (> (current-column) (current-indentation)))
	 (indent (max (apache-calculate-indentation) 0)))
     (if savep
	 (save-excursion (indent-line-to indent))
       (indent-line-to indent))))

(defun apache-previous-indentation ()
  "Return the previous (non-empty/comment) indentation.
Doesn't save position."
  (let (indent)
    (while (and (null indent)
                (zerop (forward-line -1)))
      (unless (looking-at "[ \t]*\\(#\\|$\\)")
        (setq indent (current-indentation))))
    (or indent 0)))

(defun apache-calculate-indentation ()
  "Return the amount the current line should be indented."
  (save-excursion
    (forward-line 0)
    (if (bobp)
        0
      (let ((ends-section-p (looking-at "[ \t]*</"))
            (indent (apache-previous-indentation)) ; moves point!
            (previous-starts-section-p (looking-at "[ \t]*<[^/]")))
        (if ends-section-p
            (setq indent (- indent apache-indent-level)))
        (if previous-starts-section-p
            (setq indent (+ indent apache-indent-level)))
        indent))))


;;;###autoload
(defun apache-set-file-patterns ()
  "Set file name patterns which automatically invoke `apache-mode'.
If you are using `apache-mode' installed separately from Emacs (i.e.
if its autoloads have not been automatically processed), then
add the following to your init file (see `user-init-file'):

  (add-to-list 'load-path \"/directory/where/it/is/installed\")
  (load \"apache-mode-autoloads\")

The file name patterns are taken from the variable `apache-file-patterns'."
  (mapcar (function (lambda (pat)
                      (add-to-list 'auto-mode-alist (cons pat 'apache-mode))))
          apache-file-patterns))

;;;###autoload
(apache-set-file-patterns)


;; Provides
(provide 'apache-mode)

;;; apache-mode.el ends here
