;;; lite-templates.el --- Simple template library for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: tools, lisp, editing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'text-property-search)

(defgroup lite nil
  "Simple template system for Emacs Lisp"
  :prefix "lite-"
  :group 'editing)

(defcustom lite-template-dirs (list (expand-file-name "templates/" user-emacs-directory))
  "List of top-level template directories.

Each element, a string or a symbol whose value is a string,
designates a top-level directory containing lite templates.

Elements appearing earlier in the list override later elements'
snippets."
  :type '(choice (directory :tag "Single directory")(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)))
                 (repeat :tag "List of directories"
                         (choice (directory) (variable)))))

(defcustom lite-ignore-trailing-spaces t
  "Whether to skip whitespace characters before and after the code."
  :type 'boolean)

(defvar lite-begin-regex "{{"
  "Start delimiter for a lite template.")
(defvar lite-end-regex "}}"
  "End delimiter for a lite template.")

(defun lite--default-eval-function (expression)
  "Evaluate EXPRESSION."
  ;;(call-interactively #'eval-last-sexp)
  (eval expression))

(defun lite--default-print-function (object)
  "Print srings and numbers with `princ' to `current-buffer'."
  (when (or (stringp object) (numberp object))
    (princ object (current-buffer))))

(defun lite--default-read ()
  "Move point one expression at a time within a single template."
  (condition-case nil
      (let* ((expr (read (current-buffer))) text)
        (when (symbolp expr)
          (setq text (symbol-name expr))
          (cond ((= (aref text 0) ?&)
                 (setq expr (format "%s" (read (current-buffer)))))
                ((= (aref text 0) ?!)
                 (setq expr (if (= 1 (length text))
                                (format "!%s" (read (current-buffer)))
                              (format "%s" expr))))
                (t expr)))
        expr)
    (error nil)))

(defvar lite-read-fn #'lite--default-read
  "A hook called to iterate over each expression in a template.")
(defvar lite-eval-fn #'lite--default-eval-function
  "A hook called to evaluate a single expression in a template.")
(defvar lite-print-fn #'lite--default-print-function
  "A hook called to print a result of evaluating a single expression.")

(defun lite--search-end ()
  (let ((match
         (save-excursion (text-property-search-forward 'template-search-end))))
    (when match (prop-match-end match))))

(defun lite--expand-template ()
  "Expand a single template with borders at BEG and END."
  (let* ((beg-code (point))
         (beg-template (- (point) (length lite-begin-regex)))
         (end-template (re-search-forward lite-end-regex (lite--search-end)))
         (end-code (- end-template (length lite-end-regex)))
         results)
    (narrow-to-region beg-code end-code)
    (goto-char (point-min))
    (when lite-ignore-trailing-spaces
      (skip-chars-forward " \n\t\r\v"))
    (while (not (eobp))
      (let ((beg (point)) sxp)
        (skip-chars-forward " \n\t\r\v")
        (when (> (point) beg)
          (cl-pushnew (buffer-substring beg (point)) results))
        (when (setq sxp (funcall lite-read-fn))
          (cl-pushnew (funcall lite-eval-fn sxp) results))))
    (widen)
    (kill-region beg-template end-template)
    (dolist (result (nreverse results))
      (funcall lite-print-fn result))
    (when lite-ignore-trailing-spaces
      (let ((beg (point)))
        (skip-chars-backward " \n\t\r\v")
        (kill-region (point) beg)))))

(defun lite--template-at-point ()
  "Return the template at point."
  (save-excursion
    (re-search-backward lite-begin-regex)
    (let ((beg (point))
          (end (save-excursion (re-search-forward lite-end-regex)))
          content)
      (forward-char (length lite-begin-regex))
      (when (or (= (char-after) ?!) (= (char-after) ?@) (= (char-after) ?&))
        (forward-char))
      (setq content
            (buffer-substring-no-properties
             (point) (save-excursion
                       (- (re-search-forward lite-end-regex)
                          (length lite-end-regex)))))
      (pcase (char-before)
        (?! (list beg end ""))
        (?@ (list beg end content))
        (?& (list beg end (concat lite-begin-regex content lite-end-regex)))))))

(defun lite--insert-self ()
  "Insert template at point without self-referencing character."
  (let ((tmp (lite--template-at-point)))
    (kill-region (car tmp) (cadr tmp))
    (funcall lite-print-fn (caddr tmp))))

(defun lite-expand-region (search-start search-end)
  "Expand all templates in current buffer."
  (save-excursion
    (goto-char search-start)
    (switch-to-buffer (current-buffer))
    (when (= search-end (point-max)) (cl-decf search-end))
    (put-text-property search-end (1+ search-end) 'template-search-end t)
    (while (re-search-forward lite-begin-regex (lite--search-end) t)
      (pcase (char-after)
        (?! (lite--insert-self))
        (?& (lite--insert-self))
        (?@ (lite--insert-self))
        (_  (lite--expand-template))))
    (goto-char (point-min))
    (remove-text-properties (point-min) (point-max) '(template-search-end))))

(defun lite-insert-template (template-file-name)
  "Insert contents of TEMPLATE-FILE-NAME into `current-buffer'"
  (let* ((name template-file-name)
         (template
          (catch 'found
            (dolist (dir lite-template-dirs)
              (ignore-errors
                (let ((template (directory-files-recursively dir name nil t)))
                  (when template (throw 'found template))))))))
    (if template
        (insert-file-contents (car template))
      (error "Can't find template file for: %s" name))))

(defun lite-in-template-p ()
  "Wheter the cursor is in a template."
  (and
   (save-excursion
     (re-search-backward lite-begin-regex (line-beginning-position) t))
   (save-excursion (re-search-forward lite-end-regex (line-end-position) t))))

(defun lite-template-in-line-p ()
  "Whether there is at least one template in a line."
  (save-excursion
    (goto-char (line-end-position))
    (and (re-search-backward lite-begin-regex (line-beginning-position) t)
         (re-search-forward lite-end-regex (line-end-position) t))))

(defun lite-generate-from-template (template-file &optional target-file)
  "Generate TARGET-FILE from lite template in TEMPLATE-FILE"
  (let ((target (or target-file
                    (expand-file-name (file-name-nondirectory target-file)))))
    (with-current-buffer (find-file-noselect target)
      (erase-buffer)
      (lite-insert-template template-file)
      (lite-expand-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer))))

(defun lite-expand-dwim ()
  "Expnad template at point in region, all in a line, or all in the entire
file."
  (interactive)
  (save-excursion
    (cl-destructuring-bind (beg . end)
        (cond
         ((region-active-p)
          `(,(region-beginning) . ,(region-end)))
         ((lite-template-in-line-p)
          `(,(line-beginning-position) . ,(line-end-position)))
         (t `(,(point-min) . ,(point-max))))
      (lite-expand-region beg end))))

(provide 'lite-templates)
;;; lite-templates.el ends here

