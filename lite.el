;;; lite.el --- Simple template library for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Arthur Miller

;; Author: Arthur Miller
;; Version: 0.5.0
;; Keywords: tools programming lisp
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/amno1/lite

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

(defvar lite-marker-regex "%%"
  "A regular expression to mark code.")

(defvar lite-escape-char "!"
  "A character used to escape the marker.")

(defun lite-default-filter (object)
  "Default predicate to filter out results before printing them to the buffer."
  (when (or (stringp object) (numberp object)) object))

(defvar lite-print-p #'lite-default-filter
  "A predicate used to decide whether to print the result of evaluation.")

(defun lite-expand-region (search-start search-end)
  "Expand all templates in current buffer."
  (when (= search-end (point-max)) (cl-decf search-end))
  (let ((lite-marker (concat " " lite-marker-regex)))
    (save-excursion
      (goto-char search-start)
      (while (re-search-forward lite-marker-regex search-end t)
        (unless (looking-at-p lite-escape-char)
          (replace-match lite-marker)
          (cl-incf search-end))))
    (when (> search-end search-start)    
      (save-excursion
        (goto-char search-start)
        (while (re-search-forward lite-marker search-end t)
          (if (looking-at-p lite-escape-char)
              (delete-char 1)
            (replace-match "")
            (cl-decf search-end (length lite-marker))
            (let ((beg (point)) (end (progn (forward-sexp) (point))))
              (insert " ")
              (goto-char beg)
              (let* ((sxp (read (current-buffer)))
                     (obj (eval sxp))
                     (res (prin1-to-string obj)))
                (goto-char end)
                (delete-char 1)
                (kill-region beg (point))
                (cl-decf search-end (length (prin1-to-string sxp)))
                (when (funcall lite-print-p obj)
                  (princ obj (current-buffer))
                  (cl-incf search-end (length res)))))))))))

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

(defun lite-generate-from-template (template-file &optional target-file)
  "Generate TARGET-FILE from lite template in TEMPLATE-FILE"
  (let ((auto-insert-mode nil)
        (target (or target-file
                    (expand-file-name (file-name-nondirectory target-file)))))
    (unless (file-exists-p (file-name-directory target))
      (make-directory (file-name-directory target) t))
    (with-temp-buffer
      (with-silent-modifications
        (lite-insert-template template-file)
        (setq buffer-file-name target)
        (lite-expand-region (point-min) (point-max))
        (save-buffer)))))

(defun lite-template-in-line-p ()
  "Whether there is at least one template in a line."
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward lite-marker-regex (line-beginning-position) t)))

(defun lite-expand-dwim ()
  "Expnad template at point in region, all in a line, or all in a buffer."
  (interactive)
  (save-excursion
    (cl-destructuring-bind (beg . end)
        (cond
         ((region-active-p)
          (cons (region-beginning) (region-end)))
         ((lite-template-in-line-p)
          (cons (line-beginning-position) (line-end-position)))
         (t (cons (point-min) (point-max))))
      (lite-expand-region beg end))))

(provide 'lite)
;;; lite.el ends here
