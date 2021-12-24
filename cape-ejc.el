;;; cape-ejc.el -- SQL completitions at point by company-mode (the part of ejc-sql).

;;; Copyright © 2020 - Kostafey <kostafey@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Commentary:

;; `cape-ejc' is a `company' completion backend for `ejc-sql'.
;; To use it, add `cape-ejc-backend' to `company-backends':

;;     (requre 'cape-ejc)
;;     (push 'cape-ejc-backend company-backends)

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'company)
(require 'ejc-completion-common)

(defvar ejc-sql-mode-keymap)

(defvar cape-ejc--properties
  (list :annotation-function #'cape-ejc--annotation
        :company-kind #'cape-ejc--kind)
  "Completion extra properties for `cape-ejc'.")

(defun cape-ejc--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun cape-ejc--add-meta (meta candidates)
  (-map (lambda (k) (list k meta))
        candidates))

(defun cape-ejc--candidates (str)
  (let* ((prefix-1 (ejc-get-prefix-word))
         (prefix-2 (save-excursion
                     (search-backward "." nil t)
                     (ejc-get-prefix-word)))
         (str (or (cape-ejc--grab-symbol) str))
         (res))
    (dolist (item
             (cl-remove-if-not
              (lambda (c) (string-prefix-p str (car c) t))
              (append
               (ejc-append-without-duplicates
                (cape-ejc--add-meta
                 "ansi sql" (ejc-get-ansi-sql-words))
                (cape-ejc--add-meta
                 "keyword" (ejc-get-keywords))
                'car :right)
               (cape-ejc--add-meta
                "owner" (ejc-owners-candidates))
               (cape-ejc--add-meta
                "table" (ejc-tables-candidates))
               (cape-ejc--add-meta
                "view" (ejc-views-candidates))
               (if (not prefix-1)
                   (cape-ejc--add-meta
                    "package" (ejc-packages-candidates)))
               (cape-ejc--add-meta
                "column" (ejc-colomns-candidates)))))
      (push (cape-ejc--make-candidate item) res))
    res))

(defun cape-ejc--annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun cape-ejc--kind (candidate)
  (let ((meta (get-text-property 0 'meta candidate)))
    (if (require 'kind-icon nil 'noerror)
        (pcase meta
          ("ansi sql"
           'operator)
          ("keyword"
           'keyword)
          ("owner"
           'owner)
          ("table"
           'table)
          ("view"
           'view)
          ("package"
           'package)
          ("column"
           'column))
      "text")))

(when (require 'kind-icon nil 'noerror)
  (dolist (kind  '((owner "on" :icon "account-arrow-down" :face font-lock-function-name-face)
                   (table "t" :icon "table" :face font-lock-builtin-face)
                   (view "vi" :icon "card-search" :face font-lock-type-face)
                   (package "pk" :icon "package" :face font-lock-builtin-face)
                   (column "co" :icon "table-column" :face font-lock-function-name-face)))
    (cl-pushnew kind kind-icon-mapping :test #'cl-equalp)))

(defun cape-ejc--doc-buffer (candidate)
  (company-doc-buffer (ac-ejc-documentation candidate)))

(defun cape-ejc--grab-symbol ()
  "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string."
  (if (looking-at "\\_>")
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                (point)))
    (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
      "")))

;;;###autoload
(defun cape-ejc (&optional interactive)
  "Complete with Ejc at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-ejc)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties
          (cape--cached-table (car bounds) (cdr bounds) #'cape-ejc--candidates 'substring)
          :category 'cape-ejc)
        :exclusive no
        :company-prefix-length (cdr-safe (cape-ejc--grab-symbol))
        :company-doc-buffer #'cape-ejc--doc-buffer
        ,@cape-ejc--properties))))


;;;###autoload
(defun ejc-capf-dot-pressed ()
  (interactive)
  (insert ".")
  (if ejc-complete-on-dot
      (call-interactively 'cape-ejc)))

(define-key ejc-sql-mode-keymap (kbd ".") 'ejc-capf-dot-pressed)

(provide 'cape-ejc)

;;; cape-ejc.el ends here
