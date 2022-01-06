;;; ejc-capf.el -- SQL completitions at point by company-mode (the part of ejc-sql).

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

;; `ejc-capf' is a `company' completion backend for `ejc-sql'.
;; To use it, add `ejc-capf-backend' to `company-backends':

;;     (requre 'ejc-capf)
;;     (push 'ejc-capf-backend company-backends)

;;; Code:

(require 'cl-lib)
(require 'ejc-completion-common)

(defvar ejc-sql-mode-keymap)

(defun ejc-capf--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun ejc-capf--interactive (capf)
  "Complete with CAPF."
  (let ((completion-at-point-functions (list capf)))
    (or (completion-at-point) (user-error "%s: No completions" capf))))

(cl-defun ejc-capf--table-with-properties (table &key category (sort t) &allow-other-keys)
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (if (or (not table) (and (not category) sort))
      table
    (let ((metadata `(metadata
                      ,@(and category `((category . ,category)))
                      ,@(and (not sort) '((display-sort-function . identity)
                                          (cycle-sort-function . identity))))))
      (lambda (str pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action table str pred))))))

(defun ejc-capf--input-valid-p (old-input new-input cmp)
  "Return non-nil if the NEW-INPUT is valid in comparison to OLD-INPUT.
The CMP argument determines how the new input is compared to the old input.
- never: Never treat the input as valid.
- prefix/nil: The old input is a prefix of the new input.
- equal: The old input is equal to the new input.
- substring: The old input is a substring of the new input."
  ;; Treat input as not changed if it contains space to allow
  ;; Orderless completion style filtering.
  (or (string-match-p "\\s-" new-input)
      (pcase-exhaustive cmp
        ('never nil)
        ((or 'prefix 'nil) (string-prefix-p old-input new-input))
        ('equal (equal old-input new-input))
        ('substring (string-match-p (regexp-quote old-input) new-input)))))

(defun ejc-capf--cached-table (beg end fun valid)
  "Create caching completion table.
BEG and END are the input bounds.
FUN is the function which computes the candidates.
VALID is the input comparator, see `ejc-capf--input-valid-p'."
  (let ((input 'init)
        (beg (copy-marker beg))
        (end (copy-marker end t))
        (table nil))
    (lambda (str pred action)
      (let ((new-input (buffer-substring-no-properties beg end)))
        (when (or (eq input 'init) (not (ejc-capf--input-valid-p input new-input valid)))
          ;; NOTE: We have to make sure that the completion table is interruptible.
          ;; An interruption should not happen between the setqs.
          (setq table (funcall fun new-input)
                input new-input)))
      (complete-with-action action table str pred))))

(defvar ejc-capf--properties
  (list :annotation-function #'ejc-capf--annotation
        :company-kind #'ejc-capf--kind)
  "Completion extra properties for `ejc-capf'.")

(defun ejc-capf--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun ejc-capf--add-meta (meta candidates)
  (-map (lambda (k) (list k meta))
        candidates))

(defun ejc-capf--candidates (str)
  (let* ((prefix-1 (ejc-get-prefix-word))
         (prefix-2 (save-excursion
                     (search-backward "." nil t)
                     (ejc-get-prefix-word)))
         (str (or (ejc-capf--grab-symbol) str))
         (res))
    (dolist (item
             (cl-remove-if-not
              (lambda (c) (string-prefix-p str (car c) t))
              (append
               (ejc-append-without-duplicates
                (ejc-capf--add-meta
                 "ansi sql" (ejc-get-ansi-sql-words))
                (ejc-capf--add-meta
                 "keyword" (ejc-get-keywords))
                'car :right)
               (ejc-capf--add-meta
                "owner" (ejc-owners-candidates))
               (ejc-capf--add-meta
                "table" (ejc-tables-candidates))
               (ejc-capf--add-meta
                "view" (ejc-views-candidates))
               (if (not prefix-1)
                   (ejc-capf--add-meta
                    "package" (ejc-packages-candidates)))
               (ejc-capf--add-meta
                "column" (ejc-colomns-candidates)))))
      (push (ejc-capf--make-candidate item) res))
    res))

(defun ejc-capf--annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun ejc-capf--kind (candidate)
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

(defun ejc-capf--doc-buffer (candidate)
  (with-current-buffer (get-buffer-create "*company-documentation*")
    (erase-buffer)
    (fundamental-mode)
    (when-let ((cand (ac-ejc-documentation candidate)))
      (save-excursion
        (insert cand)
        (visual-line-mode))
      (current-buffer))))

(defun ejc-capf--grab-symbol ()
  "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string."
  (if (looking-at "\\_>")
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                (point)))
    (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
      "")))

;;;###autoload
(defun ejc-capf (&optional interactive)
  "Complete with Ejc at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (ejc-capf--interactive #'ejc-capf)
    (let ((bounds (ejc-capf--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(ejc-capf--table-with-properties
          (ejc-capf--cached-table (car bounds) (cdr bounds) #'ejc-capf--candidates 'substring)
          :category 'ejc-capf)
        :exclusive no
        :company-prefix-length (cdr-safe (ejc-capf--grab-symbol))
        :company-doc-buffer #'ejc-capf--doc-buffer
        ,@ejc-capf--properties))))


;;;###autoload
(defun ejc-capf-dot-pressed ()
  (interactive)
  (insert ".")
  (if ejc-complete-on-dot
      (call-interactively 'ejc-capf)))

(define-key ejc-sql-mode-keymap (kbd ".") 'ejc-capf-dot-pressed)

(provide 'ejc-capf)

;;; ejc-capf.el ends here
