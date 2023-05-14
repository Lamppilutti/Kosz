;;; kosz-manifest.el --- manifest manipulation functions. -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Lämppi Lütti <lamppilutti@gmail.com>
;;
;; This file is part or Kosz.
;;
;; Kosz is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Kosz is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details
;;
;; You should have received a copy of the GNU General Public License
;; along with Kosz.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:



(require 'kosz-utils)



(defconst km-manifest-file "package.kosz")



(define-error 'km-validation-error
  "Invalid manifest properties")



(defmacro km--manifest-validation (manifest &rest property-cases)
  (declare (indent 1))
  (let* ((errors-sym (gensym "errors"))
         (bindings   nil)
         (cases*     nil))
    (dolist (case property-cases)
      (let* ((property         (nth 0 case))
             (bind             (nth 1 case))
             (condition        (nth 2 case))
             (expected-message (nth 3 case)))
        (push `(,bind (plist-get ,manifest ,property))
              bindings)
        (push `(unless ,condition
                 (push (list :property ,property
                             :value    ,bind
                             :expected ,expected-message)
                       ,errors-sym))
              cases*)))
    `(let* ((,errors-sym  nil)
            ,@bindings)
       ,@cases*
       (when ,errors-sym
         (signal 'km-validation-error
                 (list :invalid-properties ,errors-sym))))))

(defun km--define-package ()
  `(defun define-package (name version &rest properties)
     (plist-put properties :name name)
     (plist-put properties :version version)
     (prin1 properties)
     (setq kill-emacs-hook nil)
     (kill-emacs)))



(defun km-validate-manifest (manifest)
  (setq manifest (cdr manifest))
  (km--manifest-validation manifest
    (:name
     name (ku-symbolp name)
     "Not nil symbol")
    (:version
     version (ku-version-string-p version)
     "String of a form that can be understood by `version-to-list'")
    (:description
     desc (ku-not-blank-string-or-null-p desc)
     "Not blank string or null")
    (:dependencies
     deps (ku-list-of-pairs-p deps #'ku-symbolp #'ku-version-string-p)
     "List of (not nil symbol - `version-to-list' undestandable string) pairs")
    (:url
     url (ku-not-blank-string-p url)
     "String or null")
    (:authors
     authors (ku-list-of-pairs-p
              authors #'ku-not-blank-string-p #'ku-not-blank-string-p)
     "List of (not blank string - not blank string) pairs")
    (:license
     license (ku-not-blank-string-or-null-p license)
     "Not blank string or null")
    (:src
     src (ku-list-of-strings-p src)
     "List if strings")
    (:src-exclude
     src-ex (ku-list-of-strings-p src-ex)
     "List if strings")
    (:docs
     docs (ku-list-of-strings-p docs)
     "List if strings")
    (:docs-exclude
     docs-ex (ku-list-of-strings-p docs-ex)
     "List if strings")
    (:assets
     assets (ku-list-of-strings-p assets)
     "List if strings")
    (:assets-exclide
     assets-ex (ku-list-of-strings-p assets-ex)
     "List if strings"))
  manifest)

(defun km-read-manifest (directory)
  (setq directory (expand-file-name directory))
  (with-temp-buffer
    (insert (ku-call-process "emacs" directory
                             "--batch" "--quick"
                             "--eval" (format "%S" (km--define-package))
                             "--load" km-manifest-file))
    (km-validate-manifest
     (cons (abbreviate-file-name directory)
           (sexp-at-point)))))



(provide 'kosz-manifest)

;; Local Variables:
;; read-symbol-shorthands: (("km-" . "kosz-manifest-")
;;                          ("ku-" . "kosz-utils-"))
;; End:

;;; kosz-manifest.el ends here.
