;;; kosz-manifest.el --- -*- lexical-binding: t; -*-

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

;;; Commentary
;; Programming interface for work with manifest file.

;;; Code:



(require 'kosz-utils)



(defconst kmanifest-manifest-file "package.kosz"
  "Manifest file name.")

(defconst kmanifest-dump-file ".manifest-dump"
  "File for manifest dump.")



(define-error 'kmanifest-manifest-validation-error
  "Manifest has invalid properties")



(defmacro kmanifest--manifest-validation (manifest &rest property-cases)
  "Utility macros for validate MANIFEST's properties.

PROPERTY-CASES is a list of (PROPERTY COND MESSAGE) elements.
PROPERTY is a keyword property from MANIFEST.
COND is an expression what returns boolean.  The property value will binded to
\\=`it' symbol.
MESSAGE is a string describes what property value is expected.

If COND returns nil then the property name, value and MESSAGE will collected to
(:property PROPERTY :value PROPERTY's-value :expected MESSAGE) error form.  If
after checking all PROPERTY-CASES there is one or more error forms then signal
`kosz-manifest-manifest-validation-error'.

\(fn PLIST (PROPERTY COND MESSAGE)...)"
  (declare (indent 1))
  (let* ((manifest-sym (gensym "manifest"))
         (errors-sym   (gensym "errors")))
    `(let* ((,manifest-sym ,manifest)
            (,errors-sym   nil))
       ,@(mapcar
          (lambda (case)
            (let* ((propertry (nth 0 case))
                   (condition (nth 1 case))
                   (error-msg (nth 2 case)))
              `(let* ((it (plist-get ,manifest-sym ,propertry)))
                 (unless ,condition
                   (push (list :property ,propertry
                               :value it
                               :expected ,error-msg)
                         ,errors-sym)))))
          property-cases)
       (when ,errors-sym
         (signal 'kmanifest-manifest-validation-error
                 (list :invalid-properties ,errors-sym))))))



(defun kmanifest--init-emacs (dump-file-name)
  "Return code for initialazing Emacs.

DUMP-FILE-NAME is file in which dump manifest after reading.

This code should be evaluated before manifest reading."
  `(progn
     (defun define-package (name version &rest properties)
       (setq properties (plist-put properties :name name))
       (setq properties (plist-put properties :version version))
       (with-temp-file ,dump-file-name
         (insert (format "%S" properties))))))



(defun kmanifest-validate-manifest (manifest)
  "Validate MANIFEST properties.

Return MANIFEST if all base properties valid.  Otherwice signal
`kosz-manifest-manifest-validation-error'."
  (kmanifest--manifest-validation (cdr manifest)
    (:name
     (kutils-symbolp it)
     "Not nil symbol")
    (:version
     (kutils-version-string-p it)
     "String of a form that can be understood by `version-to-list'")
    (:description
     (kutils-not-blank-string-p it)
     "Not blank string or nil")
    (:dependencies
     (kutils-list-of-pairs-p it #'kutils-symbolp #'kutils-version-string-p)
     "List of (not nil symbol - `version-to-list' undestandable string) pairs, \
or nil")
    (:url
     (kutils-not-blank-string-p it)
     "String or nil")
    (:authors
     (kutils-list-of-pairs-p
      it #'kutils-not-blank-string-p* #'kutils-not-blank-string-p*)
     "List of (not blank string - not blank string) pairs, or nil")
    (:license
     (kutils-not-blank-string-p it)
     "Not blank string or nil")
    (:commit
     (kutils-not-blank-string-p it)
     "String or nil")
    (:keywords
     (list-of-strings-p it)
     "List of strings or nil")
    (:maintainer
     (kutils-pairp it #'kutils-not-blank-string-p* #'kutils-not-blank-string-p*)
     "Pair of not blank strings or nil")
    (:readme
     (kutils-not-blank-string-p it)
     "Not blank string or nil")
    (:src
     (list-of-strings-p it)
     "List of strings or nil")
    (:src-exclude
     (list-of-strings-p it)
     "List of strings or nil")
    (:docs
     (list-of-strings-p it)
     "List of strings or nil")
    (:docs-exclude
     (list-of-strings-p it)
     "List of strings or nil")
    (:assets
     (list-of-strings-p it)
     "List if strings or nil")
    (:assets-exclide
     (list-of-strings-p it)
     "List of strings or nil")
    (:tests
     (list-of-strings-p it)
     "List of strings or nil")
    (:tests-exclude
     (list-of-strings-p it)
     "List of strings or nil")
    (:test-runner
     (symbolp it)
     "Symbol or nil"))
  manifest)

(defun kmanifest-read-manifest (directory)
  "Read manifest from DIRECTORY.

Manifest is (PATH . PLIST) cons, where PATH is path to the directory from
where the manifest file was readed, and a PLIST is properties readed from
the manifest file.

If manifest invalid signal `kosz-manifest-validation-error'."
  (setq directory (expand-file-name directory))
  (let* ((dump-file (file-name-concat directory kmanifest-dump-file))
         (init-code (format "%S" (kmanifest--init-emacs dump-file))))
    (kutils-call-process "emacs" directory
                         "--batch" "--quick"
                         "--eval" init-code
                         "--load" kmanifest-manifest-file)
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents dump-file)
          (kmanifest-validate-manifest
           (cons (abbreviate-file-name directory)
                 (sexp-at-point))))
      (delete-file dump-file))))



(provide 'kosz-manifest)

;; Local Variables:
;; read-symbol-shorthands: (("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-manifest.el ends here.
