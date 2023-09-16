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



(defun kmanifest--pkg-name-p (object)
  "Return t if OBJECT is not nil and not keyword symbol"
  (and (symbolp object) (not (keywordp object)) (not (null object))))

(defun kmanifest--versionp (object)
  "Return t of OBJECT is string that `version-to-list' undertood."
  (ignore-errors (version-to-list object)))

(defun kmanifest--check-pairs (object firstp secondp)
  "Return t if OBJECT is a list of pairs or nil.

Check the first element of pair by FIRSTP, and the second by SECONDP.

The pair is list of two elements, for example (1 2)."
  (while (and (consp object)
              (funcall firstp (caar object))
              (funcall secondp (cadar object)))
    (setq object (cdr object)))
  (null object))

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
     (kmanifest--pkg-name-p it)
     "Name must be not nil symbol")
    (:version
     (kmanifest--versionp it)
     "Version must be string that can be understood by `version-to-list'")
    (:description
     (or (stringp it) (null it))
     "Property should be string.")
    (:dependencies
     (kmanifest--check-pairs it #'kmanifest--pkg-name-p #'kmanifest--versionp)
     "Property shoud be list of (symbol version-string); \
Symbol is not nil symbol, version-string is string that can be understood by \
`version-to-list'")
    (:url
     (or (stringp it) (null it))
     "Property should be string")
    (:authors
     (kmanifest--check-pairs it #'stringp #'stringp)
     "Property should be list of (string string) pairs")
    (:license
     (or (stringp it) (null it))
     "Property should be string")
    (:commit
     (or (stringp it) (null it))
     "Property should be string")
    (:keywords
     (list-of-strings-p it)
     "Property should be list of strings")
    (:maintainer
     (or (and (length= it 2) (stringp (car it)) (stringp (cadr it)))
         (null it))
     "Property should be (string string) pair")
    (:readme
     (or (stringp it) (null it))
     "Property should be string")
    (:src
     (list-of-strings-p it)
     "Property should be list of strings")
    (:src-exclude
     (list-of-strings-p it)
     "Property should be list of strings")
    (:docs
     (list-of-strings-p it)
     "Property should be list of strings")
    (:docs-exclude
     (list-of-strings-p it)
     "Property should be list of strings")
    (:assets
     (list-of-strings-p it)
     "Property should be list of strings")
    (:assets-exclide
     (list-of-strings-p it)
     "Property should be list of strings")
    (:tests
     (list-of-strings-p it)
     "Property should be list of strings")
    (:tests-exclude
     (list-of-strings-p it)
     "Property should be list of strings")
    (:test-runner
     (and (symbolp it) (not (keywordp it)))
     "Property should be symbol"))
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
