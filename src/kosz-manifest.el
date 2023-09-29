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

;;; Commentary:
;; API for working with manifest objects.

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
`it' symbol.
MESSAGE is a string describes what property value is expected.

If COND returns nil then the property name, value and MESSAGE will collected to
\(:property PROPERTY :value PROPERTY's-value :expected MESSAGE) error form.  If
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
  "Return t if OBJECT is not nil and not keyword symbol."
  (and (symbolp object) (not (keywordp object)) (not (null object))))

(defun kmanifest--versionp (object)
  "Return t of OBJECT is string that `version-to-list' undertood."
  (ignore-errors (version-to-list object)))

(defun kmanifest--pair-p (object firstp secondp)
  "Return t if OBJECT is pair.

Check the first element of pair by FIRSTP, and the second one by SECONDP.

The pair is list of two elements, for example (1 2)."
  (and (proper-list-p object)
       (length= object 2)
       (funcall firstp (car object))
       (funcall secondp (cadr object))))

(defun kmanifest--list-of-pairs-p (object firstp secondp)
  "Return t if OBJECT is nil or a list of pairs.

Check the first element of each pair by FIRSTP, and the second one by SECONDP.

See `kmanifest--pair-p'."
  (while (and (consp object)
              (kmanifest--pair-p (car object) firstp secondp))
    (setq object (cdr object)))
  (null object))

(defun kmanifest--valid-file-path-p (object)
  "Return t if OBJECT is not empty string that isn't \\='.', \\='..' or \\='/'."
  (and (stringp object)
       (not (string-empty-p object))
       (not (string-match-p "^/\\|^\\.\\.?/?$" object))))

(defun kmanifest--list-of-valid-file-paths-p (object)
  "Return t if OBJECT is nil or list of valid file paths.

See `kosz-manifest--valid-file-path-p'"
  (while (and (consp object)
              (stringp (car object))
              (kmanifest--valid-file-path-p (car object)))
    (setq object (cdr object)))
  (null object))

(defun kmanifest--init-emacs (dump-file)
  "Return code for initialazing Emacs.

DUMP-FILE is file in which manifest will be dumped after reading.

This code should be evaluated before manifest reading."
  `(progn
     (defun define-package (name version &rest properties)
       (setq properties (plist-put properties :name name))
       (setq properties (plist-put properties :version version))
       (with-temp-file ,dump-file
         (insert (format "%S" properties))))))



(defun kmanifest-validate-manifest (manifest)
  "Validate MANIFEST properties.

Return MANIFEST if significant properties are valid.  Otherwice signal
`kosz-manifest-manifest-validation-error' with
\(:property \\='property-name'
 :value    \\='property-value'
 :expected \\='string-that-described-expected-value') plist as data."
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
     (kmanifest--list-of-pairs-p it
                                 #'kmanifest--pkg-name-p #'kmanifest--versionp)
     "Property shoud be list of (symbol version-string); \
Symbol is not nil symbol, version-string is string that can be understood by \
`version-to-list'")
    (:url
     (or (stringp it) (null it))
     "Property should be string")
    (:authors
     (kmanifest--list-of-pairs-p it #'stringp #'stringp)
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
     (or (kmanifest--pair-p it #'stringp #'stringp) (null it))
     "Property should be (string string) pair")
    (:readme
     (or (kmanifest--valid-file-path-p it) (null it))
     "Property should be path; \
Path must not be \".\", \"..\" or \"\\\"")
    (:src
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:src-exclude
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:docs
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:docs-exclude
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:assets
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:assets-exclide
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:tests
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:tests-exclude
     (kmanifest--list-of-valid-file-paths-p it)
     "Property should be list of paths; \
Path must not be \".\", \"..\" or \"\\\"")
    (:test-runner
     (and (symbolp it) (not (keywordp it)))
     "Property should be not null symbol."))
  manifest)

(defun kmanifest-read-manifest (directory)
  "Read manifest from DIRECTORY.

Manifest is (PATH . PLIST) cons, where PATH is path to the directory from where
the manifest file was readed, and a PLIST is properties readed from the manifest
file.  Package name and package version are passed as `:name' and `:version'
respectively."
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
