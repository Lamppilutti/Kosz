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
  "Utility macros for validate PLIST.

PROPERTY-CASES is a list of (PROPERTY BIND COND MESSAGE) elements.
PROPERTY is a keyword property from MANIFEST.
BIND is a symbol to which property value will bind.
COND is an expression what returns boolean.
MESSAGE is a string describes what property value is expected.

If COND returns nil then the property name, value and MESSAGE will collected to
(:property PROPERTY :value PROPERTY's-value :expected MESSAGE) error form.  If
after checking all PROPERTY-CASES there is one or more error forms then signal
`kosz-manifest-manifest-validation-error'.

\(fn PLIST (PROPERTY BIND COND MESSAGE)...)"
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
     name (kutils-symbolp name)
     "Not nil symbol")
    (:version
     version (kutils-version-string-p version)
     "String of a form that can be understood by `version-to-list'")
    (:description
     desc (kutils-not-blank-string-p desc)
     "Not blank string or nil")
    (:dependencies
     deps (kutils-list-of-pairs-p
           deps #'kutils-symbolp #'kutils-version-string-p)
     "List of (not nil symbol - `version-to-list' undestandable string) pairs, \
or nil")
    (:url
     url (kutils-not-blank-string-p url)
     "String or nil")
    (:authors
     authors (kutils-list-of-pairs-p
              authors #'kutils-not-blank-string-p* #'kutils-not-blank-string-p*)
     "List of (not blank string - not blank string) pairs, or nil")
    (:license
     license (kutils-not-blank-string-p license)
     "Not blank string or nil")
    (:commit
     commit (kutils-not-blank-string-p commit)
     "String or nil")
    (:keywords
     keywords (list-of-strings-p keywords)
     "List of strings or nil")
    (:maintainer
     maintainer (kutils-pairp maintainer
                              #'kutils-not-blank-string-p*
                              #'kutils-not-blank-string-p*)
     "Pair of not blank strings or nil")
    (:readme
     readme (kutils-not-blank-string-p readme)
     "Not blank string or nil")
    (:src
     src (list-of-strings-p src)
     "List of strings or nil")
    (:src-exclude
     src-ex (list-of-strings-p src-ex)
     "List of strings or nil")
    (:docs
     docs (list-of-strings-p docs)
     "List of strings or nil")
    (:docs-exclude
     docs-ex (list-of-strings-p docs-ex)
     "List of strings or nil")
    (:assets
     assets (list-of-strings-p assets)
     "List if strings or nil")
    (:assets-exclide
     assets-ex (list-of-strings-p assets-ex)
     "List of strings or nil")
    (:tests
     tests (list-of-strings-p tests)
     "List of strings or nil")
    (:tests-exclude
     tests-ex (list-of-strings-p tests)
     "List of strings or nil")
    (:test-runner
     test-runner (kutils-functionp test-runner)))
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
