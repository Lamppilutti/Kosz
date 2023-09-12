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



(defun kmanifest--init-emacs ()
  "Return code for initialazing Emacs.

It defines \\='define-package' form and makes shure that result of
\\='define-package' form will be printed at the same end of Emacs work.

This code should be evaluated before any \\='load'."
  `(progn
     (defvar _define_package_result)
     (advice-add 'load :after
                 (lambda (&rest _)
                   (setq kill-emacs-hook nil)
                   (prin1 _define_package_result)
                   (kill-emacs)))
     (defun define-package (name version &rest properties)
       (plist-put properties :name name)
       (plist-put properties :version version)
       (setq _define_package_result properties))))



(defun kmanifest-validate-manifest (manifest)
  "Validate MANIFEST properties.

Return MANIFEST if all base properties valid.  Otherwice signal
`kosz-utils-validation-error'."
  (kutils-plist-validation (cdr manifest)
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
  (with-temp-buffer
    (insert (kutils-call-process "emacs" directory
                             "--batch" "--quick"
                             "--eval" (format "%S" (kmanifest--init-emacs))
                             "--load" kmanifest-manifest-file))
    (kmanifest-validate-manifest
     (cons (abbreviate-file-name directory)
           (sexp-at-point)))))



(provide 'kosz-manifest)

;; Local Variables:
;; read-symbol-shorthands: (("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-manifest.el ends here.
