;;; kosz-build.el --- -*- lexical-binding: t; -*-

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
;; API for building packages for `load-path' and "package.el".

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'map)
(require 'pp)
(require 'seq)

(require 'kosz-manifest)
(require 'kosz-utils)



(define-error 'kbuild-build-error
  "Error while package building")


(defun kbuild--copy-file (file newname)
  "Copy FILE to NEWNAME.

Create directories in NEWNAME path, if they don't exist."
  (let* ((destination (file-name-directory newname)))
    (make-directory destination t)
    (copy-file file newname)))

(defun kbuild--make-makeinfo-variables (manifest)
  "Return makeinfo arguments for creating variables from MANIFEST.

Variables has \"kosz(:property-name)\" names and relevant values from MANIFEST."
  (setq manifest (cdr manifest))
  (flatten-list
   (map-apply (lambda (key value) (list "-D" (format "kosz(%s) %s" key value)))
              manifest)))

(defun kbuild--makeinfo (manifest directory files)
  "Build \".info\" and \"dir\" files from \".texi\" FILES inside DIRECTORY.

Pass MANIFEST properties to makeinfo as \"kosz(:property-name)\" variables.

Return list of created files."
  (apply #'kutils-call-process "makeinfo" directory
         (append files (kbuild--make-makeinfo-variables manifest)))
  (dolist (file (kutils-directory-files-recursively directory))
    (kutils-call-process "install-info" directory file "dir"))
  (kutils-directory-files-recursively directory))

(defun kbuild--package-full-name (manifest)
  "Read package full name from MANIFEST.

Package full name is \"name-version\" string, like \"kosz-1.1.1\"."
  (setq manifest (cdr manifest))
  (format "%s-%s" (plist-get manifest :name) (plist-get manifest :version)))

(defun kbuild--generate-pkg-file (manifest directory)
  "Generate \"-pkg.el\" file created from MANIFEST inside DIRECTORY."
  (let* ((manifest*           (cdr manifest))
         (define-package-form (kbuild-manifest->define-package manifest))
         (file-name           (format "%s-pkg.el" (plist-get manifest* :name))))
    (with-temp-file (file-name-concat directory file-name)
      (pp-emacs-lisp-code define-package-form)
      (insert "\n;; Local Variables:\n;; no-byte-compile: t\n;; End:\n"))))

(defun kbuild--copy-src-files (manifest directory)
  "Find listed in MANIFEST src files and copy them to DIRECTORY."
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (kutils-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (kutils-expand-files root))))
    (thread-last
      (seq-difference src-includes src-excludes)
      (seq-filter (lambda (file) (string= ".el" (file-name-extension file t))))
      (mapc (lambda (file) (copy-file file directory))))))

(defun kbuild--copy-assets-files (manifest directory)
  "Find listed in MANIFEST assets files and copy them to DIRECTORY."
  (let* ((root            (car manifest))
         (manifest*       (cdr manifest))
         (assets-includes (thread-first (plist-get manifest* :assets)
                                        (kutils-expand-files root)))
         (assets-excludes (thread-first (plist-get manifest* :assets-exclude)
                                        (kutils-expand-files root))))
    (dolist (file (seq-difference assets-includes assets-excludes))
      (thread-last (file-relative-name file root)
                   (file-name-concat directory)
                   (kbuild--copy-file file)))))

(defun kbuild--copy-readme-file (manifest directory)
  "Find listed in MANIFEST readme file and copy it to DIRECTORY as \"README\".

Signal `kosz-build-build-error' if listed file is directory."
  (when-let* ((root        (car manifest))
              (manifest*   (cdr manifest))
              (readme-file (thread-first (plist-get manifest* :readme)
                                         (expand-file-name root))))
    (if (file-regular-p readme-file)
        (copy-file readme-file (file-name-concat directory "README"))
      (error (list ":readme file is directory" readme-file)))))

(defun kbuild--build-docs (manifest directory)
  "Find listed in MANIFEST \".texi\" files and build them in DIRECTORY."
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (docs-includes  (thread-first (plist-get manifest* :docs)
                                       (kutils-expand-files root)))
         (docs-excludes  (thread-first (plist-get manifest* :docs-exclude)
                                       (kutils-expand-files root)))
         (temp-directory (make-temp-file "kosz-" t)))
    (make-directory temp-directory)
    (thread-last
      (seq-difference docs-includes docs-excludes)
      (seq-filter (lambda (file)
                    (string= ".texi" (file-name-extension file t))))
      (kbuild--makeinfo manifest temp-directory)
      (mapc (lambda (file) (rename-file file directory))))))



(defun kbuild-manifest->define-package (manifest)
  "Return `define-package' form generated from MANIFEST.

If MANIFEST extra properties are invalid signal
`kosz-manifest-manifest-validation-error'.
Skip properties what have no use for \"package.el\"."
  (map-let ((:name         name)
            (:version      version)
            (:description  description)
            (:dependencies dependencies)
            (:url          url)
            (:commit       commit)
            (:keywords     keywords)
            (:maintainer   maintainer)
            (:authors      authors))
      (cdr (kmanifest-validate-manifest manifest))
    (list 'define-package
          (format "%s" name)
          version
          description
          dependencies
          :url      url
          :commit   commit
          :keywords keywords
          :maintainer
          (unless (null maintainer)
            (cons (car maintainer) (cadr maintainer)))
          :authors
          (mapcar (lambda (pair) (cons (car pair) (cadr pair))) authors))))

(defun kbuild-build-docs (manifest)
  "Build package documentation for package described in MANIFEST.

It builds \".texi\" files to \".info\" files and create \"dir\" file.
Signal `kosz-build-build-error' If error cases while building.

Return path to directory with builded documentation."
  (let* ((root             (car manifest))
         (package-fullname (kbuild--package-full-name manifest))
         (build-directory  (file-name-concat
                            root "build" package-fullname "docs/")))
    (condition-case build-error
        (progn
          (make-directory build-directory t)
          (kbuild--build-docs manifest build-directory)
          build-directory)
      (error (signal 'kbuild-build-error (cdr build-error))))))

(defun kbuild-build-package (manifest)
  "Build tar file for package described in MANIFEST.

Created tar file can be used by \"package.el\".  Extracted from tar directory
can be used in `load-path'.
Signal `kosz-build-build-error' If error cases while building.

Return path to created tar file."
  (let* ((root              (car manifest))
         (package-fullname  (kbuild--package-full-name manifest))
         (package-tar-file  (format "%s.tar" package-fullname))
         (package-directory (file-name-concat root "build" package-fullname))
         (build-directory   (file-name-as-directory ; For correct file moving.
                             (file-name-concat package-directory
                                               package-fullname))))
    (unwind-protect
        (condition-case build-error
            (progn
              (make-directory build-directory t)
              (kbuild--generate-pkg-file manifest build-directory)
              (kbuild--copy-src-files manifest build-directory)
              (kbuild--copy-assets-files manifest build-directory)
              (kbuild--copy-readme-file manifest build-directory)
              (kbuild--build-docs manifest build-directory)
              (kutils-call-process "tar" package-directory
                                   "-cf" package-tar-file
                                   package-fullname)
              (expand-file-name package-tar-file package-directory))
          (error (signal 'kbuild-build-error (cdr build-error))))
      (delete-directory build-directory t))))



(provide 'kosz-build)

;; Local Variables:
;; read-symbol-shorthands: (("kbuild-"    . "kosz-build-")
;;                          ("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-build.el ends here.
