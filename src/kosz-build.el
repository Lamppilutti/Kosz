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
;; API for building packages for package building.

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'map)
(require 'pp)
(require 'seq)

(require 'kosz-manifest)
(require 'kosz-utils)



(defvar kbuild-load-files
  (list load-file-name))

(defvar kbuild-build-package-functions
  (list #'kbuild-make-pkg-file
        #'kbuild-collect-src-files
        #'kbuild-collect-assets-files
        #'kbuild-collect-readme-file
        #'kbuild-build-texi))

(defvar kbuild-pack-package-function
  #'kbuild-pack-package)



(define-error 'kbuild-package-building-error
  "Error while package building")

(define-error 'kbuild-docs-building-error
  "Error while documentation building")



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

(defun kbuild--run-build-process (directory manifest build-directory)
  "Run package build process in DIRECTORY.

MANIFEST is manifest, it will passed to build and pack functions.
BUILD-DIRECTORY is directory what stores files of builded package, it will
passed to build and pack functions.

Return path to created archive."
  (kutils-eval-in-other-process
   directory
   `(progn
      (setq load-path
            (append load-path
                    ,(mapc #'file-name-directory kosz-build-load-files)))
      (mapc #'load-file ,kosz-build-load-files)
      (mapc (lambda (function) (funcall function ,manifest ,build-directory))
            ,kbuild-build-package-functions)
      (funcall ,kbuild-pack-package-function ,manifest ,build-directory))))



(defun kbuild-make-pkg-file (manifest directory)
  "Create \"-pkg.el\" file from MANIFEST inside DIRECTORY."
  (require 'kosz-build)
  (let* ((manifest*           (cdr manifest))
         (define-package-form (kmanifest-manifest->define-package manifest t))
         (file-name           (format "%s-pkg.el" (plist-get manifest* :name))))
    (with-temp-file (file-name-concat directory file-name)
      (pp-emacs-lisp-code define-package-form)
      (insert "\n;; Local Variables:\n;; no-byte-compile: t\n;; End:\n"))))

(defun kbuild-collect-src-files (manifest directory)
  "Find listed in MANIFEST src files and copy them to DIRECTORY."
  (require 'kosz-build)
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

(defun kbuild-collect-assets-files (manifest directory)
  "Find listed in MANIFEST assets files and copy them to DIRECTORY."
  (require 'kosz-build)
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

(defun kbuild-collect-readme-file (manifest directory)
  "Find listed in MANIFEST readme file and copy it to DIRECTORY as \"README\".

Signal error if listed file is directory."
  (require 'kosz-build)
  (when-let* ((root        (car manifest))
              (manifest*   (cdr manifest))
              (readme-file (thread-first (plist-get manifest* :readme)
                                         (expand-file-name root))))
    (if (file-regular-p readme-file)
        (copy-file readme-file (file-name-concat directory "README"))
      (error (list ":readme file is directory" readme-file)))))

(defun kbuild-build-texi (manifest directory)
  "Find listed in MANIFEST \".texi\" files and build them in DIRECTORY."
  (require 'kosz-build)
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (docs-includes  (thread-first (plist-get manifest* :docs)
                                       (kutils-expand-files root)))
         (docs-excludes  (thread-first (plist-get manifest* :docs-exclude)
                                       (kutils-expand-files root)))
         (temp-directory (make-temp-file "kosz-" t)))
    (thread-last
      (seq-difference docs-includes docs-excludes)
      (seq-filter (lambda (file)
                    (string= ".texi" (file-name-extension file t))))
      (kbuild--makeinfo manifest temp-directory)
      (mapc (lambda (file) (rename-file file directory))))))

(defun kbuild-pack-package (manifest directory)
  (require 'kosz-build)
  (let* ((package-fullname (kbuild--package-full-name manifest))
         (tar-file-name    (format "%s.tar" package-fullname)))
    (kutils-call-process
     "tar" (file-name-directory directory)
     (format "--transform='s|%s|%s|'"
             directory package-fullname)
     "-cf" tar-file-name
     directory)
    (file-name-concat directory tar-file-name)))



(defun kbuild-build-docs (manifest)
  "Build package documentation for package described in MANIFEST.

It builds \".texi\" files to \".info\" files and create \"dir\" file.
Signal `kosz-build-package-building-error'' If error cases while building.

Return path to directory with builded documentation."
  (let* ((root             (car manifest))
         (package-fullname (kbuild--package-full-name manifest))
         (build-directory  (file-name-concat
                            root "build" package-fullname ".build/"))
         (result-directory (file-name-concat
                            root "build" package-fullname "docs/"))
         (kbuild-build-package-functions (list #'kbuild-build-texi)))
    (unwind-protect
        (condition-case error
            (progn
              (make-directory build-directory t)
              (kbuild--run-build-process root manifest build-directory)
              (delete-directory result-directory)
              (rename-file build-directory result-directory)
              result-directory)
          (error
           (signal 'kbuild-build-error (cdr error))))
      (delete-directory build-directory t))))

(defun kbuild-build-package (manifest)
  "Build package described in MANIFEST.

Signal `kosz-build-package-building-error' if error cases while building.

Return path to package arvhive if NOT-PACK is nil, otherwice return nil."
  (let* ((root              (car manifest))
         (package-fullname  (kbuild--package-full-name manifest))
         (build-directory   (file-name-concat
                             root "build" package-fullname ".build/")))
    (unwind-protect
        (condition-case error
            (progn
              (make-directory build-directory t)
              (kbuild--run-build-process root manifest build-directory))
          (error
           (signal 'kbuild-build-error (cdr error))))
      (delete-directory build-directory t))))



(provide 'kosz-build)

;; Local Variables:
;; read-symbol-shorthands: (("kbuild-"    . "kosz-build-")
;;                          ("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-build.el ends here.
