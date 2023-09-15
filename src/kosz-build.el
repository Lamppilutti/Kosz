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
;; API for building packages.  Build targets `load-path' and "package.el".

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'pp)
(require 'seq)

(require 'kosz-manifest)
(require 'kosz-utils)



(defun kbuild--makeinfo(manifest files directory)
  "Build \\='.info' and \\='dir' files from \\='.texi' FILES inside DIRECTORY.

Create variables from MANIFEST's properties with \\='kosz(:property)' names.
Return list of created files."
  (setq manifest (cdr manifest))
  (let* ((variable-setters nil))
    (while-let ((not-empty-p manifest))
      (push (format "kosz(%s) %s" (pop manifest) (pop manifest))
            variable-setters)
      (push "-D" variable-setters))
    (apply #'kutils-call-process "makeinfo" directory
           (append files variable-setters))
    (dolist (file (kutils-directory-files-recursively directory))
      (kutils-call-process "install-info" directory file "dir"))
    (kutils-directory-files-recursively directory)))

(defun kbuild--generate-pkg-file (manifest directory)
  "Generate \\='-pkg.el' file from MANIFEST inside DIRECTORY."
  (let* ((manifest*           (cdr manifest))
         (define-package-form (kbuild-manifest->define-package manifest))
         (file-name           (format "%s-pkg.el" (plist-get manifest* :name))))
    (with-temp-file (file-name-concat directory file-name)
      (pp-emacs-lisp-code define-package-form)
      (insert "\n;; Local Variables:\n;; no-byte-compile: t\n;; End:\n"))))

(defun kbuild--copy-src-files (manifest directory)
  "Find listed in MANIFEST src files and copy they to DIRECTORY."
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (kutils-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (kutils-expand-files root))))
    (dolist (file (seq-difference src-includes src-excludes))
      (when (string= ".el" (file-name-extension file t))
        (copy-file file directory t)))))

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
                   (kutils-copy-file file)))))

(defun kbuild--build-docs (manifest directory)
  "Find listed in MANIFEST documentation files and build them in DIRECTORY.

Documentation files are \\='.texi' files."
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (docs-includes  (thread-first (plist-get manifest* :docs)
                                       (kutils-expand-files root)))
         (docs-excludes  (thread-first (plist-get manifest* :docs-exclude)
                                       (kutils-expand-files root)))
         (docs-files     (thread-last
                           (seq-difference docs-includes docs-excludes)
                           (seq-filter
                            (lambda (file)
                              (string= ".texi" (file-name-extension file t))))))
         (temp-directory (kutils-temporary-file-directory)))
    (make-directory temp-directory t)
    (dolist (file (kbuild--makeinfo manifest docs-files temp-directory))
      (rename-file file directory))))

  (defun kbuild--copy-readme-file (manifest directory)
    "Find listed in MANIFEST readme file and copy it to DIRECTORY.

Copied will have \\='README' name."
    (when-let* ((root         (car manifest))
                (manifest*    (cdr manifest))
                (readme-file  (plist-get manifest* :readme)))
      (copy-file (expand-file-name readme-file root)
                 (file-name-concat directory "README")
                 t)))



(defun kbuild-manifest->define-package (manifest)
  "Return \\='define-package' form generated from MANIFEST.

If MANIFEST extra properties are invalid signal `kosz-utils-validation-error'.
Skip properties what have no use for \\='package.el'."
  (setq manifest (cdr manifest))
  (let* ((name         (plist-get manifest :name))
         (version      (plist-get manifest :version))
         (description  (plist-get manifest :description))
         (dependencies (plist-get manifest :dependencies))
         (url          (plist-get manifest :url))
         (commit       (plist-get manifest :commit))
         (keywords     (plist-get manifest :keywords))
         (maintainer   (plist-get manifest :maintainer))
         (authors      (plist-get manifest :authors)))
    (list 'define-package
          (format "%s" name)
          version
          description
          dependencies
          :url        url
          :commit     commit
          :keywords   keywords
          :maintainer (kutils-pair->cons maintainer)
          :authors    (kutils-pairs->alist authors))))

(defun kbuild-build (manifest)
  "Build tar file for package described in MANIFEST.

Created tar file can be used in by \\='package.el'.  Extracted directory can be
used in `load-path'."
  (let* ((root              (car manifest))
         (manifest*         (cdr manifest))
         (package-name      (plist-get manifest* :name))
         (package-version   (plist-get manifest* :version))
         (package-fullname  (format "%s-%s" package-name package-version))
         (package-tar-file  (format "%s.tar" package-fullname))
         (build-directory   (file-name-concat root "build"))
         (package-directory (thread-last package-fullname
                                         (file-name-concat build-directory)
                                         ;; For correct file moving.
                                         (file-name-as-directory))))
    (unwind-protect
        (progn
          (make-directory package-directory t)
          (kbuild--generate-pkg-file manifest package-directory)
          (kbuild--copy-src-files manifest package-directory)
          (kbuild--copy-assets-files manifest package-directory)
          (kbuild--build-docs manifest package-directory)
          (kbuild--copy-readme-file manifest package-directory)
          (kutils-call-process "tar" build-directory
                               "-cf" package-tar-file
                               package-fullname)
          (expand-file-name package-tar-file build-directory))
      (delete-directory package-directory t))))



(provide 'kosz-build)

;; Local Variables:
;; read-symbol-shorthands: (("kbuild-"    . "kosz-build-")
;;                          ("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-build.el ends here.
