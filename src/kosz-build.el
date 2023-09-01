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
;; Programming and user interfaces for building packages.  Resulted packages can
;; be loaded by adding to `load-path' or registered by "packege.el".

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'package)
(require 'pp)
(require 'project)
(require 'vc)

(require 'kosz-manifest)
(require 'kosz-utils)



(defun kb--makeinfo(manifest files directory)
  "Build \\='.info' and \\='dir' files from \\='.texi' FILES inside DIRECTORY.

Path MANIFEST properties to
Return list of created files."
  (setq manifest (cdr manifest))
  (let* ((variable-setters nil))
    (while-let ((not-empty-p manifest))
      (push (format "kosz(%s) %s" (pop manifest) (pop manifest))
            variable-setters)
      (push "-D" variable-setters))
    (apply #'ku-call-process "makeinfo" directory
           (append files variable-setters))
    (dolist (file (ku-directory-files-recursively directory))
      (ku-call-process "install-info" directory file "dir"))
    (ku-directory-files-recursively directory)))

(defun kb--generate-pkg-file (manifest directory)
  "Generate \\='-pkg.el' file from MANIFEST inside DIRECTORY."
  (let* ((manifest*           (cdr manifest))
         (define-package-form (kb-manifest->define-package manifest))
         (file-name           (format "%s-pkg.el" (plist-get manifest* :name))))
    (with-temp-file (file-name-concat directory file-name)
      (pp-emacs-lisp-code define-package-form)
      (insert "\n;; Local Variables:\n;; no-byte-compile: t\n;; End:\n"))))

(defun kb--collect-src (manifest directory)
  "Copy package's source code files to DIRECTORY.

Use MANIFEST for getting information about source code files."
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (ku-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (ku-expand-files root))))
    (dolist (file src-includes)
      (when (and (not (member file src-excludes))
                 (equal ".el" (file-name-extension file t)))
        (copy-file file directory t)))))

(defun kb--collect-assets (manifest directory)
  "Copy package assets files to DIRECTORY.

Use MANIFEST for getting information about assets files."
  (let* ((root            (car manifest))
         (manifest*       (cdr manifest))
         (assets-includes (thread-first (plist-get manifest* :assets)
                                        (ku-expand-files root)))
         (assets-excludes (thread-first (plist-get manifest* :assets-exclude)
                                        (ku-expand-files root))))
    (dolist (file assets-includes)
      (when (not (member file assets-excludes))
        (thread-last (file-relative-name file root)
                     (file-name-concat directory)
                     (ku-copy-file file))))))

(defun kb--collect-docs (manifest directory)
  "Compile package \\='.texi' documentation files to DIRECTORY.

Use MANIFEST for getting information about documentation files."
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (docs-includes  (thread-first (plist-get manifest* :docs)
                                       (ku-expand-files root)))
         (docs-excludes  (thread-first (plist-get manifest* :docs-exclude)
                                       (ku-expand-files root)))
         (temp-directory (ku-temporary-file-directory)))
    (make-directory temp-directory t)
    (dolist (file docs-includes)
      (when (or (not (equal ".texi" (file-name-extension file t)))
                (member file docs-excludes))
        (setq docs-includes (delete file docs-includes))))
    (dolist (file (kb--makeinfo manifest docs-includes temp-directory))
      (rename-file file directory))))

(defun kb--collect-readme (manifest directory)
  (let* ((root        (car manifest))
         (manifest*   (cdr manifest))
         (readme-file (expand-file-name (plist-get manifest* :readme) root)))
    (copy-file readme-file (file-name-concat directory "README") t)))



(defun kb-manifest->define-package (manifest)
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
          :maintainer (ku-pair->cons maintainer)
          :authors    (ku-pairs->alist authors))))

(defun kb-build (manifest)
  "Build package tar file from MANIFEST that \\='package.el' understood.

The tar file contains directory what can be used in `load-path'."
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
          (kb--generate-pkg-file manifest package-directory)
          (kb--collect-src manifest package-directory)
          (kb--collect-assets manifest package-directory)
          (kb--collect-docs manifest package-directory)
          (kb--collect-readme manifest package-directory)
          (ku-call-process "tar" build-directory
                           "-cf" package-tar-file
                           package-fullname)
          (expand-file-name package-tar-file build-directory))
      (delete-directory package-directory t))))



;;;###autoload
(defun kosz-build-package ()
  "Build selected package.

Ask directory of package for build.  Resulted package will be placed in
\\='package-root/build' directory."
  (declare (interactive-only t))
  (interactive)
  (let* ((default-directory (read-directory-name "Package directory :")))
    (thread-last
      (project-current)
      (project-root)
      (km-read-manifest)
      (kb-build))))

;;;###autoload
(defun kosz-build-and-install-package ()
  "Build and install selected package.

Ask directory of package for install."
  (declare (interactive-only t))
  (interactive)
  (thread-last
    (call-interactively #'kosz-build-package)
    (package-install-file)))

;;;###autoload
(defun kosz-build-vc-package ()
  "Load package by vc and build it.

Ask package repository URL, DIRECTORY to place builded package, VCS for use and
REVISION of repository.  Revision can be empty."
  (declare (interactive-only t))
  (interactive)
  (let* ((temp-dir  (ku-temporary-file-directory))
         (remote    (read-string "Repository URL: "))
         (directory (read-directory-name "Diestination: "))
         (backend   (intern
                     (completing-read "VCR: " vc-handled-backends)))
         (revision* (read-string "Revision: "))
         (revision  (if (equal "" revision*) nil revision*)))
    (thread-first
      (vc-clone remote backend temp-dir revision)
      (km-read-manifest)
      (kb-build)
      (rename-file directory))))

;;;###autoload
(defun kosz-build-and-install-vc-package ()
  "Load package by vc and install it by package.el.

Ask package repository URL, VCS for use and REVISION of repository.
REVISION can be empty."
  (declare (interactive-only t))
  (interactive)
  (let* ((temp-dir (ku-temporary-file-directory))
         (remote   (read-string "Install from: "))
         (backend  (intern
                    (completing-read "Use backend: " vc-handled-backends)))
         (revision* (read-string "Revision: "))
         (revision  (if (equal "" revision*) nil revision*)))
    (thread-first
      (vc-clone remote backend temp-dir revision)
      (km-read-manifest)
      (kb-build)
      (package-install-file))))



(provide 'kosz-build)

;; Local Variables:
;; read-symbol-shorthands: (("kb-" . "kosz-build-")
;;                          ("km-" . "kosz-manifest-")
;;                          ("ku-" . "kosz-utils-"))
;; End:

;;; kosz-build.el ends here.
