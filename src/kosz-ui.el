;;; kosz-ui.el --- -*- lexical-binding: t; -*-

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
;; Kosz user interface.

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'cl-generic)
(require 'package)
(require 'project)

(require 'kosz-manifest)
(require 'kosz-build)
(require 'kosz-test)
(require 'kosz-utils)



;;;###autoload
(defun kosz-ui-build-package ()
  "Build selected package.

Ask directory of package for build.  Resulted package will be placed in
\\='package-root/build' directory."
  (declare (interactive-only t))
  (interactive)
  (let* ((default-directory (read-directory-name "Package directory :")))
    (thread-last
      (project-current)
      (project-root)
      (kmanifest-read-manifest)
      (kbuild-build))))

;;;###autoload
(defun kosz-ui-build-and-install-package ()
  "Build and install selected package.

Ask directory of package for install."
  (declare (interactive-only t))
  (interactive)
  (thread-last
    (call-interactively #'kosz-ui-build-package)
    (package-install-file)))

;;;###autoload
(defun kosz-ui-build-vc-package ()
  "Load package by vc and build it.

Ask package repository URL, DIRECTORY to place builded package, VCS for use and
REVISION of repository.  Revision can be empty."
  (declare (interactive-only t))
  (interactive)
  (let* ((temp-dir  (kutils-temporary-file-directory))
         (remote    (read-string "Repository URL: "))
         (directory (read-directory-name "Diestination: "))
         (backend   (intern
                     (completing-read "VCR: " vc-handled-backends)))
         (revision* (read-string "Revision: "))
         (revision  (if (equal "" revision*) nil revision*)))
    (thread-first
      (vc-clone remote backend temp-dir revision)
      (kmanifest-read-manifest)
      (kbuild-build)
      (rename-file directory))))

;;;###autoload
(defun kosz-ui-build-and-install-vc-package ()
  "Load package by vc and install it by package.el.

Ask package repository URL, VCS for use and REVISION of repository.
REVISION can be empty."
  (declare (interactive-only t))
  (interactive)
  (let* ((temp-dir (kutils-temporary-file-directory))
         (remote   (read-string "Install from: "))
         (backend  (intern
                    (completing-read "Use backend: " vc-handled-backends)))
         (revision* (read-string "Revision: "))
         (revision  (if (equal "" revision*) nil revision*)))
    (thread-first
      (vc-clone remote backend temp-dir revision)
      (kmanifest-read-manifest)
      (kbuild-build)
      (package-install-file))))

;;;###autoload
(defun kosz-ui-test-package ()
  "Run tests of selected package.

Ask directory of package which tests need to run."
  (declare (interactive-only t))
  (interactive)
  (let* ((default-directory (read-directory-name "Package directory: ")))
    (thread-last
      (project-current)
      (project-root)
      (kmanifest-read-manifest)
      (ktest-run-tests)
      (pop-to-buffer))))



(provide 'kosz-ui)

;; Local Variables:
;; read-symbol-shorthands: (("kmanifest-" . "kosz-manifest-")
;;                          ("kbuild-"    . "kosz-build-")
;;                          ("ktest-"     . "kosz-test-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-ui.el ends here.
