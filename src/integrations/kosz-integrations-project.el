;;; kosz-integrations-projet.el ---  -*- lexical-binding: t; -*-

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
;; Integration with project package.
;;
;; NOTE THAT: This is internal feature you should not use it in your code.

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'cl-generic)
(require 'elisp-mode)
(require 'keymap)
(require 'project)

(require 'kosz-manifest)
(require 'kosz-build)
(require 'kosz-test)
(require 'kosz-extra)



(defvar-keymap kintegrations-project-prefix-map
  :doc "Keymap for kosz project commands."
  :parent project-prefix-map
  ;; "d" and "b" are already used, so bindings selected by the next mnemonics:
  ;; di[a]gnistics and bu[i]ld.
  "a" #'kosz-project-run-diagnostics
  "i" #'kosz-project-build
  "t" #'kosz-project-run-tests)



(defun kintegrations-project-setup-keymap ()
  "Setup extended keymap, if current buffer belongs to kosz project.

`kosz-integrations-project-prefix-map' will be setted localy."
  (condition-case setup-error
      (when-let* ((project (project-current))
                  (kintegrations-find-package-root (project-root project)))
        (keymap-local-set "C-x p" kintegrations-project-prefix-map))
    (error (message "Kosz setup keymap error: %S" setup-error))))

(defun kintegrations-project-try-kosz (directory)
  "Return (kosz . DIRECTORY) if DIRECTORY is subdirectory of kosz package.

The DIRECTORY will be abbreviated.
This function is used in `project-find-functions'."
  (when-let* ((directory (kextra-find-package-root directory)))
    (cons 'kosz (abbreviate-file-name directory))))



;;;###autoload
(defun kosz-project-build ()
  "Build current kosz project."
  (declare (interactive-only t))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (thread-last
      (kmanifest-read-manifest default-directory)
      (kbuild-build-package))))

;;;###autoload
(defun kosz-project-run-tests ()
  "Run tests for current kosz project."
  (declare (interactive-only t))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (thread-last
      (kmanifest-read-manifest default-directory)
      (ktest-run-tests)
      (pop-to-buffer))))

;;;###autoload
(defun kosz-project-run-diagnostics ()
  "Run diagnostics for current kosz project."
  (declare (interactive-only t))
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (thread-last
      (kmanifest-read-manifest default-directory)
      (ktest-run-diagnostics)
      (pop-to-buffer))))



(cl-defmethod project-root ((project (head kosz)))
  "Return root directory for kosz package PROJECT."
  (cdr project))

(with-eval-after-load 'project
  (add-hook 'after-change-major-mode-hook #'kintegrations-project-setup-keymap)
  (add-hook 'project-find-functions #'kintegrations-project-try-kosz))



(provide 'kosz-integrations-project)

;; Local Variables:
;; read-symbol-shorthands: (("kintegrations-" . "kosz-integrations-")
;;                          ("kmanifest-"     . "kosz-manifest-")
;;                          ("kbuild-"        . "kosz-build-")
;;                          ("ktest-"         . "kosz-test-")
;;                          ("kextra-"        . "kosz-extra-"))
;; End:

;;; kosz-integrations-project.el ends here.
