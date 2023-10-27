;;; kosz-integrations.el ---  -*- lexical-binding: t; -*-

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
;; Integrations what can't be refered to specific package and some utils
;; subroutines for other integrations.

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'elisp-mode)

(require 'kosz-manifest)
(require 'kosz-utils)
(require 'kosz-extra)



(defun kintegrations-setup-elisp-flymake-byte-compile-load-path ()
  "Setup source code directories of kosz package for current buffer."
  (condition-case setup-error
      ;; Manifest properties can be nil, so use bindings only for check in
      ;; whin-let*.
      (when-let* ((file (buffer-file-name))
                  (root (kextra-find-package-root
                         (file-name-directory file))))
        (let* ((manifest     (cdr (kmanifest-read-manifest root)))
               (src-includes (thread-first (plist-get manifest :src)
                                           (kutils-expand-files root)))
               (src-excludes (thread-first (plist-get manifest :src-exclude)
                                           (kutils-expand-files root))))
          (make-local-variable 'elisp-flymake-byte-compile-load-path)
          (thread-last
            (seq-difference src-includes src-excludes)
            (seq-filter
             (lambda (file) (string= ".el" (file-name-extension file t))))
            (mapcar #'file-name-directory)
            (mapcar #'directory-file-name)
            (mapc (apply-partially #'add-to-list
                                   'elisp-flymake-byte-compile-load-path)))))
    (error (message "Kosz setup paths error: %S" setup-error))))



(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            #'kintegrations-setup-elisp-flymake-byte-compile-load-path))



(provide 'kosz-integrations)

;; Local Variables:
;; read-symbol-shorthands: (("kintegrations-" . "kosz-integrations-")
;;                          ("kmanifest-"     . "kosz-manifest-")
;;                          ("kutils-"        . "kosz-utils-")
;;                          ("kextra-"        . "kosz-extra-"))
;; End:

;;; kosz-integrations.el ends here.
