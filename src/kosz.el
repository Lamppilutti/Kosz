;;; kosz.el --- -*- lexical-binding: t; -*-

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

;;; Code:



(require 'kosz-manifest)
(require 'kosz-build)
(require 'kosz-test)
(require 'kosz-extra)

(require 'kosz-integrations nil t)
(require 'kosz-integrations-project nil t)



;;;###autoload
(defun kosz-build-selected-package (directory)
  (declare (interactive-only kbuild-build))
  (interactive "DPackage directory: ")
  (if-let* ((default-directory (kextra-find-package-root directory)))
      (kbuild-build-package (kmanifest-read-manifest default-directory))
    (user-error "Directory is not part of package" directory)))

;;;###autoload
(defun kosz-test-selected-package (directory)
  (declare (interactive-only ktest-run-tests))
  (interactive "DPackage directory: ")
  (if-let* ((default-directory (kextra-find-package-root directory)))
      (thread-last
        (kmanifest-read-manifest default-directory)
        (ktest-test-package)
        (pop-to-buffer))
    (user-error "Directory is not part of package" directory)))

;;;###autoload
(defun kosz-diagnose-selected-package (directory)
  (declare (interactive-only ktest-run-diagnostics))
  (interactive "DPackage directory: ")
  (if-let* ((default-directory (kextra-find-package-root directory)))
      (thread-last
        (kmanifest-read-manifest default-directory)
        (ktest-diagnose-package)
        (pop-to-buffer))
    (user-error "Directory is not part of package" directory)))



(provide 'kosz)

;; Local Variables:
;; read-symbol-shorthands: (("kmanifest-" . "kosz-manifest-")
;;                          ("kbuild-"    . "kosz-build-")
;;                          ("ktest-"     . "kosz-test-")
;;                          ("kextra-"    . "kosz-extra-"))
;; End:

;;; kosz.el ends here.
