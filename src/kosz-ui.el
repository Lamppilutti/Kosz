;;; kosz-ui.el --- kosz user interface. -*- lexical-binding: t; -*-

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



(eval-when-compile
  (require 'subr-x))

(require 'cl-generic)
(require 'package)
(require 'project)

(require 'kosz-manifest)
(require 'kosz-build)



(defun kosz-ui-build-current-package ()
  "Build current package."
  (declare (interactive-only t))
  (interactive)
  (let* ((root (project-root (project-current))))
    (when root
      (kb-build (km-read-manifest root)))))

(defun kosz-ui-install-current-package ()
  "Build and install current package."
  (declare (interactive-only t))
  (interactive)
  (let* ((root (project-root (project-current))))
    (when root
      (thread-first
        (km-read-manifest root)
        (kb-build)
        (package-install-file)))))



(provide 'kosz-ui)

;; Local Variables:
;; read-symbol-shorthands: (("kui-" . "kosz-ui-")
;;                          ("km-"  . "kosz-manifest-")
;;                          ("kb-"  . "kosz-build-"))
;; End:

;;; kosz-ui.el ends here.
