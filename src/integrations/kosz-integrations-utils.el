;;; kosz-integrations-utils.el ---  -*- lexical-binding: t; -*-

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
;; Util subroutines for kosz-integrations.
;; This is not part of kosz-utils becouse it requires the base kosz package.
;;
;; NOTE THAT: This is internal feature you should not use it in your code.

;;; Code:



(require 'kosz-manifest)



(defun kintegrations-utils-find-package-root (directory)
  "Return root directory for package stored in DIRECTORY."
  (named-let find-root ((dir directory))
    (cond
     ((null dir) nil)
     ((file-exists-p (expand-file-name kmanifest-manifest-file dir)) dir)
     (t (find-root (file-name-parent-directory dir))))))



(provide 'kosz-integrations-utils)

;; Local Variables:
;; read-symbol-shorthands: (("kintegrations-" . "kosz-integrations-")
;;                          ("kmanifest-"     . "kosz-manifest-"))
;; End:

;;; kosz-integrations-utils.el ends here.
