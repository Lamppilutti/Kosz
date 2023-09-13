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

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'cl-generic)
(require 'project)

(require 'kosz-manifest)



;;;###autoload
(defun kosz-integrations-project--find-project (dir)
    (named-let find-project ((dir dir))
      (cond
       ((null dir) nil)
       ((file-exists-p (expand-file-name kmanifest-manifest-file dir))
        (cons 'kosz dir))
       (t (find-project (file-name-parent-directory dir))))))



(cl-defmethod project-root ((project (head kosz)))
  (cdr project))



;;;###autoload
(with-eval-after-load 'project
  (add-hook 'project-find-functions #'kosz-integrations-project--find-project))



(provide 'kosz-integrations-project)

;; Local Variables:
;; read-symbol-shorthands: (("kip-" . "kosz-integrations-project-")
;;                          ("kmanifest-"  . "kosz-manifest-"))
;; End:

;;; kosz-integrations-project.el ends here.
