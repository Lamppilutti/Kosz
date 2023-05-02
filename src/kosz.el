;;; kosz.el --- core functional. -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Lämppi Lütti <lamppilutti@gmail.com>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:



(defconst @:manifest-file "package.kosz")



(define-error '@:external-process-error
  "Externall process ends with error")



(defun @::buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun @::call-process (program directory &rest args)
  (let* ((default-directory (or directory default-directory))
         (process-exit-code nil))
    (with-temp-buffer
      (setf process-exit-code (apply #'call-process program nil t nil args))
      (if (= 0 process-exit-code)
          (@::buffer-string)
        (signal '@:external-process-error
                (list (cons :process   program)
                      (cons :args      args)
                      (cons :exit-code process-exit-code)
                      (cons :output    (@::buffer-string))))))))

(defun @::define-package ()
  `(defun define-package (name version &rest properties)
     (plist-put properties :name name)
     (plist-put properties :version version)
     (prin1 properties)
     (setf kill-emacs-hook nil)
     (kill-emacs)))

(defun @::manifest->define-package (manifest)
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
          :maintainer maintainer
          :authors (seq-into authors 'list))))

(defun @::generate-pkg-file (manifest directory)
  (let* ((name      (plist-get manifest :name))
         (file-name (format "%s-pkg.el" name)))
    (with-temp-file (file-name-concat directory file-name)
      (insert (format "%S\n" (@::manifest->define-package manifest))
              "\n;; Local Variables:\n;; no-byte-compile: t\n;; End:"))))



(defun @:read-manifest (directory)
  (setq directory (expand-file-name directory))
  (with-temp-buffer
    (insert
     (@::call-process "emacs" directory
                      "--batch" "--quick"
                      "--eval" (format "%S" (@::define-package))
                      "--load" @:manifest-file))
    (cons (abbreviate-file-name directory)
          (sexp-at-point))))

(defun @:build-for-package-el (manifest)
  (let* ((root              (car manifest))
         (manifest*         (cdr manifest))
         (package-fullname  (format "%s-%s"
                                    (plist-get manifest* :name)
                                    (plist-get manifest* :version)))
         (build-directory   (file-name-concat root "build"))
         (package-directory (file-name-concat build-directory package-fullname))
         (package-tar-file  (format "%s.tar" package-fullname)))
    (unwind-protect
        (progn
          (make-directory package-directory t)
          (@::generate-pkg-file manifest* package-directory)
          (@::call-process "tar" build-directory
                           "-cf" package-tar-file
                           package-fullname)
          (expand-file-name package-tar-file build-directory))
      (delete-directory package-directory t))))



(provide 'kosz)

;; Local Variables:
;; read-symbol-shorthands: (("@::" . "kosz--")
;;                          ("@:"  . "kosz-"))
;; End:

;;; kosz.el ends here.
