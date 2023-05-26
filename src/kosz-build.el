;;; kosz-build.el --- build packages for default emacs load-flow. -*- lexical-binding: t; -*-

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

(require 'pp)

(require 'kosz-manifest)
(require 'kosz-utils)



(defun kb--makeinfo (files directory)
  (apply #'ku-call-process "makeinfo" directory files)
  (dolist (file (ku-directory-files-recursively directory))
    (ku-call-process "install-info" directory file "dir"))
  (ku-directory-files-recursively directory))


(defun kb--generate-pkg-file (manifest)
  (setq manifest (cdr manifest))
  (let* ((name      (plist-get manifest :name))
         (file-name (format "%s-pkg.el" name)))
    (with-temp-file file-name
      (pp (kb-manifest->define-package manifest) (current-buffer))
      (insert "\n;; Local Variables:\n;; no-byte-compile: t\n;; End:\n"))))

(defun kb--collect-src (manifest)
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (ku-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (ku-expand-files root))))
    (dolist (file src-includes)
      (when (and (not (member file src-excludes))
                 (equal ".el" (file-name-extension file t)))
        (copy-file file default-directory t)))))

(defun kb--collect-assets (manifest)
  (let* ((root            (car manifest))
         (manifest*       (cdr manifest))
         (assets-includes (thread-first (plist-get manifest* :assets)
                                        (ku-expand-files root)))
         (assets-excludes (thread-first (plist-get manifest* :assets-exclude)
                                        (ku-expand-files root))))
    (dolist (file assets-includes)
      (when (not (member file assets-excludes))
        (thread-last (file-relative-name file root)
                     (file-name-concat default-directory)
                     (ku-copy-file file))))))

(defun kb--collect-docs (manifest)
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (docs-includes  (thread-first (plist-get manifest* :docs)
                                       (ku-expand-files root)))
         (docs-excludes  (thread-first (plist-get manifest* :docs-exclude)
                                       (ku-expand-files root)))
         (temp-directory (ku-temporary-file-directory)))
    (unwind-protect
        (progn
          (make-directory temp-directory t)
          (dolist (file docs-includes)
            (when (or (not (equal ".texi" (file-name-extension file t)))
                      (member file docs-excludes))
              (setq docs-includes (delete file docs-includes))))
          (dolist (file (kb--makeinfo docs-includes temp-directory))
            (rename-file file default-directory)))
      (delete-directory temp-directory t))))

(defun kb-manifest->define-package (manifest)
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
          :authors    authors)))

(defun kb-build-for-package-el (manifest)
  (let* ((root              (car manifest))
         (manifest*         (cdr manifest))
         (package-fullname  (format "%s-%s"
                                    (plist-get manifest* :name)
                                    (plist-get manifest* :version)))
         (package-tar-file  (format "%s.tar" package-fullname))
         (build-directory   (file-name-concat root "build"))
         (package-directory (file-name-concat build-directory package-fullname))
         (default-directory (file-name-as-directory package-directory)))
    (unwind-protect
        (progn
          (make-directory package-directory t)
          (kb--generate-pkg-file manifest)
          (kb--collect-src manifest)
          (kb--collect-assets manifest)
          (kb--collect-docs manifest)
          (ku-call-process "tar" build-directory
                           "-cf" package-tar-file
                           package-fullname)
          (expand-file-name package-tar-file build-directory))
      (delete-directory package-directory t))))



(provide 'kosz-build)

;; Local Variables:
;; read-symbol-shorthands: (("kb-" . "kosz-build-")
;;                          ("km-" . "kosz-manifest-")
;;                          ("ku-" . "kosz-utils-"))
;; End:

;;; kosz-build.el ends here.
