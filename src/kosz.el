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



(eval-when-compile
  (require 'subr-x))



(defconst $:manifest-file "package.kosz")



(define-error '$:external-process-error
  "Externall process ends with error")

(define-error '$:manifest-validation-error
  "Invalid manifest properties")



(defun $::buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun $::call-process (program directory &rest args)
  (let* ((default-directory (or directory default-directory))
         (process-exit-code nil))
    (with-temp-buffer
      (setq process-exit-code (apply #'call-process program nil t nil args))
      (if (= 0 process-exit-code)
          ($::buffer-string)
        (signal '$:external-process-error
                (list (cons :process   program)
                      (cons :args      args)
                      (cons :exit-code process-exit-code)
                      (cons :output    ($::buffer-string))))))))

(defun $::copy-file (file newname)
  (let* ((destination (file-name-directory newname)))
    (when (not (file-exists-p destination))
      (make-directory destination t))
    (copy-file file newname)))

(defun $::directory-files-recursively (file)
  (if (file-directory-p file)
      (directory-files-recursively file directory-files-no-dot-files-regexp
                                   nil nil t)
    (list file)))

(defun $::temporary-file-directory ()
  (~>> (time-convert nil 'integer)
       (format "kosz-%s")
       (file-name-concat (temporary-file-directory))
       (file-name-as-directory)))

(defun $::expand-files (files directory)
  (let* ((expanded-files (list "")))
    (dolist (file files expanded-files)
      (~>> (expand-file-name file directory)
           ($::directory-files-recursively)
           (nconc expanded-files)))))

(defun $::version-string-p (object)
  (condition-case _
      (version-to-list object)
    (error nil)))

(defun $::not-blank-string-p (object)
  (and (stringp object)
       (not (string-blank-p object))))

(defun $::not-blank-string-or-null-p (object)
  (or (and (stringp object)
           (not (string-blank-p object)))
      (null object)))

(defun $::not-nil-symbol-p (object)
  (and (symbolp object)
       (not (null object))))

(defun $::list-of-pairs-p (object car-pred cadr-pred)
  (and (proper-list-p object)
       (~>> object
            (mapcar
             (lambda (elm)
               (and (funcall car-pred  (car elm))
                    (funcall cadr-pred (cdr elm)))))
            (member nil)
            (not))))

;; From compat
(defun $::list-of-strings-p (object)
  "Return t if OBJECT is nil or a list of strings."
  (declare (pure t) (side-effect-free t))
  (while (and (consp object) (stringp (car object)))
    (setq object (cdr object)))
  (null object))



(defmacro $::manifest-validation (manifest &rest property-cases)
  (declare (indent 1))
  (let* ((errors-sym (gensym "errors"))
         (bindings   nil)
         (cases*     nil))
    (dolist (case property-cases)
      (let* ((property         (nth 0 case))
             (bind             (nth 1 case))
             (condition        (nth 2 case))
             (expected-message (nth 3 case)))
        (push `(,bind (plist-get ,manifest ,property))
              bindings)
        (push `(unless ,condition
                 (push (list :property ,property
                             :value ,bind
                             :expected ,expected-message)
                       ,errors-sym))
              cases*)))
    `(let* ((,errors-sym  nil)
            ,@bindings)
       ,@cases*
       (when ,errors-sym
         (signal 'manifest-validation-error
                 (list :invalid-properties ,errors-sym))))))

(defun $::makeinfo (files directory)
  (apply #'$::call-process "makeinfo" directory files)
  (dolist (file ($::directory-files-recursively directory))
    ($::call-process "install-info" directory file "dir"))
  ($::directory-files-recursively directory))

(defun $::define-package ()
  `(defun define-package (name version &rest properties)
     (plist-put properties :name name)
     (plist-put properties :version version)
     (prin1 properties)
     (setq kill-emacs-hook nil)
     (kill-emacs)))

(defun $::generate-pkg-file (manifest)
  (setq manifest (cdr manifest))
  (let* ((name      (plist-get manifest :name))
         (file-name (format "%s-pkg.el" name)))
    (with-temp-file file-name
      (insert (format "%S\n" ($:manifest->define-package manifest))
              ;;; Monolitic line breaks emacs.
              "\n;; Local " "Variables:\n;; no-byte-compile: t\n;; End:"))))

(defun $::collect-src (manifest)
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (~> (plist-get manifest* :src)
                           ($::expand-files root)))
         (src-excludes (~> (plist-get manifest* :src-exclude)
                           ($::expand-files root))))
    (dolist (file src-includes)
      (when (and (not (member file src-excludes))
                 (equal ".el" (file-name-extension file t)))
        (copy-file file default-directory t)))))

(defun $::collect-assets (manifest)
  (let* ((root            (car manifest))
         (manifest*       (cdr manifest))
         (assets-includes (~> (plist-get manifest* :assets)
                              ($::expand-files root)))
         (assets-excludes (~> (plist-get manifest* :assets-exclude)
                              ($::expand-files root))))
    (dolist (file assets-includes)
      (when (not (member file assets-excludes))
        (~>> (file-relative-name file root)
             (file-name-concat default-directory)
             ($::copy-file file))))))

(defun $::collect-docs (manifest)
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (docs-includes  (~> (plist-get manifest* :docs)
                             ($::expand-files root)))
         (docs-excludes  (~> (plist-get manifest* :docs-exclude)
                             ($::expand-files root)))
         (temp-directory ($::temporary-file-directory)))
    (unwind-protect
        (progn
          (make-directory temp-directory t)
          (dolist (file docs-includes)
            (when (or (not (equal ".texi" (file-name-extension file t)))
                      (member file docs-excludes))
              (setq docs-includes (delete file docs-includes))))
          (dolist (file ($::makeinfo docs-includes temp-directory))
            (rename-file file default-directory)))
      (delete-directory temp-directory t))))



(defun $:read-manifest (directory)
  (setq directory (expand-file-name directory))
  (with-temp-buffer
    (insert
     ($::call-process "emacs" directory
                      "--batch" "--quick"
                      "--eval" (format "%S" ($::define-package))
                      "--load" $:manifest-file))
    (cons (abbreviate-file-name directory)
          (sexp-at-point))))

(defun $:validate-manifest (manifest)
  (setq manifest (cdr manifest))
  ($::manifest-validation manifest
    (:name
     name ($::not-nil-symbol-p name)
     "Not nil symbol")
    (:version
     version ($::version-string-p version)
     "String of a form that can be understood by `version-to-list'")
    (:description
     desc ($::not-blank-string-or-null-p desc)
     "Not blank string or null")
    (:dependencies
     deps ($::list-of-pairs-p deps #'$::not-nil-symbol-p #'$::version-string-p)
     "List of (not nil symbol - `version-to-list' undestandable string) pairs")
    (:url
     url ($::not-blank-string-p url)
     "String or null")
    (:authors
     authors ($::list-of-pairs-p
              authors #'$::not-blank-string-p #'$::not-blank-string-p)
     "List of (not blank string - not blank string) pairs")
    (:license
     license ($::not-blank-string-or-null-p license)
     "Not blank string or null")
    (:src
     src ($::list-of-strings-p src)
     "List if strings")
    (:src-exclude
     src-ex ($::list-of-strings-p src-ex)
     "List if strings")
    (:docs
     docs ($::list-of-strings-p docs)
     "List if strings")
    (:docs-exclude
     docs-ex ($::list-of-strings-p docs-ex)
     "List if strings")
    (:assets
     assets ($::list-of-strings-p assets)
     "List if strings")
    (:assets-exclide
     assets-ex ($::list-of-strings-p assets-ex)
     "List if strings")))

(defun $:manifest->define-package (manifest)
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

(defun $:build-for-package-el (manifest)
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
          ($::generate-pkg-file manifest)
          ($::collect-src manifest)
          ($::collect-assets manifest)
          ($::collect-docs manifest)
          ($::call-process "tar" build-directory
                           "-cf" package-tar-file
                           package-fullname)
          (expand-file-name package-tar-file build-directory))
      (delete-directory package-directory t))))



(provide 'kosz)

;; Local Variables:
;; read-symbol-shorthands: (("$::" . "kosz--")
;;                          ("$:"  . "kosz-")
;;                          ("~>>" . "thread-last")
;;                          ("~>"  . "thread-first"))
;; End:

;;; kosz.el ends here.
