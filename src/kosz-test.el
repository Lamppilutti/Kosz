;;; kosz-test.el --- -*- lexical-binding: t; -*-

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
;; User interfaces for runing tests.

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'package)
(require 'project)

(require 'kosz-manifest)
(require 'kosz-utils)



(defun ktest--ensure-deps (manifest)
  "Ensure all of the dependencies from MANIFEST are installed."
  (setq manifest (cdr manifest))
  (dolist (dependency (plist-get manifest :dependencies))
    (when-let* ((dependency* (car dependency))
                (min-version (version-to-list (cadr dependency)))
                (t* (not (package-installed-p dependency* min-version))))
      (package-install dependency*))))

(defun ktest--collect-tests (manifest)
  "Return list of test files.

Use MANIFEST for getting information about test files."
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (tests-includes (thread-first (plist-get manifest* :tests)
                                       (kutils-expand-files root)))
         (tests-excludes (thread-first (plist-get manifest* :tests-exclude)
                                       (kutils-expand-files root)))
         (files          nil))
    (dolist (file tests-includes files)
      (when (and (not (member file tests-excludes))
                 (equal ".el" (file-name-extension file t)))
        (push file files)))))

(defun ktest--collect-src-directories (manifest)
  "Collect src dicrectories.

Use MANIFEST for getting information about src directories."
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (kutils-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (kutils-expand-files root)))
         (directories  nil))
    (dolist (file src-includes)
      (when (and (not (member file src-excludes))
                 (equal ".el" (file-name-extension file t)))
        (push (file-name-directory file) directories)))
    (delete-dups directories)))

(defun ktest--call-test-process (directory load-directories load-files test-runner)
  "Run tests in separate Emacs process.

Tests will runned inside DIRECTORY.  LOAD-DIRECTORIES will added to `load-path'.
LOAD-FILES will loaded by `load'.  TEST-RUNNER will called after loading
LOAD-DIRECTOIES and LOAD-FILES.

If process ends with error return error message as result."
  (let* ((--directories (mapcan (lambda (dir) (list "--directory" dir))
                                load-directories))
         (--load        (mapcan (lambda (file) (list "--load" file))
                                load-files))
         (--funcall     (list "--funcall" (format "%s" test-runner))))
    (condition-case process-error
        (apply #'kutils-call-process "emacs" directory
               "--batch" "--quick"
               (nconc --directories --load --funcall)) ; Order is important.
      (kutils-external-process-error
       (alist-get :output process-error)))))

(defun ktest--load-path ()
  "Construct the `load-path' for project.
Load path is based on all of the packages under
`package-user-dir'."
  (mapcar
   #'directory-file-name
   (directory-files package-user-dir t directory-files-no-dot-files-regexp)))

(defun ktest--package-dir (root)
  "Format package directory name in project's ROOT."
  (expand-file-name
   (format "kosz-elpa-%s" emacs-version)
   root))



(defun ktest-run-tests (manifest)
  "Run test described in package MANIFEST.

Return buffer with result of test execution."
  (let* ((root             (car manifest))
         (manifest*        (cdr manifest))
         (temp-directory   (kutils-temporary-file-directory))
         (test-runner      (plist-get manifest* :test-runner))
         (test-files       (ktest--collect-tests manifest))
         (src-directories  (ktest--collect-src-directories manifest))
         (result-buffer    (generate-new-buffer
                            (format "* Kosz test result %s*" (gensym))))
         (package-user-dir (ktest--package-dir root)))
    (make-directory temp-directory t)
    (make-directory package-user-dir t)
    (ktest--ensure-deps manifest)
    (with-current-buffer result-buffer
      (insert
       (ktest--call-test-process temp-directory
                              (append src-directories (ktest--load-path))
                              test-files
                              test-runner))
      (compilation-mode))
    result-buffer))



(provide 'kosz-test)

;; Local Variables:
;; read-symbol-shorthands: (("ktest-"     . "kosz-test-")
;;                          ("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-test.el ends here.
