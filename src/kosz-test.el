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
;; API for working with tests.

;;; Code:



(eval-when-compile
  (require 'subr-x))

(require 'package)
(require 'seq)

(require 'kosz-manifest)
(require 'kosz-utils)



(define-error 'ktest-package-testing-error
  "Error while running tests")



(defun ktest--get-tests (manifest)
  "Find listed in MANIFEST test files."
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (tests-includes (thread-first (plist-get manifest* :tests)
                                       (kutils-expand-files root)))
         (tests-excludes (thread-first (plist-get manifest* :tests-exclude)
                                       (kutils-expand-files root))))
    (thread-last
      (seq-difference tests-includes tests-excludes)
      (seq-filter (lambda (file)
                    (string= ".el" (file-name-extension file t)))))))

(defun ktest--get-src-files (manifest)
  "Find listed in MANIFEST source code files."
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (kutils-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (kutils-expand-files root))))
    (thread-last
      (seq-difference src-includes src-excludes)
      (seq-filter (lambda (file)
                    (string= ".el" (file-name-extension file t)))))))

(defun ktest--get-src-directories (manifest)
  "Find listed in MANIFEST directories what contains source code files."
  (thread-last
    (ktest--get-src-files manifest)
    (mapcar #'file-name-directory)
    (delete-dups)))

(defun ktest--get-dependencies (manifest)
  "Install listed in MANIFEST dependencies and return directories of them."
  (let* ((root             (car manifest))
         (manifest*        (cdr manifest))
         (dependencies     (plist-get manifest* :dependencies))
         (package-user-dir (file-name-concat root "build/dependencies")))
    (make-directory package-user-dir t)
    (dolist (dependency dependencies)
      (when-let* ((pkg-name    (car dependency))
                  (pkg-version (version-to-list (cadr dependency)))
                  (t* (not (package-installed-p pkg-name pkg-version))))
        (package-install pkg-name)))
    (mapcar #'directory-file-name
            (directory-files package-user-dir t
                             directory-files-no-dot-files-regexp))))

(defun ktest--call-test-process (directory load-path* files test-runner)
  "Run test process in DIRECTORY.

LOAD-PATH* is list of paths is appended to `load-path'.
FILES list of files is loaded in the process.
TEST-RUNNER is function is called for running tests.

If process ends with error return error message as result."
  (condition-case error
      (kutils-eval-in-other-process
       directory
       `(progn
          (setq load-path (append load-path ,load-path*))
          (mapc #'load-file ,files)
          (funcall ,test-runner)))
    (kutils-external-process-error
     (alist-get :output error))))



(defun ktest-test-package (manifest)
  "Run tests described in package MANIFEST.

Return buffer with result of test execution."
  (condition-case test-error
      (let* ((manifest*       (cdr manifest))
             (test-runner     (plist-get manifest* :test-runner))
             (test-files      (ktest--get-tests manifest))
             (src-directories (ktest--get-src-directories manifest))
             (result-buffer   (format "*Kosz test reuslt: '%s'*"
                                      (plist-get manifest* :name)))
             (temp-directory  (make-temp-file "kosz-" t))
             (inhibit-read-only t))
        (with-current-buffer result-buffer
          (unless (equal 'compilation-mode major-mode) (compilation-mode))
          (erase-buffer)
          (insert
           (ktest--call-test-process
            temp-directory
            (append (ktest--get-dependencies manifest) src-directories)
            test-files
            test-runner))
          (current-buffer)))
    (error (signal 'ktest-package-testing-error (cdr test-error)))))



(provide 'kosz-test)

;; Local Variables:
;; read-symbol-shorthands: (("ktest-"  . "kosz-test-")
;;                          ("kutils-" . "kosz-utils-"))
;; End:

;;; kosz-test.el ends here.
