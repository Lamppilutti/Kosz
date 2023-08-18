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



(defun kt--collect-tests (manifest)
  "Return list of tests files.

Use MANIFEST for getting information about test files."
  (let* ((root           (car manifest))
         (manifest*      (cdr manifest))
         (tests-includes (thread-first (plist-get manifest* :tests)
                                      (ku-expand-files root)))
         (tests-excludes (thread-first (plist-get manifest* :tests-exclude)
                                      (ku-expand-files root)))
         (files         nil))
    (dolist (file tests-includes files)
      (when (and (not (member file tests-excludes))
                 (equal ".el" (file-name-extension file t)))
        (push file files)))))

(defun kt--collect-src-directories (manifest)
  "Collect src dicrectories.

Use MANIFEST for getting information about src directories."
  (let* ((root         (car manifest))
         (manifest*    (cdr manifest))
         (src-includes (thread-first (plist-get manifest* :src)
                                     (ku-expand-files root)))
         (src-excludes (thread-first (plist-get manifest* :src-exclude)
                                     (ku-expand-files root)))
         (directories  nil))
    (dolist (file src-includes)
      (when (and (not (member file src-excludes))
                 (equal ".el" (file-name-extension file t)))
        (push (file-name-directory file) directories)))
    (delete-dups directories)))

(defun kt--call-test-process (directory load-directories load-files test-runner)
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
        (apply #'ku-call-process "emacs" directory
               "--batch" "--quick"
               (nconc --directories --load --funcall)) ; Order is important.
      (ku-external-process-error
       (alist-get :output process-error)))))



(defun kt-run-tests (manifest)
  "Run test described in package MANIFEST.

Return buffer with result of test execution."
  (let* ((manifest*       (cdr manifest))
         (temp-directory  (ku-temporary-file-directory))
         (test-runner     (plist-get manifest* :test-runner))
         (test-files     (kt--collect-tests manifest))
         (src-directories (kt--collect-src-directories manifest))
         (result-buffer   (generate-new-buffer
                            (format "* Kosz test result %s*" (gensym)))))
    (make-directory temp-directory)
    (with-current-buffer result-buffer
      (insert
       (kt--call-test-process temp-directory
                              src-directories
                              test-files
                              test-runner))
      (compilation-mode))
    result-buffer))



;;;###autoload
(defun kosz-test-package ()
  "Run tests of selected package.

Ask directory of package which tests need to run."
  (declare (interactive-only t))
  (interactive)
  (let* ((default-directory (read-directory-name "Package directory: ")))
    (thread-last
      (project-current)
      (project-root)
      (km-read-manifest)
      (kt-run-tests)
      (pop-to-buffer))))



(provide 'kosz-test)

;; Local Variables:
;; read-symbol-shorthands: (("kt-" . "kosz-test-")
;;                          ("km-" . "kosz-manifest-")
;;                          ("ku-" . "kosz-utils-"))
;; End:

;;; kosz-test.el ends here.
