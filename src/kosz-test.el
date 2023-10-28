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

(require 'checkdoc)
(require 'package)
(require 'seq)
(require 'warnings)

(require 'kosz-manifest)
(require 'kosz-utils)



(define-error 'ktest-test-error
  "Error while running tests")

(define-error 'ktest-diagnostics-error
  "Error while running diagnostics")



(defun ktest--defect< (key1 key2)
  "Return non-nil if KEY1 is less then KEY2.

1. Split KEYS to records of three fields:
file-name, line-number, collumn-number.
2. If file-name fields are not equal then compare them by `string<'.
3. Else if line-number fields are not equal then compare them by `='.
4. Else compare collumn-number fields by `='.

This function should be used in `sort-subr', so see its doc for information
about this function arguments."
  (let* ((string1       (buffer-substring-no-properties (car key1) (cdr key1)))
         (string2       (buffer-substring-no-properties (car key2) (cdr key2)))
         (string-parts1 (string-split string1 ":"))
         (file1         (nth 0 string-parts1))
         (line1         (string-to-number (nth 1 string-parts1)))
         (collumn1      (condition-case _
                            (string-to-number (nth 2 string-parts1))
                          (error 0)))
         (string-parts2 (string-split string2 ":"))
         (file2         (nth 0 string-parts2))
         (line2         (string-to-number (nth 1 string-parts2)))
         (collumn2      (condition-case _
                            (string-to-number (nth 2 string-parts2))
                          (error 0))))
    (if (string= file1 file2)
        (if (= line1 line2)
            (< collumn1 collumn2)
          (< line1 line2))
      (string< file1 file2))))

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

(defun ktest--checkdock-file (file)
  ;; Do what `checkdoc-file' does, but without shadowing
  ;; `checkdoc-diagnostic-buffer' variable.
  "Check FILE for document, comment, error style, and rogue spaces."
  (with-current-buffer (find-file-noselect file)
    (checkdoc-current-buffer t)))

(defun ktest--call-test-process (directory directories files test-runner)
  "Run tests in separate Emacs process.

Tests will runned inside DIRECTORY.  DIRECTORIES will added to `load-path'.
FILES will loaded by `load'.  TEST-RUNNER function will called after loading
DIRECTOIES and FILES, it should exit Emacs after work.

If process ends with error return error message as result."
  (let* ((--directories (mapcan (lambda (dir) (list "--directory" dir))
                                directories))
         (--load        (mapcan (lambda (file) (list "--load" file)) files))
         (--funcall     (list "--funcall" (format "%s" test-runner))))
    (condition-case process-error
        (apply #'kutils-call-process "emacs" directory
               "--batch" "--quick"
               (append --directories --load --funcall)) ; Order is important.
      (kutils-external-process-error
       (alist-get :output process-error)))))



(defun ktest-run-diagnostics (manifest)
  "Run diagnostics for project described in MANIFEST."
  (let* ((manifest*     (cdr manifest))
         (src-files     (ktest--get-src-files manifest))
         (result-buffer (format "*Kosz diagnostic for: '%s'*"
                                (plist-get manifest* :name)))
         (byte-compile-dest-file-function #'ignore)
         (warning-minimum-level           :emergency)
         (display-buffer-alist            (list
                                           (list result-buffer
                                                 #'display-buffer-no-window
                                                 '(allow-no-window t))))
         (byte-compile-log-buffer         result-buffer)
         (checkdoc-diagnostic-buffer      result-buffer)
         (inhibit-read-only               t))
    (condition-case diagnostics-error
        (with-current-buffer (get-buffer-create result-buffer)
          (save-excursion
            (erase-buffer)
            (mapc #'byte-compile-file src-files)
            (mapc #'ktest--checkdock-file src-files)
            (delete-non-matching-lines "^.*.el:[[:digit:]]+:"
                                       (point-min)
                                       (point-max))
            (sort-subr nil 'forward-line 'end-of-line nil nil #'ktest--defect<)
            (emacs-lisp-compilation-mode)
            (current-buffer)))
      (error (signal 'ktest-diagnostics-error (cdr diagnostics-error))))))

(defun ktest-run-tests (manifest)
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
          (when (not (equal 'compilation-mode major-mode)) (compilation-mode))
          (erase-buffer)
          (insert
           (ktest--call-test-process
            temp-directory
            (append (ktest--get-dependencies manifest) src-directories)
            test-files
            test-runner))
          (current-buffer)))
    (error (signal 'ktest-test-error (cdr test-error)))))



(provide 'kosz-test)

;; Local Variables:
;; read-symbol-shorthands: (("ktest-"  . "kosz-test-")
;;                          ("kutils-" . "kosz-utils-"))
;; End:

;;; kosz-test.el ends here.
