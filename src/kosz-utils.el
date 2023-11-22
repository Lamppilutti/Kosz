;;; kosz-utils.el ---  -*- lexical-binding: t; -*-

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

;; Util subroutines for kosz.
;;
;; NOTE THAT: This is internal feature you should not use it in your code.

;;; Code:



(eval-when-compile
  (require 'subr-x))



(define-error 'kutils-external-process-error
  "Externall process ends with error")



(defun kutils-call-process (program directory &rest args)
  "Return the result of executing PROGRAM as string.

DIRECTORY is path to the directory in which PROGRAM will executed.
ARGS are strings passed as command arguments to PROGRAM.

Signal `kosz-utils--external-process-error' if PROGRAM ends with an error."
  (let* ((default-directory (or directory default-directory))
         (process-exit-code nil))
    (with-temp-buffer
      (setq process-exit-code (apply #'call-process program nil t nil args))
      (unless (= 0 process-exit-code)
        (signal 'kutils-external-process-error
                (list (cons :process   program)
                      (cons :args      args)
                      (cons :exit-code process-exit-code)
                      (cons :output    (buffer-substring-no-properties
                                        (point-min)
                                        (point-max)))))))))

(defun kutils-eval-in-other-process (directory form)
  "Run Emacs process inside DIRECTORY and eval FORM in it.

Process starts as \"--batch --quick\" and with predefined
`debugger-stack-frame-as-list' as t.  Other Emacs initialization should be in
FORM.
FORM can use `kosz--return-from-process' function for returning some Lisp object
to parent process.  If form contains multiple calls of this function then result
of last call will returned.
Signal `kosz-utils--external-process-error' if process ends with an error.

Return Lisp object passed to `kosz--return-from-process', if FORM does not
call this function return nil."
  (let* ((temp-file (make-temp-name ".kosz-process-")))
    (unwind-protect
        (with-temp-buffer
          (kutils-call-process
           "emacs" directory
           "--batch" "--quick"
           "--eval"  "(setq debugger-stack-frame-as-list t)"
           "--eval"  (format "(defun kosz--return-from-process (object)
                               (with-temp-file %S
                                 (insert (format \"%%S\\n\" object))))"
                             temp-file)
           "--eval"  (format "%S" form))
          (insert-file-contents temp-file)
          (sexp-at-point))
      (delete-file temp-file))))

(defun kutils-directory-files-recursively (file)
  "As `directory-files-recursively', but if FILE is file return list with it.

If FILE is directory return its contant recursively.  Otherwice if FILE is
actially file return (FILE) list."
  (declare (side-effect-free t))
  (if (file-directory-p file)
      (directory-files-recursively file directory-files-no-dot-files-regexp
                                   nil nil t)
    (list file)))

(defun kutils-expand-files (files directory)
  "Recursively expand FILES relative to DIRECTORY.

If file is directory then recursively get files of it whithout subdirectories."
  (declare (side-effect-free t))
  (let* ((expanded-files nil))
    (dolist (file files expanded-files)
      (thread-last
        (expand-file-name file directory)
        (kutils-directory-files-recursively)
        (nconc expanded-files)
        (setq expanded-files)))))



(provide 'kosz-utils)

;; Local Variables:
;; read-symbol-shorthands: (("kutils-" . "kosz-utils-"))
;; End:

;;; kosz-utils.el ends here.
