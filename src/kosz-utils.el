;;; kosz-utils.el --- utils subroutines. -*- lexical-binding: t; -*-

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

;; Additional subroutines what can be used separately from Kosz.

;;; Code:



(eval-when-compile
  (require 'subr-x))



(define-error 'ku-external-process-error
  "Externall process ends with error")



(defun ku-buffer-string ()
  "Return the content of current buffer as a string without properties."
  (declare (side-effect-free t))
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ku-call-process (program directory &rest args)
  "Return the result of executing PROGRAM as string.

DIRECTORY is path to the directory in which PROGRAM will executed.
ARGS are strings passed as command arguments to PROGRAM.

Signal kosz-utils--external-process-error if PROGRAM ends with an error."
  (let* ((default-directory (or directory default-directory))
         (process-exit-code nil))
    (with-temp-buffer
      (setq process-exit-code (apply #'call-process program nil t nil args))
      (if (= 0 process-exit-code)
          (ku-buffer-string)
        (signal 'ku-external-process-error
                (list (cons :process   program)
                      (cons :args      args)
                      (cons :exit-code process-exit-code)
                      (cons :output    (ku-buffer-string))))))))

(defun ku-copy-file (file newname)
  "Copy FILE to NEWNAME.

Create directories in NEWNAME path, if they don't exist."
  (let* ((destination (file-name-directory newname)))
    (when (not (file-exists-p destination))
      (make-directory destination t))
    (copy-file file newname)))

(defun ku-directory-files-recursively (file)
  "Return FILE directory files recursively, or list with FILE if it it is file.

Return list without directories."
  (declare (side-effect-free t))
  (if (file-directory-p file)
      (directory-files-recursively file directory-files-no-dot-files-regexp
                                   nil nil t)
    (list file)))

(defun ku-temporary-file-directory ()
  "Return (almost) unique temporary file name."
  (declare (side-effect-free t))
  (thread-last
    (time-convert nil 'integer)
    (format "temp-%s")
    (file-name-concat (temporary-file-directory))
    (file-name-as-directory)))

(defun ku-expand-files (files directory)
  "Recursively expand FILES relative to DIRECTORY.

If file is directory then recursively get files of this directory whithout
subdirectories."
  (declare (side-effect-free t))
  (let* ((expanded-files (list "")))
    (dolist (file files expanded-files)
      (thread-last
        (expand-file-name file directory)
        (ku-directory-files-recursively)
        (nconc expanded-files)))))

(defun ku-version-string-p (object)
  "Return t if OBJECT is string that `version-to-list' understood."
  (declare (pure t) (side-effect-free t))
  (condition-case _
      (version-to-list object)
    (error nil)))

(defun ku-not-blank-string-p (object)
  "Return t if OBJECT is not blank string or nil."
  (declare (pure t) (side-effect-free t))
  (or (and (stringp object)
           (not (string-blank-p object)))
      (null object)))

(defun ku-not-blank-string-p* (object)
  "Return t if OBJECT is not blank string."
  (declare (pure t) (side-effect-free t))
  (and (stringp object)
       (not (string-blank-p object))))

(defun ku-symbolp (object)
  "Return t if OBJECT is a symbol, but not nil and not a keyword."
  (declare (pure t) (side-effect-free t))
  (and (symbolp object)
       (not (keywordp object))
       (not (null object))))

(defun ku-list-of-pairs-p (object firstp secondp)
  "Return t if OBJECT is null or a list of pairs.

Check the first element of pair by FIRSTP, and the second by SECONDP.

The pair is list of two elements, for example (1 2)."
  (declare (pure t) (side-effect-free t))
  (while (and (consp object)
              (funcall firstp (caar object))
              (funcall secondp (cadar object)))
    (setq object (cdr object)))
  (null object))

;; From compat-29
(defun ku-list-of-strings-p (object)
  "Return t if OBJECT is nil or a list of strings."
  (declare (pure t) (side-effect-free t))
  (while (and (consp object) (stringp (car object)))
    (setq object (cdr object)))
  (null object))



(provide 'kosz-utils)

;; Local Variables:
;; read-symbol-shorthands: (("ku-" . "kosz-utils-"))
;; End:

;;; kosz-utils.el ends here.
