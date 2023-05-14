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
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ku-call-process (program directory &rest args)
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
  (let* ((destination (file-name-directory newname)))
    (when (not (file-exists-p destination))
      (make-directory destination t))
    (copy-file file newname)))

(defun ku-directory-files-recursively (file)
  (if (file-directory-p file)
      (directory-files-recursively file directory-files-no-dot-files-regexp
                                   nil nil t)
    (list file)))

(defun ku-temporary-file-directory ()
  (thread-last
    (time-convert nil 'integer)
    (format "kosz-%s")
    (file-name-concat (temporary-file-directory))
    (file-name-as-directory)))

(defun ku-expand-files (files directory)
  (let* ((expanded-files (list "")))
    (dolist (file files expanded-files)
      (thread-last
        (expand-file-name file directory)
        (ku-directory-files-recursively)
        (nconc expanded-files)))))

(defun ku-version-string-p (object)
  (condition-case _
      (version-to-list object)
    (error nil)))

(defun ku-not-blank-string-p (object)
  (and (stringp object)
       (not (string-blank-p object))))

(defun ku-not-blank-string-or-null-p (object)
  (or (and (stringp object)
           (not (string-blank-p object)))
      (null object)))

(defun ku-not-nil-symbol-p (object)
  (and (symbolp object)
       (not (null object))))

(defun ku-list-of-pairs-p (object car-pred cadr-pred)
  (and (proper-list-p object)
       (thread-last
         object
         (mapcar (lambda (elm)
                   (and (funcall car-pred  (car elm))
                        (funcall cadr-pred (cdr elm)))))
         (member nil)
         (not))))

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
