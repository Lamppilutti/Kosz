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

;; Subroutines what could have been in Emacs, but they are not.

;;; Code:



(eval-when-compile
  (require 'subr-x))



(define-error 'kutils-external-process-error
  "Externall process ends with error")

(define-error 'kutils-validation-error
  "Plist has invalid properties")



(defmacro kutils-plist-validation (plist &rest property-cases)
  "Utility macros for validate PLIST.

PROPERTY-CASES is a list of (PROPERTY BIND COND MESSAGE) elements.
PROPERTY is a keyword property from MANIFEST.
BIND is a symbol to which property value will bind.
COND is an expression what returns boolean.
MESSAGE is a string describes what property value is expected.

If COND returns nil then the property name, value and MESSAGE will collected to
(:property PROPERTY :value PROPERTY's-value :expected MESSAGE) error form.  If
after checking all PROPERTY-CASES there is one or more error forms then signal
`kosz-utils-validation-error'.

\(fn PLIST (PROPERTY BIND COND MESSAGE)...)"
  (declare (indent 1))
  (let* ((errors-sym (gensym "errors"))
         (bindings   nil)
         (cases*     nil))
    (dolist (case property-cases)
      (let* ((property         (nth 0 case))
             (bind             (nth 1 case))
             (condition        (nth 2 case))
             (expected-message (nth 3 case)))
        (push `(,bind (plist-get ,plist ,property))
              bindings)
        (push `(unless ,condition
                 (push (list :property ,property
                             :value    ,bind
                             :expected ,expected-message)
                       ,errors-sym))
              cases*)))
    `(let* ((,errors-sym  nil)
            ,@bindings)
       ,@cases*
       (when ,errors-sym
         (signal 'kutils-validation-error
                 (list :invalid-properties ,errors-sym))))))

(defun kutils-buffer-string ()
  "Return the content of current buffer as a string without properties."
  (declare (side-effect-free t))
  (buffer-substring-no-properties (point-min) (point-max)))

(defun kutils-call-process (program directory &rest args)
  "Return the result of executing PROGRAM as string.

DIRECTORY is path to the directory in which PROGRAM will executed.
ARGS are strings passed as command arguments to PROGRAM.

Signal `kosz-utils--external-process-error' if PROGRAM ends with an error."
  (let* ((default-directory (or directory default-directory))
         (process-exit-code nil))
    (with-temp-buffer
      (setq process-exit-code (apply #'call-process program nil t nil args))
      (if (= 0 process-exit-code)
          (kutils-buffer-string)
        (signal 'kutils-external-process-error
                (list (cons :process   program)
                      (cons :args      args)
                      (cons :exit-code process-exit-code)
                      (cons :output    (kutils-buffer-string))))))))

(defun kutils-copy-file (file newname)
  "Copy FILE to NEWNAME.

Create directories in NEWNAME path, if they don't exist."
  (let* ((destination (file-name-directory newname)))
    (make-directory destination t)
    (copy-file file newname)))

(defun kutils-directory-files-recursively (file)
  "Return list of files.

If FILE is directory return its contant recursively. Otherwice if FILE is
actially file return (FILE) list."
  (declare (side-effect-free t))
  (if (file-directory-p file)
      (directory-files-recursively file directory-files-no-dot-files-regexp
                                   nil nil t)
    (list file)))

(defun kutils-temporary-file-directory ()
  "Return (almost) unique temporary file name."
  (declare (side-effect-free t))
  (thread-last
    (time-convert nil 'integer)
    (format "temp-%s")
    (file-name-concat (temporary-file-directory))
    (file-name-as-directory)))

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

(defun kutils-version-string-p (object)
  "Return t if OBJECT is string that `version-to-list' understood."
  (declare (pure t) (side-effect-free t))
  (condition-case _
      (version-to-list object)
    (error nil)))

(defun kutils-not-blank-string-p (object)
  "Return t if OBJECT is nil or not blank string."
  (declare (pure t) (side-effect-free t))
  (or (null object)
      (and (stringp object)
           (not (string-blank-p object)))))

(defun kutils-not-blank-string-p* (object)
  "Return t if OBJECT is not blank string."
  (declare (pure t) (side-effect-free t))
  (and (stringp object)
       (not (string-blank-p object))))

(defun kutils-symbolp (object)
  "Return t if OBJECT is a symbol, but not nil and not a keyword."
  (declare (pure t) (side-effect-free t))
  (and (symbolp object)
       (not (keywordp object))
       (not (null object))))

(defun kutils-pairp (object firstp secondp)
  "Return t if OBJECT is nil or pair.

Check the first element of pair by FIRSTP, and the second by SECONDP.

The pair is list of two elements, for example (1 2)."
  (declare (pure t) (side-effect-free t))
  (or (null object)
      (and (funcall firstp (car object))
           (funcall secondp (cdar object)))))

(defun kutils-list-of-pairs-p (object firstp secondp)
  "Return t if OBJECT is nil or a list of pairs.

Check the first element of pair by FIRSTP, and the second by SECONDP.

The pair is list of two elements, for example (1 2)."
  (declare (pure t) (side-effect-free t))
  (while (and (consp object)
              (funcall firstp (caar object))
              (funcall secondp (cadar object)))
    (setq object (cdr object)))
  (null object))

(defun kutils-functionp (object)
  "Return t if OBJECT is nil or function."
  (declare (pure t) (side-effect-free t))
  (or (null object)
      (functionp object)))

(defun kutils-pair->cons (pair)
  "Return cons created from PAIR.  If PAIR is nil return nil.

The pair is list of two elements, for example (1 2)."
  (if (null pair)
      nil
    (cons (car pair) (cadr pair))))

(defun kutils-pairs->alist (pairs)
  "Retun alist created from list of PAIRS.

The pair is list of two elements, for example (1 2)."
  (declare (pure t) (side-effect-free t))
  (let* ((res nil))
    (while (consp pairs)
      (push (cons (caar pairs) (cadar pairs)) res)
      (pop pairs))
    (nreverse res)))



(provide 'kosz-utils)

;; Local Variables:
;; read-symbol-shorthands: (("kutils-" . "kosz-utils-"))
;; End:

;;; kosz-utils.el ends here.
