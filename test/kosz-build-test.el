;;; kosz-build-test.el --- -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Lämppi Lütti <lamppilutti@gmail.com>

;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty, provided the copyright notice and
;; this notice are preserved. This file is offered as-is, without any warranty.

;;; Commentary:

;;; Code:



(require 'ert)

(require 'kosz-build)



(ert-deftest kbuild-manifest->define-package-ok ()
  "Test manifest converted to \\=`define-package' form correctly."
  (should (equal '(define-package "test" "1.1.1"
                    "description" ((package "1.1.1"))
                    :url        "url"
                    :commit     "commit"
                    :keywords   ("keywords")
                    :maintainer ("name" . "email")
                    :authors    (("name" . "email")))
                 (kbuild-manifest->define-package
                  (cons nil (list :name        'test
                                  :version      "1.1.1"
                                  :description  "description"
                                  :dependencies '((package "1.1.1"))
                                  :url          "url"
                                  :commit       "commit"
                                  :keywords     '("keywords")
                                  :maintainer   '("name" "email")
                                  :authors      '(("name" "email"))
                                  :other-prop   :val))))))



(ert-deftest kbuild-build-docs-test-ok ()
  "Test docs builded without errors."
  (make-empty-file "docs.texi" t)
  (let* ((directiry (kbuild-build-docs
                     (cons default-directory (list :name    'test
                                                   :version "1.1.1"
                                                   :docs    '("docs.texi"))))))
    (should (and (member "docs.info" (directory-files directiry))
                 (member "dir" (directory-files directiry))))))

(ert-deftest kbuild-build-package-test-ok ()
  "Test package builded without errors."
  (make-empty-file "docs.texi" t)
  (make-empty-file "readme" t)
  (make-empty-file "src.el" t)
  (let* ((pkg-file (kbuild-build-package
                    (cons default-directory (list :name    'test
                                                  :version "1.1.1"
                                                  :readme  "readme"
                                                  :src     '("src.el")
                                                  :docs    '("docs.texi"))))))
    (should (file-exists-p pkg-file))))



(provide 'kosz-build-test)

;; Local Variables:
;; read-symbol-shorthands: (("kbuild-" . "kosz-build-")
;;                          ("kutils-" . "kosz-utils-"))
;; End:

;;; kosz-build-test.el ends here.
