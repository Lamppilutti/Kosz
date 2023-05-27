;;; bootstrap.el --- bootstrap Kosz. -*- lexical-binding: t; -*-

;;; Commentary:
;; Build Kosz package by Kosz without Kosz.

;;; Code:



(call-process "emacs" nil nil nil
              "--batch" "--quick"
              "--load" (expand-file-name "src/kosz-utils.el")
              "--load" (expand-file-name "src/kosz-manifest.el")
              "--load" (expand-file-name "src/kosz-build.el")
              "--eval"
              (format "%S" '(kosz-build-build
                             (kosz-manifest-read-manifest
                              (expand-file-name ".")))))



;;; bootstrap.el ends here
