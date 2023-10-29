#!/usr/bin/env -S emacs --script



(call-process "emacs" nil nil nil
              "--batch" "--quick"
              "--load" (expand-file-name "src/kosz-utils.el")
              "--load" (expand-file-name "src/kosz-manifest.el")
              "--load" (expand-file-name "src/kosz-build.el")
              "--eval"
              (format "%S" '(kosz-build-build
                             (kosz-manifest-read-manifest
                              (expand-file-name ".")))))

(print "Done")



;; Local Variables:
;; flymake-diagnostic-functions: nil
;; End:
