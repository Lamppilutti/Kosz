Kosz is multi-file package what means it includes several Emacs Lisp source code
files and info manual. This instruction for how to build Kosz for package.el
package manager or for `load-path' mechanism.


* Build by built.el
build.el is small script that load Kosz core and build Kosz by themself.

- Build tar package
  Eval build.el from Emacs:
      M-x load-file RET build.el RET
  Or eval as elisp script:
      $ emacs --batch --quick --script build.el
- Install build/kosz-*.tar archive by package.el:
    M-x package-install-file RET build/kosz-*.tar
  Or extrat tar archive and add path to extracted directory to `load-path'.


* Manual build

- Create 'kosz' directory.
- Copy all .el files from src/ recursively to 'kosz' directory.
- Use makeinfo program to compile .texi file to .info inside docs/info
  directory.
- Use install-info _file.info_ dir to created .info files for setup dir file.
- Copy all .info files and dir file to 'kosz' directory.
- Add path to 'kosz' directory to `load-path'.
