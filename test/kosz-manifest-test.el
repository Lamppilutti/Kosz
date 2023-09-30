;;; kosz-manifest-test.el --- -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Lämppi Lütti <lamppilutti@gmail.com>

;; Copying and distribution of this file, with or without modification, are
;; permitted in any medium without royalty, provided the copyright notice and
;; this notice are preserved. This file is offered as-is, without any warranty.

;;; Commentary

;;; Code:



(require 'ert)
(require 'seq)
(require 'subr-x)

(require 'kosz-manifest)
(require 'kosz-utils)



(defun bad-property-p (property err)
  "Return t if ERR's data has invalid PROPERTY.

Err should be `kosz-manifest-manifest-validation-error'"
  (declare (indent 1))
  (seq-find
   (lambda (property-info)
     (equal property (plist-get property-info :property)))
   (plist-get (cdr err) :invalid-properties)))



(defun make-manifest (&rest optional-properties)
  "Return manifest with predefined valid :name and :version properties.

OPTIONAL-PROPERTIES will added to manifest."
  (cons nil
        (append '(:name name :version "1.1.1") optional-properties)))



(ert-deftest kmanifest-read-manifest-test-ok ()
  "Test manifest reading without error."
  (with-temp-file "package.kosz" (insert "(define-package 'test \"1.1.1\")"))
  (should (kmanifest-read-manifest ".")))

(ert-deftest kmanifest-read-manifest-test-error ()
  "Test manifest reading signal error if there is no manifest in directory."
  (should-error (kmanifest-read-manifest "..")
                :type 'kosz-utils-external-process-error))



(ert-deftest kmanifest-validate-manifest:name:version-test-ok ()
  "Test valid `:name' and `:version' is validated without error."
  (should (kmanifest-validate-manifest (make-manifest))))

(ert-deftest kmanifest-validate-manifest:description-test-ok ()
  "Test valid `:description' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :description "description"))))

(ert-deftest kmanifest-validate-manifest:dependencies-test-ok ()
  "Test valid `:dependencies' is validated withour error."
  (should (kmanifest-validate-manifest
           (make-manifest :dependencies '((name "1.1.1"))))))

(ert-deftest kmanifest-validate-manifest:url-test-ok ()
  "Test valid `:url' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :url "url"))))

(ert-deftest kmanifest-validate-manifest:authors-test-ok ()
  "Test valid `:authors' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :authors '(("name" "email"))))))

(ert-deftest kmanifest-validate-manifest:license-test-ok ()
  "Test valid `:license' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :license "license"))))

(ert-deftest kmanifest-validate-manifest:commit-test-ok ()
  "Test valid `:commit' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :commit "commit"))))

(ert-deftest kmanifest-validate-manifest:keywords-test-ok ()
  "Test valid `:keywords' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :keywords '("keyword")))))

(ert-deftest kmanifest-validate-manifest:maintainer-test-ok ()
  "Test valid `:maintainer' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :maintainer '("name" "email")))))

(ert-deftest kmanifest-validate-manifest:readme-test-ok ()
  "Test valid `:readme' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :readme "path"))))

(ert-deftest kmanifest-validate-manifest:src-test-ok ()
  "Test valid `:src' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :src '("path")))))

(ert-deftest kmanifest-validate-manifest:src-exclude-test-ok ()
  "Test valid `:src-exclude' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :src-exclude '("path")))))

(ert-deftest kmanifest-validate-manifest:docs-test-ok ()
  "Test valid `:docs' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :docs '("path")))))

(ert-deftest kmanifest-validate-manifest:docs-exclude-test-ok ()
  "Test valid `:docs-exclude' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :docs-exclude '("path")))))

(ert-deftest kmanifest-validate-manifest:assets-test-ok ()
  "Test valid `:assets' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :assets '("path")))))

(ert-deftest kmanifest-validate-manifest:assets-exclude-test-ok ()
  "Test valid `:assets-exclude' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :assets-exclude '("path")))))

(ert-deftest kmanifest-validate-manifest:tests-test-ok ()
  "Test valid `:tests' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :tests '("path")))))

(ert-deftest kmanifest-validate-manifest:tests-exclude-test-ok ()
  "Test valid `:tests-exclude' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :tests-exclude '("path")))))

(ert-deftest kmanifest-validate-manifest:test-runner-test-ok ()
  "Test valid `:test-runner' is validated without error."
  (should (kmanifest-validate-manifest
           (make-manifest :test-runner 'test-runner))))

(ert-deftest kmanifest-validate-manifest:name-test-error ()
  "Test invalid `:name' is validated with error."
  (should
   (bad-property-p :name
     (should-error (kmanifest-validate-manifest
                    (cons nil '(:version "1.1.1")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :name
     (should-error (kmanifest-validate-manifest
                    (cons nil '(:name :keyword :version "1.1.1")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:version-test-error ()
  "Test invalid `:version' is validated with error."
  (should
   (bad-property-p :version
     (should-error (kmanifest-validate-manifest
                    (cons nil '(:name name)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :version
     (should-error (kmanifest-validate-manifest
                    (cons nil '(:name name :version :not-a-string)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :version
     (should-error (kmanifest-validate-manifest
                    (cons nil '(:name name :version "some-string")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:description-test-error ()
  "Test invalid `:description' is validated with error."
  (should
   (bad-property-p :description
     (should-error (kmanifest-validate-manifest
                    (make-manifest :description :not-a-string))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:dependencies-test-error ()
  "Test invalid `:dependencies' is validated with error."
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies :not-a-pair))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '(:list :of :not :pairs)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '((name "1.1.1") :other)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '((name "1.1.1") (:not-pair))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '((:name "1.1.1"))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '((name "version"))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '((:name "version"))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :dependencies
     (should-error (kmanifest-validate-manifest
                    (make-manifest :dependencies '((name "1.1.1" :other))))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:url-test-error ()
  "Test invalid `:url' is validated with error."
  (should
   (bad-property-p :url
     (should-error (kmanifest-validate-manifest
                    (make-manifest :url :not-a-string))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:authors-test-error ()
  "Test invalid `:authors' is validated with error."
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '(:list :of :not :pairs)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '(("name" "email") :other)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '(("name" "email") (:not-pair))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '((:name "email"))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '(("name" :email))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '((:name :email))))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :authors
     (should-error (kmanifest-validate-manifest
                    (make-manifest :authors '(("name" "email" :other))))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:license-test-error ()
  "Test invalid `:license' is validated with error."
  (should
   (bad-property-p :license
     (should-error (kmanifest-validate-manifest
                    (make-manifest :license :not-a-string))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:commit-test-error ()
  "Test invalid `:commit' is validated with error."
  (should
   (bad-property-p :commit
     (should-error (kmanifest-validate-manifest
                    (make-manifest :commit :not-a-string))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:keywords-test-error ()
  "Test invalid `:keywords' is validated with error."
  (should
   (bad-property-p :keywords
     (should-error (kmanifest-validate-manifest
                    (make-manifest :keywords :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :keywords
     (should-error (kmanifest-validate-manifest
                    (make-manifest :keywords '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :keywords
     (should-error (kmanifest-validate-manifest
                    (make-manifest :keywords '("string" :other)))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:maintainer-test-error ()
  "Test invalid `:maintainer' is validated with error."
  (should
   (bad-property-p :maintainer
     (should-error (kmanifest-validate-manifest
                    (make-manifest :maintainer :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :maintainer
     (should-error (kmanifest-validate-manifest
                    (make-manifest :maintainer '(:not-pair)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :maintainer
     (should-error (kmanifest-validate-manifest
                    (make-manifest :maintainer '(:name "email")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :maintainer
     (should-error (kmanifest-validate-manifest
                    (make-manifest :maintainer '("name" :email)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :maintainer
     (should-error (kmanifest-validate-manifest
                    (make-manifest :maintainer '(:name :email)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :maintainer
     (should-error (kmanifest-validate-manifest
                    (make-manifest :maintainer '("name" "email" :other)))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:readme-test-error ()
  "Test invalid `:readme' is validated with error."
  (should
   (bad-property-p :readme
     (should-error (kmanifest-validate-manifest
                    (make-manifest :readme ""))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :readme
     (should-error (kmanifest-validate-manifest
                    (make-manifest :readme "."))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :readme
     (should-error (kmanifest-validate-manifest
                    (make-manifest :readme ".."))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :readme
     (should-error (kmanifest-validate-manifest
                    (make-manifest :readme "/"))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:src-test-error ()
  "Test invalid `:src' is validated with error."
  (should
   (bad-property-p :src
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:src-exclude-test-error ()
  "Test invalid `:src-exclude' is validated with error."
  (should
   (bad-property-p :src-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src-exclude :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src-exclude '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src-exclude '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src-exclude '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src-exclude '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :src-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :src-exclude '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:docs-test-error ()
  "Test invalid `:docs' is validated with error."
  (should
   (bad-property-p :docs
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:docs-exclude-test-error ()
  "Test invalid `:docs-exclude' is validated with error."
  (should
   (bad-property-p :docs-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs-exclude :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs-exclude '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs-exclude '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs-exclude '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs-exclude '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :docs-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :docs-exclude '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:assets-test-error ()
  "Test invalid `:assets' is validated with error."
  (should
   (bad-property-p :assets
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:assets-exclude-test-error ()
  "Test invalid `:assets-exclude' is validated with error."
  (should
   (bad-property-p :assets-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets-exclude :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets-exclude '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets-exclude '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets-exclude '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets-exclude '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :assets-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :assets-exclude '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:tests-test-error ()
  "Test invalid `:tests' is validated with error."
  (should
   (bad-property-p :tests
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:tests-exclude-test-error ()
  "Test invalid `:tests-exclude' is validated with error."
  (should
   (bad-property-p :tests-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests-exclude :not-a-list))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests-exclude '(:list :of :not :strings)))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests-exclude '("")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests-exclude '(".")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests-exclude '("..")))
                   :type 'kmanifest-manifest-validation-error)))
  (should
   (bad-property-p :tests-exclude
     (should-error (kmanifest-validate-manifest
                    (make-manifest :tests-exclude '("/")))
                   :type 'kmanifest-manifest-validation-error))))

(ert-deftest kmanifest-validate-manifest:test-runner-test-error ()
  "Test invalid `:tests-exclude' is validated with error."
  (should
   (bad-property-p :test-runner
     (should-error (kmanifest-validate-manifest
                    (make-manifest :test-runner :keyword))
                   :type 'kmanifest-manifest-validation-error))))

(provide 'kosz-manifest-test)

;; Local Variables:
;; read-symbol-shorthands: (("kmanifest-" . "kosz-manifest-")
;;                          ("kutils-"    . "kosz-utils-"))
;; End:

;;; kosz-manifest-test.el ends here.
