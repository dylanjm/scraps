#-asdf3.1 (error "ASDF 3.1 or bust!")

(in-package :cl-user)
(defpackage scraps-asd
  (:use :cl :asdf))
(in-package :scraps-asd)

(defsystem :scraps
  :name "scraps"
  :version "0.0.1"
  :author "Dylan McDowell"
  :license "MIT"
  :description "A place to store common lisp scripts"
  :long-description "SCRAPS: A system containing useful common lisp programs to replace shell programs."
  :class :package-inferred-system
  :depends-on ( #:cl-scripting
                #:cl-launch/dispatch
                #:cl-ansi-text
                #:inferior-shell
                #:unix-opts
                #:opticl
                #:opticl-core
                "scraps/src/utils"
                "scraps/src/memes"
                "scraps/src/main"
                "scraps/src/driver"))

;; #+sb-core-compression
;; (defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
;;   (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem :scraps/test
  :name "scraps-test"
  :author "Dylan McDowell"
  :license "MIT"
  :depends-on ( #:scraps
                #:mockingbird
                #:prove)
  :components ((:module "tests"
                 :components
                 ((:test-file "test-memes"))))
  :description "Test system for scraps."
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
             (funcall (intern #.(string :run-test-system) :prove-asdf) c)
             (asdf:clear-system c)))
