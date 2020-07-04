(uiop:define-package #:scraps/src/driver
  (:nicknames :scraps :scrap)
  (:use :uiop/common-lisp)
  (:use-reexport #:scraps/src/utils
                 #:scraps/src/memes
                 #:scraps/src/main))

(provide "scraps")
(provide "SCRAPS")
