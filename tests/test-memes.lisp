(in-package #:cl-user)
(uiop:define-package :test-memes
  (:use #:cl
    #:scraps/src/memes
    #:mockingbird
    #:prove))

(in-package #:test-memes)

(plan nil)

(subtest "Testing Memes"
  (is (scraps/src/memes::map-pixel-to-char 12) #\@ "check character values")
  (is (scraps/src/memes::map-pixel-to-char 255) #\ "check character values"))


(finalize)
