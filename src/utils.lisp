(in-package #:cl-user)
(uiop:define-package #:scraps/src/utils
  (:use #:cl)
  (:import-from #:cl-launch/dispatch
    #:all-entry-names
    #:get-name)
  (:export #:create-symlinks #:help))

(in-package #:scraps/src/utils)

(defun create-symlinks (src)
  (let* ( (bindir (or (uiop:getenv "DEST") (uiop:getenv "XDG_BIN_HOME")))
          (destination (uiop:truenamize bindir)))
    (uiop:with-current-directory (destination)
      (dolist (i (cl-launch/dispatch:all-entry-names))
        (unless (uiop:file-exists-p i)
          (format t "linking file ~A~%" i)
          (inferior-shell:run `("ln" "-s" ,src ,i))))))
  (cl-scripting:success))

(defun help ()
  (uiop:format! t "~A commands: ~{~A~^ ~}~%" (get-name) (all-entry-names))
  (cl-scripting:success))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       (progn
         ,@body))))

;; Describe handlers for bad command line arguments.
(defun unknown-option (condition)
  (format t "~s option is unknown.~%" (opts:option condition))
  (opts:describe)
  (opts:exit))

(defun missing-arg (condition)
  (format t "Bad options: ~a needs an argument.~&" (opts:option condition))
  (opts:describe)
  (opts:exit))

(defun arg-parser-failed (condition)
  (format t "Error: could not parse ~a as argument of ~a~&."
          (opts:raw-arg condition)
          (opts:option condition))
  (opts:describe)
  (opts:exit))

(defun parse-size (input)
  (multiple-value-bind (num idx)
    (parse-integer input :junk-allowed t)
    (let* ( (width num)
            (height (parse-integer (subseq input idx) :start 1 :junk-allowed t)))
      (values (cons width height)))))

(defun parse-color (input)
  (let ((upcase-input (string-upcase input)))
    (cond
      ((member upcase-input '( "BLACK" "RED"
                               "GREEN" "YELLOW"
                               "BLUE"  "MAGENTA"
                               "CYAN"  "WHITE" ) :test #'equal)
        (intern upcase-input :keyword))
      (t nil))))

(cl-scripting:register-commands :scraps/src/utils)
