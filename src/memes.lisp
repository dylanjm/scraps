(in-package #:cl-user)
(uiop:define-package #:scraps/src/memes
  (:use #:cl)
  (:import-from #:scraps/src/utils
    #:when-option
    #:unknown-option
    #:missing-arg
    #:arg-parser-failed
    #:parse-size
    #:parse-color)
  (:export #:clash))

(in-package #:scraps/src/memes)

(defparameter *ascii-chars* (coerce "@%#*+=-:., " 'list))
(defparameter *fixed-width* 80 "Max width of ascii output.")
(defparameter *fixed-height* 80 "Max height of ascii output.")
(defparameter *output-color* nil "Ansi color to output.")

(defun get-image-data (filename)
  (let ((img (opticl:read-image-file filename)))
    (opticl:coerce-image img 'opticl:8-bit-gray-image)))

(defun resize-image-fixed (image max-width max-height)
  (opticl:with-image-bounds (height width) image
    (let* ( (ratio (min (/ max-width width) (/ max-height height)))
            (neww (* width ratio))
            (newh (* height ratio)))
      (opticl:resize-image image neww newh))))

(defun paste0 (stream vals)
  (format stream "~{~a~} ~%" vals))

(defun map-pixel-to-char (val)
  (nth (floor val 25) *ascii-chars*))

(defun convert-image-to-ascii (a)
  (destructuring-bind (n m) (array-dimensions a)
    (loop for i from 0 below n collect
      (mapcar #'map-pixel-to-char
        (loop for j from 0 below m collect (aref a i j))))))

(defun display-ascii-image (a)
  (dolist (val a) (paste0 t val)) (values))

(defun clash (&rest args)
  (declare (ignore args))
  (opts:define-opts
    ( :name         :help
      :description  "show the help message"
      :short        #\h
      :long         "help")
    ( :name         :size
      :description  "size of ascii image specified as 'WxH'"
      :short        #\s
      :long         "size"
      :meta-var     "SIZE"
      :arg-parser   #'parse-size
      :default      (lambda () '(80 . 80)))
    ( :name         :inverse
      :description  "highlight the background"
      :short        #\i
      :long         "inverse")
    ( :name         :color
      :description  "Color the output using ANSI escape code"
      :short        #\c
      :long         "color"
      :meta-var     "COLOR"
      :default      :white
      :arg-parser   #'parse-color))

  (multiple-value-bind (options free-args)
    (handler-bind ( (opts:unknown-option #'unknown-option)
                    (opts:missing-arg #'missing-arg)
                    (opts:arg-parser-failed #'arg-parser-failed))
      (opts:get-opts))

    (when-option (options :help)
      (opts:describe
        :prefix    (format nil "Scraps:Clash version ~a" "0.0.1")
        :usage-of  "clash"
        :args      "[IMAGE]")
      (opts:exit 0))

    (when-option (options :size)
      (setf *fixed-width* (car (getf options :size)))
      (setf *fixed-height* (cdr (getf options :size))))

    (when-option (options :inverse)
      (setf *ascii-chars* (reverse *ascii-chars*)))

    (when-option (options :color)
      (setf *output-color* (getf options :color)))

    (let* ( (img (get-image-data (second free-args)))
            (scaled-img (resize-image-fixed img *fixed-width* *fixed-height*))
            (ascii-img (convert-image-to-ascii scaled-img)))
      (if *output-color*
        (cl-ansi-text:with-color (*output-color*)
          (display-ascii-image ascii-img))
        (display-ascii-image ascii-img)))

  (cl-scripting:success)))

(cl-scripting:register-commands :scraps/src/memes)
