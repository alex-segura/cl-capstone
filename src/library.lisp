;;;; library.lisp

(in-package #:capstone)

(cffi:define-foreign-library capstone
  (:unix "libcapstone.so.3.0")
  (t (:default "libcapstone")))

(defun load-capstone-library ()
  (cffi:use-foreign-library capstone))

(unless (cffi:foreign-library-loaded-p 'capstone)
  (load-capstone-library))
