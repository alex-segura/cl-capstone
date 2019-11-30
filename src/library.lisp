;;;; library.lisp

(in-package #:capstone)

(cffi:define-foreign-library capstone
    (:unix (:or "libcapstone.so.3.0"
                "libcapstone.so.3"
                "libcapstone.so"))
  (t (:default "libcapstone")))

(defun load-capstone-library ()
  (cffi:use-foreign-library capstone))

(unless (cffi:foreign-library-loaded-p 'capstone)
  (load-capstone-library))
