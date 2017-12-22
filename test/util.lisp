(in-package #:capstone-test)

(defun make-bytes (&rest bytes)
  (make-array (list (length bytes))
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))
