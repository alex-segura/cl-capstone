;;;; ppc.lisp

(in-package #:capstone-test)

(defparameter *ppc-code*
  (make-bytes #x43 #x20 #x0c #x07 #x41
              #x56 #xff #x17 #x80 #x20
              #x00 #x00 #x80 #x3f #x00
              #x00 #x10 #x43 #x23 #x0e
              #xd0 #x44 #x00 #x80 #x4c
              #x43 #x22 #x02 #x2d #x03
              #x00 #x80 #x7c #x43 #x20
              #x14 #x7c #x43 #x20 #x93
              #x4f #x20 #x00 #x21 #x4c
              #xc8 #x00 #x21 #x40 #x82
              #x00 #x14))

(defun print-ppc-detail (insn)
  (format t "0x~x: ~a ~a~%"
          (instruction-address insn)
          (instruction-mnemonic insn)
          (instruction-operand-string insn)))

(in-suite ppc)

(test test-ppc
  (is (null
       (with-open-handle (csh :ppc :big-endian)
         (do-disassembled-instructions (insn csh *ppc-code*)
           (print-ppc-detail insn))))))
