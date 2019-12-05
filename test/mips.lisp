(in-package #:capstone-test)

(defparameter *mips-code*
  (make-bytes #x0C #x10 #x00 #x97
              #x00 #x00 #x00 #x00
              #x24 #x02 #x00 #x0c
              #x8f #xa2 #x00 #x00
              #x34 #x21 #x34 #x56))

(defparameter *mips-code-2*
  (make-bytes  #x56 #x34 #x21 #x34 #xc2 #x17 #x01 #x00))

(defun print-mips-instruction (insn))

(in-suite mips)

(test test-mips64-be
  (is (null
       (capstone:with-open-handle (csh :mips :mips64 :big-endian)
         (capstone:set-option csh :detail :on)
         (capstone:do-disassembled-instructions (insn csh *mips-code*)
           (format t "0x~x: ~a ~a~%"
                   (instruction-address insn)
                   (instruction-mnemonic insn)
                   (instruction-operand-string insn)))))))
