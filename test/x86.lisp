(in-package #:capstone-test)

(defparameter *x86-code-64*
  (make-bytes #x55 #x48 #x8b #x05 #xb8 #x13
              #x00 #x00 #xe9 #xea #xbe #xad
              #xde #xff #x25 #x23 #x01 #x00
              #x00 #xe8 #xdf #xbe #xad #xde
              #x74 #xff))

(defparameter *x86-code-32*
  (make-bytes #x8d #x4c #x32 #x08 #x01 #xd8
              #x81 #xc6 #x34 #x12 #x00 #x00
              #x05 #x23 #x01 #x00 #x00 #x36
              #x8b #x84 #x91 #x23 #x01 #x00
              #x00 #x41 #x8d #x84 #x39 #x89
              #x67 #x00 #x00 #x8d #x87 #x89
              #x67 #x00 #x00 #xb4 #xc6 #xe9
              #xea #xbe #xad #xde #xff #xa0
              #x23 #x01 #x00 #x00 #xe8 #xdf
              #xbe #xad #xde #x74 #xff))

(defparameter *x86-code-16*
  (make-bytes #x8d #x4c #x32 #x08 #x01 #xd8
              #x81 #xc6 #x34 #x12 #x00 #x00
              #x05 #x23 #x01 #x00 #x00 #x36
              #x8b #x84 #x91 #x23 #x01 #x00
              #x00 #x41 #x8d #x84 #x39 #x89
              #x67 #x00 #x00 #x8d #x87 #x89
              #x67 #x00 #x00 #xb4 #xc6 #x66
              #xe9 #xb8 #x00 #x00 #x00 #x67
              #xff #xa0 #x23 #x01 #x00 #x00
              #x66 #xe8 #xcb #x00 #x00 #x00
              #x74 #xfc))

(defun print-x86-detail (mode insn)
  (format t "0x~x: ~a ~a~%"
          (instruction-address insn)
          (instruction-mnemonic insn)
          (instruction-operand-string insn))

  (when (zerop (instruction-id insn))
    (return-from print-x86-detail nil))

  (format t "~4,0Tprefix: ~x~%" (instruction-prefix insn))
  (format t "~4,0Topcode: ~x~%" (instruction-opcode insn))
  (format t "~4,0Ttrex: 0x~x~%" (instruction-rex insn))
  (format t "~4,0Taddress-size: ~a~%" (instruction-address-size insn))
  (format t "~4,0Tmodrm: ~a~%" (instruction-modrm insn))

  (let ((enc (instruction-encoding insn)))
    (unless (zerop (encoding-modrm-offset enc))
      (format t "~4,0Tmodrm-offset: 0x~x~%" (encoding-modrm-offset enc)))
    (unless (zerop (encoding-displacement-offset enc))
      (format t "~4,0Tdisplacement-offset: 0x~x~%" (encoding-displacement-offset enc)))
    (unless (zerop (encoding-displacement-size enc))
      (format t "~4,0Tdisplacement-size: 0x~x~%" (encoding-displacement-size enc))))

  (unless (eql mode :16)
    (format t "~4,0Tsib: 0x~x~%" (instruction-sib insn))
    (unless (zerop (instruction-sib insn))
      (when (instruction-sib-base-register insn)
        (format t "~4,0Tsib-base: ~a~%" (instruction-sib-base-register insn)))
      (when (instruction-sib-index-register insn)
        (format t "~4,0Tsib-index: ~a~%" (instruction-sib-index-register insn)))
      (unless (zerop (instruction-sib-scale insn))
        (format t "~4,0Tsib-scale: ~a~%" (instruction-sib-scale insn)))))

  (unless (eql (instruction-xop-condition-code insn) :invalid)
    (format t "~4,0Txop-cc: ~a~%" (instruction-xop-condition-code insn)))
  (unless (eql (instruction-sse-condition-code insn) :invalid)
    (format t "~4,0Tsse-cc: ~a~%" (instruction-sse-condition-code insn)))
  (unless (eql (instruction-avx-condition-code insn) :invalid)
    (format t "~4,0Tavx-cc: ~a~%" (instruction-avx-condition-code insn)))
  (when (instruction-avx-sae-p insn)
    (format t "~4,0Tavx-sae: t~%"))
  (unless (eql (instruction-avx-rounding-mode insn) :invalid)
    (format t "~4,0Tavx-rm: ~a~%" (instruction-avx-rounding-mode insn)))

  (format t "~4,0Toperand-count: ~a~%" (operand-count insn))
  (do-instruction-operands (op i insn)
    (case (operand-type op)
      (:reg
       (format t "~8,0Toperands[~a].type: REG = ~a~%" i (operand-register op)))
      (:imm
       (format t "~8,0Toperands[~a].type: IMM = 0x~x~%" i (operand-immediate-value op)))
      (:mem
       (format t "~8,0Toperands[~a].type: MEM~%" i)
       (unless (eql (memory-operand-segment-register op) :invalid)
         (format t "~12,0Toperands[~a].mem.segment: REG = ~a~%"
                 i (memory-operand-segment-register op)))
       (unless (eql (memory-operand-base-register op) :invalid)
         (format t "~12,0Toperands[~a].mem.base: REG = ~a~%"
                 i (memory-operand-base-register op)))
       (unless (eql (memory-operand-index-register op) :invalid)
         (format t "~12,0Toperands[~a].mem.index: REG = ~a~%"
                 i (memory-operand-index-register op)))
       (unless (= 1 (memory-operand-scale op))
         (format t "~12,0Toperands[~a].mem.scale: ~a~%"
                 i (memory-operand-scale op)))
       (unless (zerop (memory-operand-displacement op))
         (format t "~12,0Toperands[~a].mem.disp: ~a~%"
                 i (memory-operand-displacement op)))))

    (unless (eql (operand-avx-broadcast op) :invalid)
      (format t "~8,0Toperands[~a].avx_bcast ~a~%" i (operand-avx-broadcast op)))
    (when (operand-avx-zero-opmask-p op)
      (format t "~8,0Toperands[~a].avx_zero_opmask: TRUE~%" i))

    (format t "~8,0Toperands[~a].size: ~a~%" i (operand-size op))

    (cond ((subsetp (list :read :write) (operand-access op))
           (format t "~8,0Toperands[~a].access: READ|WRITE~%" i))
          ((member :write (operand-access op))
           (format t "~8,0Toperands[~a].access: WRITE~%" i))
          ((member :read (operand-access op))
           (format t "~8,0Toperands[~a].access: READ~%" i))))

  (dolist (reg (instruction-registers-read insn)))
  (dolist (reg (instruction-registers-written insn)))

  (when (instruction-eflags-updated insn)))

(def-suite x86
  :description "Tests for X86"
  :in capstone)

(in-suite x86)

(test test-x86-64
  (is (null
       (with-open-handle (csh :x86 :64)
         (set-option csh :detail :on)
         (do-disassembled-instructions (insn csh *x86-code-64*)
           (print-x86-detail :64 insn))))))


(test test-x86-32
  (is (null
       (with-open-handle (csh :x86 :32)
         (set-option csh :detail :on)
         (do-disassembled-instructions (insn csh *x86-code-32*)
           (print-x86-detail :32 insn))))))


(test test-x86-16
  (is (null
       (with-open-handle (csh :x86 :16)
         (set-option csh :detail :on)
         (do-disassembled-instructions (insn csh *x86-code-16*)
           (print-x86-detail :16 insn))))))
