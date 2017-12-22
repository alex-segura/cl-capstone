;;;; x86.lisp

(in-package #:capstone)

(define-arch-specific instruction-prefix ((x86 capstone-ffi:cs-x86))
  (cffi:foreign-array-to-lisp (cs-x86.prefix[]& x86) '(:array :uint8 4)))

(define-arch-specific instruction-opcode ((x86 capstone-ffi:cs-x86))
  (let ((op (cffi:foreign-array-to-lisp (cs-x86.opcode[]& x86) '(:array :uint8 4))))
    (+ (aref op 0)
       (ash (aref op 1) 8)
       (ash (aref op 2) 16)
       (ash (aref op 3) 24))))

(defmethod operand-count ((x86 capstone-ffi:cs-x86))
  (cs-x86.op-count x86))

(defmethod operand-ref ((x86 capstone-ffi:cs-x86) (i integer))
  (cs-x86.operands[] x86 i))

(define-arch-specific instruction-rex ((x86 capstone-ffi:cs-x86))
  (cs-x86.rex x86))

(define-arch-specific instruction-address-size ((x86 capstone-ffi:cs-x86))
  (cs-x86.addr-size x86))

(define-arch-specific instruction-modrm ((x86 capstone-ffi:cs-x86))
  (cs-x86.modrm x86))

(define-arch-specific instruction-sib ((x86 capstone-ffi:cs-x86))
  (cs-x86.sib x86))

(define-arch-specific instruction-displacement ((x86 capstone-ffi:cs-x86))
  (cs-x86.disp x86))

(define-arch-specific instruction-sib-index-register ((x86 capstone-ffi:cs-x86))
  (enum-key 'capstone-ffi:x86-reg (cs-x86.sib-index x86)))

(define-arch-specific instruction-sib-base-register ((x86 capstone-ffi:cs-x86))
  (enum-key 'capstone-ffi:x86-reg (cs-x86.sib-base x86)))

(define-arch-specific instruction-sib-scale ((x86 capstone-ffi:cs-x86))
  (cs-x86.sib-scale x86))

(define-arch-specific instruction-xop-condition-code ((x86 capstone-ffi:cs-x86))
  (enum-key 'capstone-ffi:x86-xop-cc (cs-x86.xop-cc x86)))

(define-arch-specific instruction-sse-condition-code ((x86 capstone-ffi:cs-x86))
  (enum-key 'capstone-ffi:x86-sse-cc (cs-x86.sse-cc x86)))

(define-arch-specific instruction-avx-condition-code ((x86 capstone-ffi:cs-x86))
  (enum-key 'capstone-ffi:x86-avx-cc (cs-x86.avx-cc x86)))

(define-arch-specific instruction-avx-sae-p ((x86 capstone-ffi:cs-x86))
  (not (zerop (cs-x86.avx-sae x86))))

(define-arch-specific instruction-avx-rounding-mode ((x86 capstone-ffi:cs-x86))
  (enum-key 'capstone-ffi:x86-avx-rm (cs-x86.avx-rm x86)))

(define-arch-specific instruction-eflags-updated ((x86 capstone-ffi:cs-x86))
  (mask-keywords 'x86-eflags (cs-x86.eflags x86)))

(define-arch-specific instruction-fpu-flags-updated ((x86 capstone-ffi:cs-x86))
  (mask-keywords 'x86-fpu-flags (cs-x86.fpu-flags x86)))

;; operands

(defmethod operand-type ((op capstone-ffi:cs-x86-op))
  (enum-key 'capstone-ffi:x86-op-type (cs-x86-op.type op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :imm)
  (cs-x86-op.imm op))

(defmethod memory-operand-segment-register ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:x86-reg (cs-x86-op.mem.segment op)))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:x86-reg (cs-x86-op.mem.base op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:x86-reg (cs-x86-op.mem.index op)))

(defmethod memory-operand-scale ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :mem)
  (cs-x86-op.mem.scale op))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :mem)
  (cs-x86-op.mem.disp op))

(defmethod operand-register ((op capstone-ffi:cs-x86-op))
  (check-operand-type op :reg)
  (enum-key 'capstone-ffi:x86-reg (cs-x86-op.reg op)))

(defmethod operand-access ((op capstone-ffi:cs-x86-op))
  (cs-x86-op.access op))

(defmethod operand-avx-broadcast ((op capstone-ffi:cs-x86-op))
  (cs-x86-op.avx-bcast op))

(defmethod operand-avx-zero-opmask-p ((op capstone-ffi:cs-x86-op))
  (not (zerop (cs-x86-op.avx-zero-opmask op))))

(defmethod operand-size ((op capstone-ffi:cs-x86-op))
  (cs-x86-op.size op))

;; encoding

(define-arch-specific instruction-encoding ((instruction capstone-ffi:cs-x86))
  (cs-x86.encoding instruction))

(defmethod encoding-modrm-offset ((encoding capstone-ffi:cs-x86-encoding))
  (cs-x86-encoding.modrm-offset encoding))

(defmethod encoding-displacement-offset ((encoding capstone-ffi:cs-x86-encoding))
  (cs-x86-encoding.disp-offset encoding))

(defmethod encoding-displacement-size ((encoding capstone-ffi:cs-x86-encoding))
  (cs-x86-encoding.disp-size encoding))

(defmethod encoding-immediate-offset ((encoding capstone-ffi:cs-x86-encoding))
  (cs-x86-encoding.imm-offset encoding))

(defmethod encoding-immediate-size ((encoding capstone-ffi:cs-x86-encoding))
  (cs-x86-encoding.imm-size encoding))
