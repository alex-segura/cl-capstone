;;;; systemz.lisp

(in-package #:capstone)

(define-arch-specific instruction-condition-code ((sysz capstone-ffi:cs-sysz))
  (enum-key 'capstone-ffi:sysz-cc (cs-sysz.cc sysz)))

(defmethod operand-count ((sysz capstone-ffi:cs-sysz))
  (cs-sysz.op-count sysz))

(defmethod operand-ref ((sysz capstone-ffi:cs-sysz) (i integer))
  (cs-sysz.operands[] sysz i))

(defmethod operand-type ((op capstone-ffi:cs-sysz-op))
  (enum-key 'capstone-ffi:sysz-op-type (cs-sysz-op.type op)))

(defmethod operand-register ((op capstone-ffi:cs-sysz-op))
  (enum-key 'capstone-ffi:sysz-reg (cs-sysz-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-sysz-op))
  (cs-sysz-op.imm op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-sysz-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:sysz-reg (cs-sysz-op.mem.base op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-sysz-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:sysz-reg (cs-sysz-op.mem.index op)))

(defmethod memory-operand-length ((op capstone-ffi:cs-sysz-op))
  (check-operand-type op :mem)
  (cs-sysz-op.mem.length op))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-sysz-op))
  (check-operand-type op :mem)
  (cs-sysz-op.mem.disp op))
