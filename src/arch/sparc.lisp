;;;; sparc.lisp

(in-package #:capstone)

(define-arch-specific instruction-condition-code ((sparc capstone-ffi:cs-sparc))
  (enum-key 'capstone-ffi:sparc-op-type (cs-sparc.cc sparc)))

(define-arch-specific instruction-hint ((sparc capstone-ffi:cs-sparc))
  (enum-key 'capstone-ffi:sparc-hint (cs-sparc.hint sparc)))

(defmethod operand-count ((sparc capstone-ffi:cs-sparc))
  (cs-sparc.op-count sparc))

(defmethod operand-ref ((sparc capstone-ffi:cs-sparc) (i integer))
  (cs-sparc.operands[] sparc i))

(defmethod operand-type ((op capstone-ffi:cs-sparc-op))
  (enum-key 'capstone-ffi:cs-sparc-op (cs-sparc-op.type op)))

(defmethod operand-register ((op capstone-ffi:cs-sparc-op))
  (enum-key 'capstone-ffI:sparc-reg (cs-sparc-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-sparc-op))
  (cs-sparc-op.imm op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-sparc-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:cs-sparc-op (cs-sparc-op.mem.base op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-sparc-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:cs-sparc-op (cs-sparc-op.mem.base op)))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-sparc-op))
  (check-operand-type op :mem)
  (cs-sparc-op.mem.disp op))
