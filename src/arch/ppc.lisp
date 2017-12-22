;;;; ppc.lisp

(in-package #:capstone)

(define-arch-specific instruction-branch-code ((ppc capstone-ffi:cs-ppc))
  (enum-key 'capstone-ffi:ppc-bc (cs-ppc.bc ppc)))

(define-arch-specific instruction-branch-hint ((ppc capstone-ffi:cs-ppc))
  (enum-key 'capstone-ffi:ppc-bh (cs-ppc.bh ppc)))

(define-arch-specific instruction-updates-cr0-p ((ppc capstone-ffi:cs-ppc))
  (not (zerop (cs-ppc.update-cr0 ppc))))

(defmethod operand-count ((ppc capstone-ffi:cs-ppc))
  (cs-ppc.op-count ppc))

(defmethod operand-ref ((ppc capstone-ffi:cs-ppc) (i integer))
  (cs-ppc.operands[] ppc i))

(defmethod operand-type ((op capstone-ffi:cs-ppc-op))
  (enum-key 'capstone-ffi:ppc-op-type (cs-ppc-op.type op)))

(defmethod operand-register ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :reg)
  (enum-key 'capstone-ffi:ppc-reg (cs-ppc-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :imm)
  (cs-ppc-op.imm op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:ppc-reg (cs-ppc-op.mem.base op)))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :mem)
  (cs-ppc-op.mem.disp op))

(defmethod condition-register-operand-scale ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :crx)
  (cs-ppc-op.crx.scale op))

(defmethod condition-register-operand-register ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :crx)
  (enum-key 'capstone-ffi:ppc-reg (cs-ppc-op.crx.reg op)))

(defmethod condition-register-operand-condition ((op capstone-ffi:cs-ppc-op))
  (check-operand-type op :crx)
  (enum-key 'capstone-ffi:ppc-bc (cs-ppc-op.crx.cond op)))
