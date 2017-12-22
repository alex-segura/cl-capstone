;;;; mips.lisp

(in-package #:capstone)

(defmethod operand-count ((mips capstone-ffi:cs-mips))
  (cs-mips.op-count mips))

(defmethod operand-ref ((mips capstone-ffi:cs-mips) (i integer))
  (cs-mips.operands[] mips i))

(defmethod operand-type ((op capstone-ffi:cs-mips-op))
  (enum-key 'capstone-ffi:mips-op-type (cs-mips-op.type op)))

(defmethod operand-register ((op capstone-ffi:cs-mips-op))
  (check-operand-type op :reg)
  (enum-key 'capstone-ffi:mips-reg (cs-mips-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-mips-op))
  (check-operand-type op :imm)
  (cs-mips-op.imm op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-mips-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:mips-reg (cs-mips-op.mem.base op)))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-mips-op))
  (check-operand-type op :mem)
  (cs-mips-op.mem.disp op))
