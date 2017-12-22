;;;; xcore.lisp

(in-package #:capstone)

(defmethod operand-count ((xcore capstone-ffi:cs-xcore))
  (cs-xcore.op-count xcore))

(defmethod operand-ref ((xcore capstone-ffi:cs-xcore) (i integer))
  (cs-xcore.operands[] xcore i))

(defmethod operand-immediate-value ((op capstone-ffi:cs-xcore-op))
  (cs-xcore-op.imm op))

(defmethod operand-register ((op capstone-ffi:cs-xcore-op))
  (enum-key 'capstone-ffi:xcore-reg (cs-xcore-op.reg op)))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-xcore-op))
  (enum-key 'capstone-ffi:xcore-reg (cs-xcore-op.mem.base op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-xcore-op))
  (enum-key 'capstone-ffi:xcore-reg (cs-xcore-op.mem.index op)))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-xcore-op))
  (cs-xcore-op.mem.disp op))

(defmethod memory-operand-direction ((op capstone-ffi:cs-xcore-op))
  (let ((direction (cs-xcore-op.mem.direct op)))
    (cond ((= direction 1) :forward)
          ((= direction -1) :backward)
          (t (error 'capstone-error :string "Unknown XCORE operand direction")))))
