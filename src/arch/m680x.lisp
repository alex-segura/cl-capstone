;;;; m680x.lisp

(in-package #:capstone)

(define-arch-specific instruction-flags ((m680x capstone-ffi:cs-m680x))
  (cs-m680x.flags m680x))

(defmethod operand-count ((m680x capstone-ffi:cs-m680x))
  (cs-m680x.op-count m680x))

(defmethod operand-ref ((m680x capstone-ffi:cs-m680x) (i integer))
  (cs-m680x.operands[] m680x i))

(defmethod operand-type ((op capstone-ffi:cs-m680x-op))
  (enum-key 'capstone-ffi:m680x-op-type (cs-m680x-op.type op)))

(defmethod operand-size ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.size op))

(defmethod operand-access ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.access op))

(defmethod operand-immediate-value ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :immediate)
  (cs-m680x-op.imm op))

(defmethod operand-register ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :register)
  (enum-key 'capstone-ffi:m680x-reg (cs-m680x-op.reg op)))

(defmethod extended-operand-address ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :extended)
  (cs-m680x-op.ext.address op))

(defmethod direct-operand-address ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :direct)
  (cs-m680x-op.direct-addr op))

(defmethod extended-operand-indirect-address-p ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :extended)
  (not (zerop (cs-m680x-op.ext.indirect op))))

(defmethod relative-operand-address ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :relative)
  (cs-m680x-op.rel.address op))

(defmethod relative-operand-address-offset ((op capstone-ffi:cs-m680x-op))
  (check-operand-type op :relative)
  (cs-m680x-op.rel.offset op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-m680x-op))
  (enum-key 'capstone-ffi:m680x-reg (cs-m680x-op.idx.base-reg op)))

(defmethod memory-operand-offset-register ((op capstone-ffi:cs-m680x-op))
  (enum-key 'capstone-ffi:m680x-reg (cs-m680x-op.idx.offset-reg op)))

(defmethod memory-operand-offset ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.idx.offset op))

(defmethod memory-operand-offset-address ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.idx.offset-addr op))

(defmethod memory-operand-offset-bits ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.idx.offset-bits op))

(defmethod memory-operand-increment/decrement ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.idx.inc-dec op))

(defmethod memory-operand-flags ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.idx.flags op))

(defmethod constant-operand-value ((op capstone-ffi:cs-m680x-op))
  (cs-m680x-op.const-val op))
