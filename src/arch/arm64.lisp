;;;; arm64.lisp

(in-package #:capstone)

(define-arch-specific instruction-condition-code ((arm64 capstone-ffi:cs-arm64))
  (enum-key 'capstone-ffi:arm64-cc (cs-arm64.cc arm64)))

(define-arch-specific instruction-updates-flags-p ((arm64 capstone-ffi:cs-arm64))
  (not (zerop (cs-arm64.update-flags arm64))))

(define-arch-specific instruction-writeback-p ((arm64 capstone-ffi:cs-arm64))
  (not (zerop (cs-arm64.writeback arm64))))

(defmethod operand-type ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-op-type (cs-arm64-op.type op)))

(defmethod operand-count ((arm64 capstone-ffi:cs-arm64))
  (cs-arm64.op-count arm64))

(defmethod operand-ref ((arm64 capstone-ffi:cs-arm64) (i integer))
  (cs-arm64.operands[] arm64 i))

(defmethod operand-register ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :reg)
  (enum-key 'capstone-ffi:arm64-reg (cs-arm64-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :imm)
  (cs-arm64-op.imm op))

(defmethod operand-float-value ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :fp)
  (cs-arm64-op.fp op))

(defmethod operand-sys-op ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :sys)
  (cs-arm64-op.sys op))

(defmethod operand-vector-index ((op capstone-ffi:cs-arm64-op))
  (cs-arm64-op.vector-index op))

(defmethod operand-vector-arrangement-specifier ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-vas (cs-arm64-op.vas op)))

(defmethod operand-vector-element-size-specifier ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-vess (cs-arm64-op.vess op)))

(defmethod operand-shift-type ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-shifter (cs-arm64-op.shift.type op)))

(defmethod operand-shift-value ((op capstone-ffi:cs-arm64-op))
  (cs-arm64-op.shift.value op))

(defmethod operand-extender ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-extender (cs-arm64-op.ext op)))

(defmethod operand-pstate ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-pstate (cs-arm64-op.pstate op)))

(defmethod operand-prefetch ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-prefetch-op (cs-arm64-op.prefetch op)))

(defmethod operand-barrier ((op capstone-ffi:cs-arm64-op))
  (enum-key 'capstone-ffi:arm64-barrier-op (cs-arm64-op.barrier op)))

(defmethod operand-access ((op capstone-ffi:cs-arm64-op))
  (cs-arm64-op.access op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:arm64-reg (cs-arm64-op.mem.base op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:arm64-reg (cs-arm64-op.mem.index op)))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-arm64-op))
  (check-operand-type op :mem)
  (cs-arm64-op.mem.disp op))
