;;;; arm.lisp

(in-package #:capstone)

(defmethod operand-count ((arm capstone-ffi:cs-arm))
  (cs-arm.op-count arm))

(defmethod operand-ref ((arm capstone-ffi:cs-arm) (i integer))
  (cs-arm.operands[] arm i))

(define-arch-specific instruction-usermode-p ((arm capstone-ffi:cs-arm))
  (not (zerop (cs-arm.usermode arm))))

(define-arch-specific instruction-vector-size ((arm capstone-ffi:cs-arm))
  (cs-arm.vector-size arm))

(define-arch-specific instruction-vector-data ((arm capstone-ffi:cs-arm))
  (enum-key 'capstone-ffi:arm-vectordata-type (cs-arm.vector-data arm)))

(define-arch-specific instruction-cps-mode ((arm capstone-ffi:cs-arm))
  (enum-key 'capstone-ffi:arm-cpsmode-type (cs-arm.cps-mode arm)))

(define-arch-specific instruction-cps-flag ((arm capstone-ffi:cs-arm))
  (enum-key 'capstone-ffi:arm-cpsflag-type (cs-arm.cps-flag arm)))

(define-arch-specific instruction-condition-code ((arm capstone-ffi:cs-arm))
  (enum-key 'capstone-ffi:arm-cc (cs-arm.cc arm)))

(define-arch-specific instruction-updates-flags-p ((arm capstone-ffi:cs-arm))
  (not (zerop (cs-arm.update-flags arm))))

(define-arch-specific instruction-writeback-p ((arm capstone-ffi:cs-arm))
  (not (zerop (cs-arm.writeback arm))))

(define-arch-specific instruction-memory-barrier ((arm capstone-ffi:cs-arm))
  (enum-key 'capstone-ffi:arm-mem-barrier (cs-arm.mem-barrier arm)))

(defmethod operand-type ((op capstone-ffi:cs-arm-op))
  (enum-key 'capstone-ffi:arm-op-type (cs-arm-op.type op)))

(defmethod operand-register ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :reg)
  (enum-key 'capstone-ffi:arm-reg (cs-arm-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :imm)
  (cs-arm-op.imm op))

(defmethod operand-float-value ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :fp)
  (cs-arm-op.fp op))

(defmethod operand-vector-index ((op capstone-ffi:cs-arm-op))
  (cs-arm-op.vector-index op))

(defmethod operand-shift-type ((op capstone-ffi:cs-arm-op))
  (cs-arm-op.shift.type op))

(defmethod operand-shift-value ((op capstone-ffi:cs-arm-op))
  (cs-arm-op.shift.value op))

(defmethod operand-subtracted-p ((op capstone-ffi:cs-arm-op))
  (not (zerop (cs-arm-op.subtracted op))))

(defmethod operand-access ((op capstone-ffi:cs-arm-op))
  (cs-arm-op.access op))

(defmethod operand-neon-lane ((op capstone-ffi:cs-arm-op))
  (cs-arm-op.neon-lane op))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:arm-reg (cs-arm-op.mem.base op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:arm-reg (cs-arm-op.mem.index op)))

(defmethod memory-operand-scale ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :mem)
  (cs-arm-op.mem.scale op))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :mem)
  (cs-arm-op.mem.disp op))

(defmethod memory-operand-lshift ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :mem)
  (cs-arm-op.mem.lshift op))

(defmethod operand-setend ((op capstone-ffi:cs-arm-op))
  (check-operand-type op :setend)
  (enum-key 'capstone-ffi:arm-setend-type (cs-arm-op.setend op)))
