;;;; tms320c64x.lisp

(in-package #:capstone)

(defmethod operand-count ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.op-count tms))

(defmethod operand-ref ((tms capstone-ffi:cs-tms320c64x) (i integer))
  (cs-tms320c64x.operands[] tms i))

(define-arch-specific instruction-parallel ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.parallel tms))

(define-arch-specific instruction-condition-register ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.condition.reg tms))

(define-arch-specific instruction-condition-zero ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.condition.zero tms))

(define-arch-specific instruction-funit-unit ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.funit.unit tms))

(define-arch-specific instruction-funit-side ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.funit.side tms))

(define-arch-specific instruction-funit-crosspath ((tms capstone-ffi:cs-tms320c64x))
  (cs-tms320c64x.funit.crosspath tms))

(defmethod operand-type ((op capstone-ffi:cs-tms320c64x-op))
  (enum-key 'capstone-ffi:tms320c64x-op-type (cs-tms320c64x-op.type op)))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:tms320c64x-reg (cs-tms320c64x-op.mem.base op)))

(defmethod memory-operand-displacement-type ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:tms320c64x-mem-disp (cs-tms320c64x-op.mem.disptype op)))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (let ((disp (cs-tms320c64x-op.mem.disp op)))
    (case (memory-operand-displacement-type op)
      (:constant disp)
      (:register (enum-key 'capstone-ffi:tms320c64x-reg disp)))))

(defmethod memory-operand-unit ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (cs-tms320c64x-op.mem.unit op))

(defmethod memory-operand-scaled ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (cs-tms320c64x-op.mem.scaled op))

(defmethod memory-operand-direction ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:tms320c64x-mem-dir (cs-tms320c64x-op.mem.direction op)))

(defmethod memory-operand-modify ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :mem)
  (enum-key 'capstone-ffi:tms320c64x-mem-mod (cs-tms320c64x-op.mem.modify op)))

(defmethod operand-register ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op (or :reg :regpair))
  (enum-key 'capstone-ffi:tms320c64x-reg (cs-tms320c64x-op.reg op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-tms320c64x-op))
  (check-operand-type op :imm)
  (cs-tms320c64x-op.imm op))
