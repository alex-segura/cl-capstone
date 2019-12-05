;;;; m68k.lisp

(in-package #:capstone)

(define-arch-specific instruction-operand-size-type ((m68k capstone-ffi:cs-m68k))
  (enum-key 'capstone-ffi:m68k-size-type m68k))

(define-arch-specific instruction-operand-size ((m68k capstone-ffi:cs-m68k))
  (case (instruction-operand-size-type m68k)
    (:fpu (enum-key 'capstone-ffi:m68k-fpu-size (cs-m68k.op-size.fpu-size m68k)))
    (:cpu (enum-key 'capstone-ffi:m68k-cpu-size (cs-m68k.op-size.cpu-size m68k)))))

(defmethod operand-count ((m68k capstone-ffi:cs-m68k))
  (cs-m68k.op-count m68k))

(defmethod operand-ref ((m68k capstone-ffi:cs-m68k) (i integer))
  (cs-m68k.operands[] m68k i))

(defmethod operand-type ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-op-type (cs-m68k-op.type op)))

(defmethod operand-immediate-value ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.imm op))

(defmethod operand-double-value ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.dimm op))

(defmethod operand-float-value ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.simm op))

(defmethod operand-register ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-reg (cs-m68k-op.reg op)))

(defmethod operand-register-pair ((op capstone-ffi:cs-m68k-op))
  (mapcar (lambda (reg) (enum-key 'capstone-ffi:m68k-reg reg))
          (list (cs-m68k-op.reg-pair.reg-0 op)
                (cs-m68k-op.reg-pair.reg-1 op))))

(defmethod memory-operand-base-register ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-reg (cs-m68k-op.mem.base-reg op)))

(defmethod memory-operand-index-register ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-reg (cs-m68k-op.mem.index-reg op)))

(defmethod memory-operand-indirect-base-register ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-reg (cs-m68k-op.mem.in-base-reg op)))

(defmethod memory-operand-indirect-displacement ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.in-disp op))

(defmethod memory-operand-other-displacement ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.out-disp op))

(defmethod memory-operand-displacement ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.disp op))

(defmethod memory-operand-scale ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.scale op))

(defmethod memory-operand-bitfield-p ((op capstone-ffi:cs-m68k-op))
  (not (zerop (cs-m68k-op.mem.bitfield op))))

(defmethod memory-operand-width ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.width op))

(defmethod memory-operand-offset ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.offset op))

(defmethod memory-operand-index-size ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.mem.offset op))

(defmethod branch-displacement-operand-displacement ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.br-disp.disp op))

(defmethod branch-displacement-operand-displacement-size ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-op-br-disp-size (cs-m68k-op.br-disp.disp-size op)))

(defmethod operand-register-bits ((op capstone-ffi:cs-m68k-op))
  (cs-m68k-op.register-bits op))

(defmethod operand-addressing-mode ((op capstone-ffi:cs-m68k-op))
  (enum-key 'capstone-ffi:m68k-address-mode (cs-m68k-op.address-mode op)))
