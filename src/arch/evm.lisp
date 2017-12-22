;;;; evm.lisp

(in-package #:capstone)

(define-arch-specific instruction-number-of-items-popped ((evm capstone-ffi:cs-evm))
  (cs-evm.pop evm))

(define-arch-specific instruction-number-of-items-pushed ((evm capstone-ffi:cs-evm))
  (cs-evm.push evm))

(define-arch-specific instruction-fee ((evm capstone-ffi:cs-evm))
  (cs-evm.fee evm))
