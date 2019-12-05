;;;; suite.lisp

(in-package #:capstone-test)

(def-suite capstone
  :description "Toplevel test suite for CL-CAPSTONE")

(def-suite arm
  :description "Tests for ARM"
  :in capstone)

(def-suite mips
  :description "Tests for MIPSen"
  :in capstone)

(def-suite x86
  :description "Tests for X86"
  :in capstone)

(def-suite ppc
  :description "Tests for PowerPC"
  :in capstone)
