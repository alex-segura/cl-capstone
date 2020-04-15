;;;; cl-capstone.asd

(defsystem #:cl-capstone
  :name "cl-capstone"
  :description "Bindings to the capstone engine disassembly library"
  :version "0.0.1"
  :author "Alex Segura <alex@lispm.dev>"
  :depends-on (#:alexandria
               #:cl-autowrap/libffi
               #:cl-plus-c)
  :pathname "src"
  :serial t
  :components
  ((:module :autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "capstone.arm-pc-linux.gnu.spec")
     (:static-file "capstone.i386-unknown-freebsd.spec")
     (:static-file "capstone.i386-unknown-openbsd.spec")
     (:static-file "capstone.i686-apple-darwin9.spec")
     (:static-file "capstone.i686-pc-windows-msvc.spec")
     (:static-file "capstone.x86_64-apple-darwin9.spec")
     (:static-file "capstone.x86_64-pc-linux-gnu.spec")
     (:static-file "capstone.x86_64-unknown-freebsd.spec")
     (:static-file "capstone.x86_64-unknown-openbsd.spec")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "bitfield")
   (:file "capstone")
   (:file "instruction")
   (:module :arch
    :pathname "arch"
    :serial t
    :components
    ((:file "arm")
     (:file "arm64")
     (:file "evm")
     (:file "m680x")
     (:file "m68k")
     (:file "mips")
     (:file "ppc")
     (:file "sparc")
     (:file "systemz")
     (:file "tms320c64x")
     (:file "xcore")
     (:file "x86"))))
  :in-order-to ((test-op (test-op #:cl-capstone-test))))

(defsystem #:cl-capstone-test
  :name "cl-capstone-test"
  :description "Test suite for CL-CAPSTONE library"
  :version "0.0.1"
  :author "Alex Segura <alex@lispm.dev>"
  :depends-on (#:cl-capstone #:fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam
                                      '#:run!
                                      (uiop:find-symbol* '#:capstone :capstone-test)))
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "suite")
   (:file "util")
   (:file "x86")
   (:file "mips")
   (:file "ppc")))
