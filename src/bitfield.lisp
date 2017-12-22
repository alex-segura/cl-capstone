;;;; bitfield.lisp

(in-package #:capstone)

(autowrap:define-bitmask-from-constants (cs-mode)
  capstone-ffi:+cs-mode-16+            capstone-ffi:+cs-mode-32+
  capstone-ffi:+cs-mode-64+            capstone-ffi:+cs-mode-arm+
  capstone-ffi:+cs-mode-big-endian+    capstone-ffi:+cs-mode-little-endian+
  capstone-ffi:+cs-mode-m680x-6301+    capstone-ffi:+cs-mode-m680x-6309+
  capstone-ffi:+cs-mode-m680x-6800+    capstone-ffi:+cs-mode-m680x-6801+
  capstone-ffi:+cs-mode-m680x-6805+    capstone-ffi:+cs-mode-m680x-6808+
  capstone-ffi:+cs-mode-m680x-6809+    capstone-ffi:+cs-mode-m680x-6811+
  capstone-ffi:+cs-mode-m680x-cpu12+   capstone-ffi:+cs-mode-m680x-hcs08+
  capstone-ffi:+cs-mode-m68k-000+      capstone-ffi:+cs-mode-m68k-010+
  capstone-ffi:+cs-mode-m68k-020+      capstone-ffi:+cs-mode-m68k-030+
  capstone-ffi:+cs-mode-m68k-040+      capstone-ffi:+cs-mode-m68k-060+
  capstone-ffi:+cs-mode-mclass+        capstone-ffi:+cs-mode-micro+
  capstone-ffi:+cs-mode-mips2+         capstone-ffi:+cs-mode-mips3+
  capstone-ffi:+cs-mode-mips32+        capstone-ffi:+cs-mode-mips32r6+
  capstone-ffi:+cs-mode-mips64+        capstone-ffi:+cs-mode-qpx+
  capstone-ffi:+cs-mode-thumb+         capstone-ffi:+cs-mode-v8+
  capstone-ffi:+cs-mode-v9+)

(autowrap:define-bitmask-from-constants (x86-eflags)
  capstone-ffi:+x86-eflags-modify-af+           capstone-ffi:+x86-eflags-modify-cf+
  capstone-ffi:+x86-eflags-modify-df+           capstone-ffi:+x86-eflags-modify-if+
  capstone-ffi:+x86-eflags-modify-nt+           capstone-ffi:+x86-eflags-modify-of+
  capstone-ffi:+x86-eflags-modify-pf+           capstone-ffi:+x86-eflags-modify-rf+
  capstone-ffi:+x86-eflags-modify-sf+           capstone-ffi:+x86-eflags-modify-tf+
  capstone-ffi:+x86-eflags-modify-zf+           capstone-ffi:+x86-eflags-prior-af+
  capstone-ffi:+x86-eflags-prior-cf+            capstone-ffi:+x86-eflags-prior-df+
  capstone-ffi:+x86-eflags-prior-if+            capstone-ffi:+x86-eflags-prior-nt+
  capstone-ffi:+x86-eflags-prior-of+            capstone-ffi:+x86-eflags-prior-pf+
  capstone-ffi:+x86-eflags-prior-sf+            capstone-ffi:+x86-eflags-prior-tf+
  capstone-ffi:+x86-eflags-prior-zf+            capstone-ffi:+x86-eflags-reset-0f+
  capstone-ffi:+x86-eflags-reset-ac+            capstone-ffi:+x86-eflags-reset-af+
  capstone-ffi:+x86-eflags-reset-cf+            capstone-ffi:+x86-eflags-reset-df+
  capstone-ffi:+x86-eflags-reset-if+            capstone-ffi:+x86-eflags-reset-nt+
  capstone-ffi:+x86-eflags-reset-of+            capstone-ffi:+x86-eflags-reset-pf+
  capstone-ffi:+x86-eflags-reset-rf+            capstone-ffi:+x86-eflags-reset-sf+
  capstone-ffi:+x86-eflags-reset-tf+            capstone-ffi:+x86-eflags-reset-zf+
  capstone-ffi:+x86-eflags-set-af+              capstone-ffi:+x86-eflags-set-cf+
  capstone-ffi:+x86-eflags-set-df+              capstone-ffi:+x86-eflags-set-if+
  capstone-ffi:+x86-eflags-set-of+              capstone-ffi:+x86-eflags-set-pf+
  capstone-ffi:+x86-eflags-set-sf+              capstone-ffi:+x86-eflags-set-zf+
  capstone-ffi:+x86-eflags-test-af+             capstone-ffi:+x86-eflags-test-cf+
  capstone-ffi:+x86-eflags-test-df+             capstone-ffi:+x86-eflags-test-if+
  capstone-ffi:+x86-eflags-test-nt+             capstone-ffi:+x86-eflags-test-of+
  capstone-ffi:+x86-eflags-test-pf+             capstone-ffi:+x86-eflags-test-rf+
  capstone-ffi:+x86-eflags-test-sf+             capstone-ffi:+x86-eflags-test-tf+
  capstone-ffi:+x86-eflags-test-zf+             capstone-ffi:+x86-eflags-undefined-af+
  capstone-ffi:+x86-eflags-undefined-cf+        capstone-ffi:+x86-eflags-undefined-of+
  capstone-ffi:+x86-eflags-undefined-pf+        capstone-ffi:+x86-eflags-undefined-sf+
  capstone-ffi:+x86-eflags-undefined-zf+)

(autowrap:define-bitmask-from-constants (x86-fpu-flags)
  capstone-ffi:+x86-fpu-flags-modify-c0+
  capstone-ffi:+x86-fpu-flags-modify-c1+
  capstone-ffi:+x86-fpu-flags-modify-c2+
  capstone-ffi:+x86-fpu-flags-modify-c3+
  capstone-ffi:+x86-fpu-flags-reset-c0+
  capstone-ffi:+x86-fpu-flags-reset-c1+
  capstone-ffi:+x86-fpu-flags-reset-c2+
  capstone-ffi:+x86-fpu-flags-reset-c3+
  capstone-ffi:+x86-fpu-flags-set-c0+
  capstone-ffi:+x86-fpu-flags-set-c1+
  capstone-ffi:+x86-fpu-flags-set-c2+
  capstone-ffi:+x86-fpu-flags-set-c3+
  capstone-ffi:+x86-fpu-flags-test-c0+
  capstone-ffi:+x86-fpu-flags-test-c1+
  capstone-ffi:+x86-fpu-flags-test-c2+
  capstone-ffi:+x86-fpu-flags-test-c3+
  capstone-ffi:+x86-fpu-flags-undefined-c0+
  capstone-ffi:+x86-fpu-flags-undefined-c1+
  capstone-ffi:+x86-fpu-flags-undefined-c2+
  capstone-ffi:+x86-fpu-flags-undefined-c3+)

(autowrap:define-bitmask-from-constants (cs-access)
  capstone-ffi:+cs-ac-invalid+
  capstone-ffi:+cs-ac-read+
  capstone-ffi:+cs-ac-write+)
