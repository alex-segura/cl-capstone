;;;; autowrap.lisp

(cl:in-package #:capstone-ffi)

(autowrap:c-include "/usr/local/include/capstone/capstone.h"
 :accessor-package #:capstone-ffi.accessors
 :function-package #:capstone-ffi.functions
 :exclude-sources ("/usr/include")
 :include-sources ("stdint.h"
                   "bits/types.h"
                   "sys/types.h"
                   "sys/_types.h"
                   "bits/stdint"
                   "machine/_types.h")
 :exclude-definitions ("truncate" "ftruncate" "abs")
 :spec-path '(:cl-capstone :autowrap-spec)
 :release-p cl:t)
