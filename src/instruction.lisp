;;;; instruction.lisp

(in-package #:capstone)

(defun architecture-specific-detail (instruction)
  (ecase *architecture*
    (:x86 (cs-insn.detail*.x86 instruction))
    (:arm (cs-insn.detail*.arm instruction))
    (:arm64 (cs-insn.detail*.arm64 instruction))
    (:evm (cs-insn.detail*.evm instruction))
    (:sparc (cs-insn.detail*.sparc instruction))
    (:sysz (cs-insn.detail*.sysz instruction))
    (:mips (cs-insn.detail*.mipsen instruction))
    (:m680x (cs-insn.detail*.m680x instruction))
    (:m68k (cs-insn.detail*.m68k instruction))
    (:tms320c64x (cs-insn.detail*.tms320c64x instruction))
    (:xcore (cs-insn.detail*.xcore instruction))
    (:ppc (cs-insn.detail*.ppc instruction))))

(defun architecture-group-enum ()
  (ecase *architecture*
    (:x86 'capstone-ffi:x86-insn-group)
    (:arm 'capstone-ffi:arm-insn-group)
    (:arm64 'capstone-ffi:arm64-insn-group)
    (:evm 'capstone-ffi:evm-insn-group)
    (:sparc 'capstone-ffi:sparc-insn-group)
    (:sysz 'capstone-ffi:sysz-insn-group)
    (:mips 'capstone-ffi:mips-insn-group)
    (:m680x 'capstone-ffi:m680x-group-type)
    (:m68k 'capstone-ffi:m68k-group-type)
    (:tms320c64x 'capstone-ffi:tms320c64x-insn-group)
    (:xcore 'capstone-ffi:xcore-insn-group)
    (:ppc 'capstone-ffi:ppc-insn-group)))

(defmacro check-detail (form)
  (with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (cffi:null-pointer-p (c-ref ,wrapper capstone-ffi:cs-insn :detail))
           (error 'capstone-error :error-code capstone-ffi:+cs-err-detail+)
           ,wrapper))))

(defun instruction-id (instruction)
  (cs-insn.id instruction))

(defun instruction-address (instruction)
  (cs-insn.address instruction))

(defun instruction-size (instruction)
  (cs-insn.size instruction))

(defun instruction-bytes (instruction)
  (let* ((size (instruction-size instruction))
         (bytes (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref bytes i) (cs-insn.bytes[] instruction i)))
    bytes))

(defun instruction-mnemonic (instruction)
  (cffi:foreign-string-to-lisp (cs-insn.mnemonic[]& instruction)
   :max-chars capstone-ffi:+cs-mnemonic-size+
   :encoding :ascii))

(defun instruction-operand-string (instruction)
  (cffi:foreign-string-to-lisp (cs-insn.op-str[]& instruction)
   :max-chars 160
   :encoding :ascii))

(defun instruction-detail (instruction)
  (cs-insn.detail* instruction))

(defun instruction-registers-read (instruction)
  (check-detail instruction)
  (let ((count (cs-insn.detail*.regs-read-count instruction)))
    (loop :for i :from 0 :below count
          :collect (cs-insn.detail*.regs-read[] instruction i))))

(defun instruction-registers-written (instruction)
  (check-detail instruction)
  (let ((count (cs-insn.detail*.regs-write-count instruction)))
    (loop :for i :from 0 :below count
          :collect (cs-insn.detail*.regs-write[] instruction i))))

(defgeneric operand-ref (object i)
  (:method ((insn capstone-ffi:cs-insn) (i integer))
    (check-detail insn)
    (operand-ref (architecture-specific-detail insn) i)))

(defgeneric operand-count (object)
  (:method ((insn capstone-ffi:cs-insn))
    (check-detail insn)
    (operand-count (architecture-specific-detail insn))))

(defgeneric instruction-operands (instruction)
  (:method ((insn capstone-ffi:cs-insn))
    (check-detail insn)
    (let* ((detail (architecture-specific-detail insn))
           (count (operand-count detail)))
      (loop :for i :from 0 :below count
            :collect (operand-ref detail i)))))

(defun instruction-groups (instruction)
  (check-detail instruction)
  (loop :for i :from 0 :below (cs-insn.detail*.groups-count instruction)
        :collect (enum-key (architecture-group-enum)
                           (cs-insn.detail*.groups[] instruction i))))

(defgeneric operand-access (operand)
  (:method :around ((operand t))
    (mask-keywords 'cs-access (call-next-method))))

(defgeneric operand-type (operand))

(defgeneric operand-type-p (operand type)
  (:method ((operand t) (type symbol))
    (eql (operand-type operand) (alexandria:make-keyword type))))

(defgeneric memory-operand-base-register (operand))
(defgeneric memory-operand-index-register (operand))
(defgeneric memory-operand-segment-register (operand))
(defgeneric memory-operand-displacement (operand))

(defgeneric operand-register (operand))
(defgeneric operand-immediate-value (operand))
(defgeneric operand-float-value (operand))

(defmacro define-arch-specific (name args &body body)
  (let* ((generic-function-p
           (and (fboundp name)
                (typep (fdefinition name) 'standard-generic-function)))
         (method-defined-p
           (and generic-function-p
                (handler-case (find-method (fdefinition name) nil (list 'capstone-ffi:cs-insn))
                  (t nil)))))
    `(progn
       ,(unless generic-function-p
          `(defgeneric ,name (instruction)))
       ,(unless method-defined-p
          `(defmethod ,name ((insn capstone-ffi:cs-insn))
               (,name (architecture-specific-detail insn))))
       (defmethod ,name ,args ,@body))))

(defmacro check-operand-type (op type)
  (with-gensyms (ty)
    (flet ((or-form-p (form)
             (and (consp form) (eql (car form) 'or)))
           (and-form-p (form)
             (and (consp form) (eql (car form) 'and))))
      `(let ((,ty (operand-type ,op)))
         (unless ,(cond
                    ((or-form-p type)
                     `(or ,@(mapcar (lambda (s) `(eql ,ty ,s)) (cdr type))))
                    ((and-form-p type)
                     `(and ,@(mapcar (lambda (s) `(eql ,ty ,s)) (cdr type))))
                    (t `(eql ,ty ,type)))
           (error 'capstone-error :string "Operand type mismatch"))))))
