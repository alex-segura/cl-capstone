;;;; capstone.lisp

(in-package #:capstone)

(define-condition capstone-error (error)
  ((string
    :initform nil
    :initarg :string
    :reader capstone-error-string))
  (:report (lambda (condition stream)
             (format stream "~a" (capstone-error-string condition)))))

(define-condition capstone-rc-error (capstone-error)
  ((error-code
    :initform 0
    :initarg :error-code
    :reader capstone-error-code))
  (:report (lambda (condition stream)
             (format stream "~a" (capstone-error-string condition)))))

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (= ,rc capstone-ffi:+cs-err-ok+)
         (error 'capstone-rc-error :error-code ,rc
                                   :string (cs-strerror ,rc)))
       ,rc)))

(defmacro check-null (form)
  (once-only (form)
    `(if (wrapper-null-p ,form)
         (error "NULL returned from ~A" ',form)
         ,form)))

(defvar *architecture*)

(defstruct handle
  csh
  architecture
  mode
  open-p)

(defun version ()
  "Retrieve the version of the loaded capstone library.
Returns multiple values: major version and minor version."
  (c-with ((major :int)
           (minor :int))
    (cs-version (major &) (minor &))
    (values major minor)))

(defun open-handle (arch mode)
  "Open a CS handle using ARCHITECTURE and MODE."
  (c-with ((csh capstone-ffi:csh))
    (check-rc (cs-open arch mode (csh &)))
    (make-handle :csh csh
                 :architecture arch
                 :mode mode
                 :open-p t)))

(defun close-handle (handle)
  "Close the CS handle HANDLE."
  (when (handle-open-p handle)
    (c-with ((csh capstone-ffi:csh))
      (setf csh (handle-csh handle))
      (prog1 (check-rc (cs-close (csh &)))
        (setf (handle-open-p handle) nil)))))

(defcallback skipdata-callback :unsigned-long
    ((code :pointer)
     (code-size :unsigned-long)
     (offset :unsigned-long)
     (user-data :pointer))
  (destructuring-bind (lisp-function lisp-user-data)
      user-data
    (funcall lisp-function code code-size offset user-data lisp-user-data)))

(defun set-option (handle option value)
  "Set OPTION to VALUE for the open handle HANDLE."
  (with-slots (csh) handle
    (let ((opt (enum-value 'capstone-ffi:cs-opt-type option))
          (val (if (keywordp value)
                   (enum-value 'capstone-ffi:cs-opt-value value)
                   value)))
      (check-rc (cs-option csh opt val)))))

(defun errno (handle) (cs-errno (handle-csh handle)))

(defun call-with-open-handle (f architecture mode)
  (let ((handle (open-handle architecture mode)))
    (unwind-protect (funcall f handle)
      (close-handle handle))))

(defmacro with-open-handle ((var architecture mode &rest modes) &body body)
  "Bind VAR to an open CSH in the context of BODY. The handle is opened using the architecture
keyword ARCHITECTURE, mode keyword MODE, and any additional MODES."
  `(call-with-open-handle
    (lambda (,var)
      ,(when (or modes (eql mode :big-endian))
         ;; HACK: autowrap makes enums into signed ints.
         ;; when using :big-endian (1 << 31) mode, this results in an error when
         ;; calling cs-open with the converted bitmask.
         ;; set-option is declared as accepting a size_t instead of int, so this
         ;; seems to work ok.
         `(set-option ,var :mode (autowrap:mask-apply 'cs-mode (list ,mode ,@modes))))
      ,@body)
    ,architecture
    ,(if (eql mode :big-endian) :little-endian mode)))

(defun call-with-cs-insn (function handle)
  (let ((insn (check-null (cs-malloc (handle-csh handle)))))
    (unwind-protect (funcall function insn)
      (cs-free insn 1))))

(defmacro with-cs-insn ((name handle) &body body)
  `(call-with-cs-insn (lambda (,name) ,@body) ,handle))

(defmacro do-disassembled-instructions ((var handle bytes &key (start-address 0)) &body body)
  "Iteratively disassemble BYTES using the opened handle HANDLE. VAR is bound to the disassembled
instruction in the context of BODY. The address of the first instruction can be specified using
th START-ADDRESS keyword."
  (once-only (bytes handle)
    (with-gensyms (insn code code* size addr ret)
      `(with-cs-insn (,insn ,handle)
         (cffi:with-foreign-array (,code ,bytes `(:array :uint8 ,(length ,bytes)))
           (c-with ((,code* :pointer)
                    (,size :unsigned-long-long)
                    (,addr :unsigned-long-long))
             (setf ,code* ,code
                   ,size (length ,bytes)
                   ,addr ,start-address)
             (loop :for ,ret := (cs-disasm-iter (handle-csh ,handle)
                                                (,code* &) (,size &) (,addr &) (ptr ,insn))
                   :until (zerop ,ret)
                   :do (let ((,var ,insn)
                             (*architecture* (handle-architecture ,handle)))
                         ,@body))))))))

(defmacro do-instruction-operands ((var count instruction) &body body)
  "Iterate over the operands of INSTRUCTION, binding VAR sequentially to each operand and COUNT
to the operand's index, in the context of BODY."
  (with-gensyms (i)
    `(dotimes (,i (operand-count ,instruction))
       (let ((,var (operand-ref ,instruction ,i))
             (,count ,i))
         ,@body))))

#+nil
(progn
(defun instruction-group-p (instruction group)
  (cs-insn-group *csh* instruction group))

(defun instruction-operand-index (instruction op-type position)
  (cs-op-index *csh* instruction op-type position))

(defun instruction-operand-count (instruction op-type)
  (cs-op-count *csh* instruction op-type))

(defun instruction-register-read-p (instruction register-id)
  (not (zerop (cs-reg-read *csh* instruction register-id))))

(defun instruction-register-written-p (instruction register-id)
  (not (zerop (cs-reg-write *csh* instruction register-id))))
)
