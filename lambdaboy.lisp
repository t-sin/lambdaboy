(defpackage #:lambdaboy
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export))
(in-package #:lambdaboy)

;;;; CPU

;;; Registers
;;
;; Each slots of `register` is 16-bit integer because of a couple of 8-bit registers can be
;; treated as one 16-bit register.
;;
;; cf. https://gbdev.io/pandocs/CPU_Registers_and_Flags.html
(defstruct register
  ;; accumulator (A) & flags (F) registers
  ;;
  ;; Flags register (lower 8bits of af):
  ;;   bit 7: (z) zero flag
  ;;   bit 6: (n) subtraction flag (BCD)
  ;;   bit 5: (h) half carry flag (BCD)
  ;;   bit 4: (c) carry flag
  (af 0 :type (unsigned-byte 16))

  ;; general purpose (B) & (C) registers
  (bc 0 :type (unsigned-byte 16))
  ;; general purpose (D) & (E) registers
  (de 0 :type (unsigned-byte 16))
  ;; general purpose (H) & (L) registers
  (hl 0 :type (unsigned-byte 16))

  ;; stack pointer
  (sp 0 :type (unsigned-byte 16))
  ;; program counter
  (pc 0 :type (unsigned-byte 16)))

(defmacro make-accessors (two-char-reg)
  (let* ((name (symbol-name two-char-reg))
         (high-r (char name 0))
         (low-r (char name 1)))

    (let ((%reg-accessor (intern (format nil "REGISTER-~a" name)))
          (%reg-h-accessor (intern (format nil "REGISTER-~a" high-r)))
          (%reg-l-accessor (intern (format nil "REGISTER-~a" low-r))))
      (with-gensyms ($reg $reg-h $reg-l)
        `(progn
           (defun ,%reg-h-accessor (,$reg)
             (ash (,%reg-accessor ,$reg) -8))

           (defun (setf ,%reg-h-accessor) (,$reg-h ,$reg)
             (setf (,%reg-accessor ,$reg)
                   (logior (logand (ash ,$reg-h 8) #xff00)
                           (logand (,%reg-accessor ,$reg) #x00ff))))

           (defun ,%reg-l-accessor (,$reg)
             (logand (,%reg-accessor ,$reg) #x00ff))

           (defun (setf ,%reg-l-accessor) (,$reg-l ,$reg)
             (setf (,%reg-accessor ,$reg)
                   (logior (logand (,%reg-accessor ,$reg) #xff00)
                           (logand ,$reg-l #x00ff)))))))))

;; auto generate (register-a), (register-f) and their setf functions.
(make-accessors af)

;; auto generate (register-b), (register-c) and their setf functions.
(make-accessors bc)

;; auto generate (register-d), (register-e) and their setf functions.
(make-accessors de)

;; auto generate (register-h), (register-l) and their setf functions.
(make-accessors hl)
