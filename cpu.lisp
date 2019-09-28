(defpackage #:lambdaboy
  (:use #:cl)
  (:export))
(in-package #:lambdaboy)

(defclass memory ()
  ())

(defgeneric mem-set (m addr))
(defgeneric mem-get (m addr v))

(defgeneric mem-setw (m addr))
(defgeneric mem-getw (m addr w))

(defclass memory-map (memory)
  (map))

(defstruct cpu
  ;;; internal states
  halted?  ; TBD

  ;;; registers
  ;;; this would be changed

  ;; 8-bit registers pair can be used as one 16-bit register
  ;; it should be a 16-bit bit vector?

  ;; 8-bit registers set #1
  a f  ; accumlator & flag register
  b c d e h l  ; general purpose registers

  ;; 8-bit registers set #2
  a* f*  ; accumlator & flag register
  b* c* d* e* h* l*  ; general purpose registers

  ;; special purpose registers
  i  ; interrupt page access register
  r  ; memory refresh register
  ix  ; index register #1
  iy  ; index register #2
  sp  ; stack pointer
  pc  ; program counter

  ;;; memory
  mem
)

(defun make-cpu* ()
  (make-cpu
   :mem nil
   :halted? nil
   :a 0 :f 0
   :b 0 :c 0 :d 0 :e 0 :h 0 :l 0
   :a* 0 :f* 0
   :b* 0 :c* 0 :d* 0 :e* 0 :h* 0 :l* 0
   :i 0 :r 0 :ix 0 :iy 0 :sp 0 :pc 0))

(defun run-1 (cpu)
  (format t "pc: ~a~%" (cpu-pc cpu)))

(defun run (cpu)
  (loop
    :for n := 0 :then (incf n)
    :until (cpu-halted? cpu)
    :when (= n 100) :do (setf (cpu-halted? cpu) t)
    :do (run-1 cpu)))
