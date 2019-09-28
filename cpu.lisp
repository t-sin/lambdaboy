(defpackage #:lambdaboy.cpu
  (:use #:cl)
  (:export))
(in-package #:lambdaboy.cpu)

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
