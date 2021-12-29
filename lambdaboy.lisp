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

(defmacro make-flag-accessors (name bit)
  (let* ((n-bit (ash 1 bit))
         (n-bit-not (lognot n-bit)))
    (let ((%accessor-name (intern (format nil "REGISTER-FLAG-~a" name))))
      (with-gensyms ($reg $val)
        `(progn
           (defun ,%accessor-name (,$reg)
             (not (= 0 (logand (register-f ,$reg) ,n-bit))))

           (defun (setf ,%accessor-name) (,$val ,$reg)
             (setf (register-f ,$reg)
                   (logior (logand (register-f ,$reg) ,n-bit-not)
                           (if ,$val ,n-bit 0)))))))))

;; auto generated `register-flag-zero` and its setf function.
;; zero flag (z) is set if the result of an operation is zero.
(make-flag-accessors zero 7)

;; auto generated `register-flag-subtract` and its setf function.
(make-flag-accessors subtract 6)

;; auto generated `register-flag-half-carry` and its setf function.
(make-flag-accessors half-carry 5)

;; auto generated `register-flag-carry` and its setf function.
(make-flag-accessors carry 4)

;;; Memory

;; TODO: I think that the memory map should be a balanced binary tree-like structure but
;; I choose naive and rough way for now.

(defstruct range
  (start 0 :type (unsigned-byte 16))
  (end 0 :type (unsigned-byte 16)))

(defun in-range-p (range n)
  (and (<= (range-start range) n)
       (<= n (range-end range))))

(defun overwrap-p (range1 range2)
  (let ((s1 (range-start range1))
        (e1 (range-end range1))
        (s2 (range-start range2))
        (e2 (range-end range2)))
    (if (<= s1 s2)
        (<= s2 e1)
        (<= s1 e2))))

(defstruct memory-block
  (range nil :type range)
  (array nil :type (array (unsigned-byte 8)))
  (write-hook nil :type function))

(defstruct (memory (:constructor make-memory*))
  (map (vector) :type (vector memory-block)))

(defun map-memory (mem block)
  (if (loop
        :for b :across (memory-map mem)
        :always (not (overwrap-p (memory-block-range block) (memory-block-range b))))
      (setf (memory-map mem)
            (concatenate 'vector (memory-map mem) (vector block)))
      (error "new block ~a is overwrapped existed ranges:~%  ~a"
             (memory-block-range block)
             (map 'list (lambda (b) (memory-block-range b)) (memory-map mem)))))

(flet ((do-nothing (addr val)
         (declare (ignore addr val))))
  (defun map-memory* (mem s e &optional (fn #'do-nothing))
    (let ((block (make-memory-block :range (make-range :start s :end e)
                                    :array (make-array (1+ (- e s)) :element-type '(unsigned-byte 8))
                                    :write-hook fn)))
      (map-memory mem block))))

(defun find-block (mem addr)
  (loop
    :for b :across (memory-map mem)
    :do (when (in-range-p (memory-block-range b) addr)
          (return-from find-block b))))

(defun memory-address (mem addr)
  (let ((b (find-block mem addr)))
    (when b
      (aref (memory-block-array b)
            (- addr (range-start (memory-block-range b)))))))

(defun (setf memory-address) (val mem addr)
  (let ((b (find-block mem addr)))
    (when b
      (let ((offset (- addr (range-start (memory-block-range b)))))
        (when (memory-block-write-hook b)
          (funcall (memory-block-write-hook b) offset val))
        (setf (aref (memory-block-array b) offset) val)))))

;; make memory and initialize memory map according to this document:
;; - https://gbdev.io/pandocs/Memory_Map.html
(defun make-memory ()
  (let ((mem (make-memory*)))
    ;; ROM cartridge bank 00 (16KiB). its usually fixed bank.
    (map-memory* mem #x0000 #x3fff)
    ;; ROM cartridge bank 01 ~ nn (16KiB). its switchable bank.
    (map-memory* mem #x4000 #x7fff)
    ;; Video RAM (8KiB).
    (map-memory* mem #x8000 #x9fff)
    ;; External RAM (8KiB).
    (map-memory* mem #xa000 #xbfff)
    ;; 4KiB Work RAM.
    (map-memory* mem #xc000 #xcfff)
    ;; 4KiB Work RAM 2.
    (map-memory* mem #xd000 #xdfff)
    ;; [Prohibited] Echo RAM: Mirror of #xc000 ~ #xdfff.
    (map-memory* mem #xe000 #xfdff)
    ;; Sprite attribute table.
    (map-memory* mem #xfe00 #xfe9f)
    ;; [Prohibited] Not usable.
    (map-memory* mem #xfea0 #xfeff)
    ;; IO registers
    (map-memory* mem #xff00 #xff7f)
    ;; High RAM
    (map-memory* mem #xff80 #xfffe)
    ;; Interrupt enable register
    (map-memory* mem #xffff #xffff)
    mem))

;;; gameboy

(defstruct gameboy
  (register (make-register) :type register)
  (memory (make-memory) :type memory))

;; execute power-up sequence
(defun initialize-gameboy (gb)
  (setf (register-pc (gameboy-register gb)) #x0100))

(defun load-rom (gb rom)
  (loop
    :for addr :from 0 :upto #x7fff
    :do (setf (memory-address (gameboy-memory gb) addr)
              (aref rom addr))))

(defun fetch-byte (gb)
  (prog1
      (memory-address (gameboy-memory gb)
                      (register-pc (gameboy-register gb)))
    (incf (register-pc (gameboy-register gb)))))

(defun execute-1 (gb)
  (let* ((reg (gameboy-register gb))
         (byte (fetch-byte gb)))
    (case byte
      (#x00 (vom:debug "op: NOP")
            nil)
      (#xc3 (vom:debug "op: JP a16")
            (let* ((lsb (fetch-byte gb))
                   (msb (fetch-byte gb))
                   (pc (8bit->16bit msb lsb)))
              (setf (register-pc reg) pc)))
      (t (error "unknown instruction: #x~x as pc = #x~x"
                byte (register-pc (gameboy-register gb)))))))

(defun run (gb)
  (loop
    (execute-1 gb)))

(defun start-gb (pathname)
  (let ((gb (make-gameboy))
        (rom (make-array #x8000 :element-type '(unsigned-byte 8))))
    (with-open-file (in pathname :direction :input
                        :element-type '(unsigned-byte 8))
      (read-sequence rom in))
    (load-rom gb rom)
    (initialize-gameboy gb)
    (run gb)))

;;;; util

(defun 8bit->16bit (msb lsb)
  (logior (ash msb 8) lsb))
