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
  (memory (make-memory) :type memory)
  (interrupt-enabled t :type (member t nil)))

;; execute power-up sequence
(defun initialize-gameboy (gb)
  (setf (register-pc (gameboy-register gb)) #x0100
        (register-sp (gameboy-register gb)) #xfffe))
        ;; (register-af (gameboy-register gb)) #x0100
        ;; (register-bc (gameboy-register gb)) #xff13
        ;; (register-de (gameboy-register gb)) #x00c1
        ;; (register-hl (gameboy-register gb)) #x8403))

(defun load-rom (gb rom)
  (loop
    :for addr :from 0 :upto #x7fff
    :do (setf (memory-address (gameboy-memory gb) addr)
              (aref rom addr))))

(defun op-call (reg mem addr)
  (let ((sp (incf (register-sp reg) -2))
        (pc (1+ (register-pc reg))))
    (setf (memory-address mem sp) (logand pc #x00ff)
          (memory-address mem (1+ sp)) (ash (logand pc #xff00) -8)))
  (setf (register-pc reg) addr))

(defmacro 4bit-case ((ms4 ls4) &body clauses)
  `(cond
     ,@(loop
         :for (cond . body) :in clauses
         :collect `(,(cond ((eq (first cond) '_)
                            `(= ,ls4 ,(second cond)))
                           ((eq (second cond) '_)
                            `(= ,ms4 ,(first cond)))
                           (t `(and (= ,ms4 ,(first cond))
                                    (= ,ls4 ,(second cond)))))
                    ,@body))
     (t (error "unknown instruction: #x~x as pc = #x~x"
               opcode (register-pc (gameboy-register gb))))))

(defmacro select-register (idx reg-list)
  `(ecase ,idx
     ,@(loop
         :for n :from 0 :below (length reg-list)
         :collect `(,n ,(let ((r (elt reg-list n)))
                          (if (listp r)
                              (let (($accessor (intern (format nil "REGISTER-~a" (first r)))))
                                `(values ',r (memory-address mem (,$accessor reg))))
                              (let (($accessor (intern (format nil "REGISTER-~a" r))))
                                `(values ',r (,$accessor reg)))))))))

(defmacro set-register (idx reg-list val)
  `(ecase ,idx
     ,@(loop
         :for n :from 0 :below (length reg-list)
         :collect `(,n ,(let ((r (elt reg-list n)))
                          (if (listp r)
                              (let (($accessor (intern (format nil "REGISTER-~a" (first r)))))
                                `(progn
                                   (setf (memory-address mem (,$accessor reg)) ,val)
                                   ',r))
                              (let (($accessor (intern (format nil "REGISTER-~a" r))))
                                `(progn
                                   (setf (,$accessor reg) ,val)
                                   ',r))))))))

;; decode and execute one instruction
;;
;; cf. - https://gbdev.io/pandocs/CPU_Instruction_Set.html
;;     - https://gbdev.io/gb-opcodes/optables/
(defun execute-1 (gb)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (pc (register-pc reg))
         (opcode (memory-address mem pc))
         (op-ms4 (ash (logand opcode #xf0) -4))
         (op-ls4 (logand opcode #x0f)))
    (flet ((log-op (op &rest args)
             (vom:debug "[PC:#x~4,'0x, SP:#x~4,'0x] op ~2,'0x: ~a"
                        (register-pc reg) (register-sp reg)
                        opcode (apply #'format nil op args)))
           (operand-1 ()
             (memory-address mem (+ (register-pc reg) 1)))
           (operand-2 ()
             (memory-address mem (+ (register-pc reg) 2)))
           (set-flags (&key (zero nil zero-p)
                            (sub nil sub-p)
                            (hc nil hc-p)
                            (carry nil carry-p))
             (when zero-p
               (setf (register-flag-zero reg) zero))
             (when sub-p
               (setf (register-flag-subtract reg) sub))
             (when hc-p
               (register-flag-half-carry reg) hc)
             (when carry-p
               (register-flag-carry reg) carry)))
      (let ((pc-diff
              (4bit-case (op-ms4 op-ls4)
                ((#x0 #x0)
                 (log-op "NOP")
                 1)
                ((#xc #x3)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-op "JP #x~x" a16)
                   (setf (register-pc reg) a16))
                 0)
                ((#xc #x9)
                 (log-op "RET")
                 (let* ((addr (memory-address mem (register-sp reg)))
                        (addr (logior addr
                                      (ash (memory-address mem (1+ (register-sp reg))) 8))))
                   (incf (register-sp reg) 2)
                   (setf (register-pc reg) addr))
                 0)
                ((#xc #xb)
                 (let* ((pc (incf (register-pc reg)))
                        (opcode (memory-address mem pc))
                        (op-ms4 (ash (logand opcode #xf0) -4))
                        (op-ls4 (logand opcode #x0f)))
                   (flet ((reset-bit (n)
                            (let ((n (+ n (if (zerop (logand #x8 op-ls4))
                                              0
                                              1)))
                                  (idx (logand #x7 op-ls4)))
                              (multiple-value-bind (name val)
                                  (select-register idx (b c d e h l (hl) a))
                                (log-op "RES ~a, ~a" n name)
                                (set-register idx (b c d e h l (hl) a)
                                              (logand val (lognot (ash 1 n))))))))
                     (4bit-case (op-ms4 op-ls4)
                       ((#x8 _) (reset-bit 0)
                                1)
                       ((#x9 _) (reset-bit 2)
                                1)
                       ((#xa _) (reset-bit 4)
                                1)
                       ((#xb _) (reset-bit 6)
                                1)
                       ((#xe #x6)
                        (log-op "SET 4, (HL)")
                        (setf (register-hl reg)
                              (logand (register-hl reg) #.(lognot #x0008)))
                        1)))))
                ((#xc #xd)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-op "CALL #x~x" a16)
                   (op-call reg mem a16))
                 0)
                ((#xe #x0)
                 (log-op "LDH (#x~x), A" (operand-1))
                 (setf (memory-address mem (+ #xff00 (operand-1)))
                       (register-a reg))
                 2)
                ((#xe #xa)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-op "LD (#x~x), A" a16)
                   (setf (memory-address mem a16) (register-a reg))
                   3))
                ((#xf #x0)
                 (log-op "LDH A, (#x~x)" (operand-1))
                 (setf (register-a reg) (+ #xff00 (operand-1)))
                 2)
                ((#xf #x3)
                 (log-op "DI")
                 (setf (gameboy-interrupt-enabled gb) nil)
                 1)
                ((#xf #xa)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-op "LD A, #x~x" a16)
                   (setf (register-a reg) a16))
                 3)
                ((#xf #xe)
                 (log-op "CP #x~x " (operand-1))
                 (let ((result (- (register-a reg) (operand-1))))
                   (set-flags :zero (zerop result)
                              :sub t
                              :hc (> result #x0f)
                              :carry (minusp result)))
                 2)
                ((#x3 #x8)
                 (let ((offset (i8-as-integer (operand-1))))
                   (log-op "JR C, #x~x" offset)
                   (if (register-flag-carry reg)
                       (progn
                         (incf (register-pc reg) offset)
                         0)
                       2)))
                ((#xa _)
                 (multiple-value-bind (name val)
                     (select-register (logand #x7 op-ls4)
                                      (b c d e h l (hl) a))
                   (if (zerop (logand #x8 op-ls4))
                       (progn
                         (log-op "AND ~a" name))
                       (progn
                         (log-op "XOR ~a" name)
                         (let ((result (logxor (register-a reg) val)))
                           (setf (register-a reg) result)
                           (set-flags :zero (zerop result) :sub nil :hc nil :carry nil))))
                   1))
                ((#xb _)
                 (multiple-value-bind (name val)
                     (select-register (logand #x7 op-ls4)
                                      (b c d e h l (hl) a))
                   (if (zerop (logand #x8 op-ls4))
                       (progn
                         (log-op "OR ~a" name))
                       (progn
                         (log-op "CP ~a" name)
                         (let ((result (- (register-a reg) val)))
                           (set-flags :zero (zerop result)
                                      :sub t
                                      :hc (> result #x0f)
                                      :carry (minusp result)))))
                   1))
                ((_ #x1)
                 (let ((d16 (8bit->16bit (operand-1) (operand-2))))
                   (let ((name (set-register op-ms4 (bc de hl sp) d16)))
                     (log-op "LD ~a, #x~x" name d16)
                     3)))
                ((_ #xc)
                 (if (<= op-ms4 #x3)
                     (multiple-value-bind (name val)
                         (select-register op-ms4 (c e l a))
                       (log-op "INC ~a" name)
                       (let ((result (1+ val)))
                         (set-register op-ms4 (c e l a) result)
                         (set-flags :zero (> result #xff)
                                    :sub nil
                                    :hc (> result #x0f)))
                       1)
                     (error "not implemented")))
                ((_ #x6)
                 (if (<= op-ms4 #x3)
                     (error "not implemented")
                     (ecase op-ms4
                       (#xd (log-op "SUB #x~x" (operand-1))
                            (let ((result (- (register-a reg) (operand-1))))
                              (set-flags :zero (zerop result)
                                         :sub 1
                                         :hc (> result #x0f)
                                         :carry (minusp result))
                              (setf (register-a reg) result)))))
                 2)
                ((_ #x9)
                 (if (<= op-ms4 #x3)
                     (multiple-value-bind (name val)
                         (select-register op-ms4 (bc de hl sp))
                       (log-op "ADD HL, ~a" name)
                       (let ((result (+ (register-hl reg) val)))
                         (setf (register-a reg) result)
                         (set-flags :zero (zerop result)
                                    :sub nil
                                    :hc (> result #x0f)
                                    :carry (> result #xff)))
                       1)
                     ;; not implemented
                     0))
                ((_ #xe)
                 (if (<= op-ms4 #x3)
                     (let ((name (set-register op-ms4 (c e l a) (operand-1))))
                       (log-op "LD ~a, #x~x" name (operand-1))
                       2)
                     ;; #xCE ~ #xFE
                     0
                 )))

              ;; (case opcode
              ;;   (#x06 (log-op "LD B, #x~x" (operand-1))
              ;;         (setf (register-b reg) (operand-1))
              ;;         2)
              ;;   (#x18 (let ((offset (i8-as-integer (operand-1))))
              ;;           (log-op "JR #x~x" offset)
              ;;           (incf (register-pc reg) offset)
              ;;           0))
              ;;   (#x20 (let ((offset (i8-as-integer (operand-1))))
              ;;           (log-op "JR NZ, #x~x" offset)
              ;;           (if (not (register-flag-zero reg))
              ;;               (progn
              ;;                 (incf (register-pc reg) offset)
              ;;                 0)
              ;;               2)))
              ;;   (#x28 (let ((offset (i8-as-integer (operand-1))))
              ;;           (log-op "JR Z, #x~x" offset)
              ;;           (if (register-flag-zero reg)
              ;;               (progn
              ;;                 (incf (register-pc reg) offset)
              ;;                 0)
              ;;               2)))
              ;;   (#x26 (log-op "LD H, #x~x" (operand-1))
              ;;         (setf (register-h reg) (operand-1))
              ;;         2)
              ;;   (#x2a (log-op "LDI A, (HL)")
              ;;         (setf (register-a reg)
              ;;               (memory-address mem (register-hl reg)))
              ;;         (incf (register-hl reg))
              ;;         1)
              ;;   (#x40 (log-op "LD B, B")
              ;;         ;; equivalent to NOP...?
              ;;         (setf (register-b reg) (register-b reg))
              ;;         1)
              ;;   (#x44 (log-op "LD B, H (#x~x)" (register-h reg))
              ;;         (setf (register-b reg) (register-h reg))
              ;;         1)
              ;;   (#x47 (log-op "LD B, A (#x~x)" (register-a reg))
              ;;         (setf (register-b reg) (register-a reg))
              ;;         1)
              ;;   (#x4a (log-op "LD C, (HL)")
              ;;         (setf (register-c reg) (memory-address mem (register-hl reg)))
              ;;         1)
              ;;   (#x4f (log-op "LD C, A")
              ;;         (setf (register-c reg) (register-a reg))
              ;;         1)
              ;;   (#x6f (log-op "LD L, A")
              ;;         (setf (register-l reg) (register-a reg))
              ;;         1)
              ;;   (#x70 (log-op "LD (HL), B")
              ;;         (setf (memory-address mem (register-hl reg))
              ;;               (register-b reg))
              ;;         1)
              ;;   (#x7d (log-op "LD A, L")
              ;;         (setf (register-a reg) (register-l reg))
              ;;         1)
              ;;   (#x91 (log-op "SUB C (A=#x~x, C=#x~x)" (register-a reg) (register-c reg))
              ;;         (let ((result (incf (register-a reg) (register-c reg))))
              ;;           (setf (register-a reg) result)
              ;;           (set-flags :zero (zerop result)
              ;;                      :sub t
              ;;                      :hc (> result #x0f)
              ;;                      :carry (minusp result)))
              ;;         1)
              ;;   (#xaf (log-op "XOR A")
              ;;         (setf (register-a reg) 0)
              ;;         (set-flags :zero t :sub nil :hc nil :carry nil)
              ;;         1)
              ;;   (#xc5 (log-op "PUSH BC")
              ;;         (incf (register-sp reg) 2)
              ;;         (let ((sp (register-sp reg)))
              ;;           (setf (memory-address mem sp) (register-b reg)
              ;;                 (memory-address mem (1+ sp)) (register-c reg)))
              ;;         1)
              ;;   (#xff (log-op "RST 38H")
              ;;         (let ((addr (8bit->16bit (memory-address mem #x0038)
              ;;                                  (memory-address mem (1+ #x0038)))))
              ;;           (op-call reg mem addr))
              ;;         0)
              ;;   (t (error "unknown instruction: #x~x as pc = #x~x"
              ;;             opcode (register-pc (gameboy-register gb)))))
))
        (incf (register-pc reg) pc-diff)))))

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

(defun 8bit->16bit (lsb msb)
  (logior (ash msb 8) lsb))

(defun i8-as-integer (byte)
  (if (zerop (logand #x80 byte))
      byte
      (1+ (- #xff byte))))
