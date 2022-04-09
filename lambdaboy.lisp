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

(defmethod print-object ((reg register) stream)
  (format stream "#(REGISTER :AF ~4,'0x :BC ~4,'0x :DE ~4,'0x :HL ~4,'0x :SP ~4,'0x :PC ~4,'0x)"
          (register-af reg)
          (register-bc reg)
          (register-de reg)
          (register-hl reg)
          (register-sp reg)
          (register-pc reg)))

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

(defmethod print-object ((object range) stream)
  (format stream "#(RANGE :START #x~4,'0x :END #x~4,'0x"
          (range-start object) (range-end object)))

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
  (name :block :type keyword)
  (description "NIL" :type string)
  (range nil :type range)
  (array nil :type (array (unsigned-byte 8)))
  (write-hook nil :type function))

(defmethod print-object ((object memory-block) stream)
  (format stream "#(MEMORY-BLOCK ")
  (format stream ":NAME ~s :DESCRIPTION ~s :RANGE ~s :WRITE-HOOK ~s"
          (memory-block-name object)
          (memory-block-description object)
          (memory-block-range object)
          (memory-block-write-hook object))
  (format stream ":ARRAY #(")
  (loop
    :with first-byte := t
    :with count := 0
    :for b :across (memory-block-array object)
    :do (if first-byte
            (setf first-byte nil)
            (write-char #\space stream))
    :do (format stream "#x~2,'0x" b)
    :do (cond ((= count 7) (write-char #\space stream))
              ((= count 15) (terpri stream)))
    :do (setf count (rem (1+ count) 16)))
  (format stream "))"))

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
  (defun map-memory* (mem s e name desc &optional (fn #'do-nothing))
    (let ((block (make-memory-block :name name
                                    :description desc
                                    :range (make-range :start s :end e)
                                    :array (make-array (1+ (- e s)) :element-type '(unsigned-byte 8))
                                    :write-hook fn)))
      (map-memory mem block))))

(defun find-block (mem addr)
  (loop
    :for b :across (memory-map mem)
    :do (when (in-range-p (memory-block-range b) addr)
          (return-from find-block b))))

(defun list-block (mem)
  (loop
    :for block :across (memory-map mem)
    :collect block))

(defun memory-address (mem addr)
  (let ((b (find-block mem addr)))
    (when b
      (aref (memory-block-array b)
            (- addr (range-start (memory-block-range b)))))))

(defun (setf memory-address) (val mem addr)
  (let ((b (find-block mem addr)))
    (when b
      (funcall (memory-block-write-hook b) addr val)
      (let ((offset (- addr (range-start (memory-block-range b)))))
        (when (memory-block-write-hook b)
          (funcall (memory-block-write-hook b) offset val))
        (setf (aref (memory-block-array b) offset) val)))))

;; make memory and initialize memory map according to this document:
;; - https://gbdev.io/pandocs/Memory_Map.html
(defun make-memory ()
  (let ((mem (make-memory*)))
    ;; ROM cartridge bank 00 (16KiB). its usually fixed bank.
    (map-memory* mem #x0000 #x3fff :cartridge-1 "ROM cartridge bank")
    ;; ROM cartridge bank 01 ~ nn (16KiB). its switchable bank.
    (map-memory* mem #x4000 #x7fff :cartridge-2 "ROM cartridge bank 01 ~ nn")
    ;; Video RAM (8KiB).
    (map-memory* mem #x8000 #x9fff :video-ram "Video RAM")
    ;; External RAM (8KiB).
    (map-memory* mem #xa000 #xbfff :extra-ram "External RAM")
    ;; 4KiB Work RAM.
    (map-memory* mem #xc000 #xcfff :work-ram-1 "Work RAM 1")
    ;; 4KiB Work RAM 2.
    (map-memory* mem #xd000 #xdfff :work-ram-2 "Work RAM 2")
    ;;              (lambda (addr val)
    ;;                (when (= addr #xd803)
    ;;                  (format t "write value ~2,'0x (~c) at addr ~4,'0x ~%"
    ;;                          val (code-char val) addr))))
    ;; [Prohibited] Echo RAM: Mirror of #xc000 ~ #xdfff.
    (map-memory* mem #xe000 #xfdff :echo-ram "[Prohibited] Echo RAM")
    ;; Sprite attribute table.
    (map-memory* mem #xfe00 #xfe9f :sprite-attribute-table "Sprite attribute table")
    ;; [Prohibited] Not usable.
    (map-memory* mem #xfea0 #xfeff :dont-use "[Prohibited] Not usable")
    ;; IO registers
    (map-memory* mem #xff00 #xff7f :io-registers "IO registers")
    ;; High RAM
    (map-memory* mem #xff80 #xfffe :high-ram "High RAM")
    ;; Interrupt enable register
    (map-memory* mem #xffff #xffff :interrupt-enable-register "Interrupt enable register")
    mem))

;;;; debugger

(defstruct breakpoint
  (enabled t :type (member t nil))
  (addr #x0000 :type (unsigned-byte 16))
  (condition #'(lambda (gb) (declare (ignorable gb)) t) :type function)
  (hook #'(lambda (gb) (declare (ignorable gb)) t) :type function))

(defstruct debugger
  (breakpoints (make-array 0 :element-type 'breakpoint)
               :type (vector breakpoint))
  (break-p nil :type (member t nil)))

;;; gameboy

(defstruct gameboy
  (register (make-register) :type register)
  (memory (make-memory) :type memory)
  (interrupt-enabled t :type (member t nil))
  (debugger (make-debugger) :type debugger))

(defun on-breakpoint-p (gb)
  (let* ((reg (gameboy-register gb))
         (pc (register-pc reg))
         (bps (debugger-breakpoints (gameboy-debugger gb))))
    (let ((bp (find pc bps :key #'breakpoint-addr)))
      (values (and bp
                   (breakpoint-enabled bp)
                   (funcall (breakpoint-condition bp) gb))
              bp))))

(defun test-on-breakpoint (gb)
  (multiple-value-bind (on-breakpoint bp)
      (on-breakpoint-p gb)
    (when on-breakpoint
      (setf (breakpoint-enabled bp) nil)
      (setf (debugger-break-p (gameboy-debugger gb)) t)
      (funcall (breakpoint-hook bp) gb))))

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

;;;; instruction

(defun set-flags (reg &key (zero nil zero-p)
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
    (register-flag-carry reg) carry))

(defun log-inst (gb opcode opstr &rest args)
  (let ((reg (gameboy-register gb)))
    (vom:debug "[PC:#x~4,'0x, SP:#x~4,'0x] op ~2,'0x: ~a"
               (register-pc reg) (register-sp reg)
               opcode (apply #'format nil opstr args))
    (vom:debug1 "with ~s" reg)))


(defun inst-nop (gb opcode)
  (log-inst gb opcode "NOP")
  0)

(defun inst-jump (gb opcode operand1 cond &optional operand2)
  (let ((reg (gameboy-register gb)))
    (if operand2
        (let ((a16 (8bit->16bit operand1 operand2)))
          (log-inst gb opcode "JP #x~x" a16)
          (setf (register-pc reg) a16)
          0)
        (ecase cond
          ((nil)
           (let ((offset (i8-as-integer operand1)))
             (log-inst gb opcode "JR #x~x" offset)
             (incf (register-pc reg) offset))
           0)
          (:non-zero
           (let ((offset (i8-as-integer operand1)))
             (log-inst gb opcode "JR NZ, #x~x" offset)
             (if (not (register-flag-zero reg))
                 (progn
                   (incf (register-pc reg) offset)
                   0)
                 1)))
          (:zero
           (let ((offset (i8-as-integer operand1)))
             (log-inst gb opcode "JR Z, #x~x" offset)
             (if (register-flag-zero reg)
                 (progn
                   (incf (register-pc reg) offset)
                   0)
                 1)))
          (:carry
           (let ((offset (i8-as-integer operand1)))
             (log-inst gb opcode "JR C, #x~x" offset)
             (if (register-flag-carry reg)
                 (progn
                   (incf (register-pc reg) offset)
                   0)
                 1)))))))

(defun inst-call (reg mem addr)
  (when (or (= addr #x3ecc) (= addr #xcc3e))
    (print "std_print!!!"))
  (let ((sp (incf (register-sp reg) -2))
        (pc (1+ (register-pc reg))))
    (setf (memory-address mem sp) (logand pc #x00ff)
          (memory-address mem (1+ sp)) (ash (logand pc #xff00) -8)))
  (setf (register-pc reg) addr)
  0)

(defun inst-ret (gb opcode cond)
  (let ((reg (gameboy-register gb))
        (mem (gameboy-memory gb)))
    (ecase cond
      ((nil)
       (log-inst gb opcode "RET")
       (let* ((addr (memory-address mem (register-sp reg)))
              (addr (logior addr
                            (ash (memory-address mem (1+ (register-sp reg))) 8))))
         (incf (register-sp reg) 2)
         (setf (register-pc reg) addr))
       0)
      (:non-zero
       (log-inst gb opcode "RET NZ")
       (if (not (register-flag-zero reg))
           (let* ((addr (memory-address mem (register-sp reg)))
                  (addr (logior addr
                                (ash (memory-address mem (1+ (register-sp reg))) 8))))
             (incf (register-sp reg) 2)
             (setf (register-pc reg) addr)
             0)
           1))
      (:zero
       (log-inst gb opcode "RET Z")
       (if (register-flag-zero reg)
           (let* ((addr (memory-address mem (register-sp reg)))
                  (addr (logior addr
                                (ash (memory-address mem (1+ (register-sp reg))) 8))))
             (incf (register-sp reg) 2)
             (setf (register-pc reg) addr)
             0)
           1)))))

(defun inst-set-bit (gb opcode nth-base reset-p)
  (let* ((mem (gameboy-memory gb))
         (reg (gameboy-register gb))
         (opcode-ls4 (logand opcode #x0f))
         (nth (+ (if (zerop (logand #x8 opcode-ls4))
                     0
                     1)
                 nth-base))
         (reg-id (logand #x7 opcode-ls4)))
    (multiple-value-bind (name val)
        (select-register reg-id (b c d e h l (hl) a))
      (if reset-p
          (log-inst gb opcode "RES ~a, ~a" nth name)
          (log-inst gb opcode "SET ~a, ~a" nth name))
      (let ((mask (ecase nth
                    (0 #x01)
                    (1 #x02)
                    (2 #x04)
                    (3 #x08)
                    (4 #x10)
                    (5 #x20)
                    (6 #x40)
                    (7 #x80))))
        (set-register reg-id (b c d e h l (hl) a)
                      (if reset-p
                          (logior val mask)
                          (logand val (lognot mask)))))))
  1)

(defun inst-pop (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (reg-idx (- (ash opcode -4) #xc))
         (sp (register-sp reg))
         (d16 (8bit->16bit (memory-address mem (1+ sp)) (memory-address mem sp))))
    (multiple-value-bind (name _)
        (select-register reg-idx (bc de hl af))
      (declare (ignore _))
      (log-inst gb opcode "POP ~a" name))
    (set-register reg-idx (bc de hl af) d16)
    (incf (register-sp reg) 2))
  1)

(defun inst-inc-dec (gb opcode reg-name inc-p 16bit-p)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (val (if (listp reg-name)
                  (memory-address mem (slot-value reg (first reg-name)))
                  (slot-value reg reg-name))))
    (log-inst gb opcode (if inc-p "INC ~a" "DEC ~a") reg-name)
    (let ((result (if inc-p (1+ val) (1- val))))
      (if (listp reg-name)
          (setf (memory-address mem (slot-value reg (first reg-name)))
                (rem result (if 16bit-p #x10000 #x100)))
          (setf (slot-value reg reg-name) (rem result (if 16bit-p #x10000 #x100))))
      (unless 16bit-p
        (if inc-p
            (set-flags reg :zero (> result #xff)
                       :sub nil
                       :hc (> result #x0f))
            (set-flags reg :zero (zerop result)
                       :sub t
                       :hc (< result #x0f)
                       :carry nil)))))
  1)

(defun inst-add (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (op-ls3 (logand #x7 opcode)))
    (multiple-value-bind (name val)
        (select-register op-ls3
                         (b c d e h l (hl) a))
      (log-inst gb opcode "ADD A, ~a" name)
      (let ((result (+ (register-a reg) val)))
        (setf (register-a reg) result)
        (set-flags reg :zero (zerop result)
                   :sub nil
                   :hc (> result #xf))
        :carry (> result #xff))))
  1)

(defun inst-adc (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (op-ls3 (logand #x7 opcode)))
    (multiple-value-bind (name val)
        (select-register op-ls3
                         (b c d e h l (hl) a))
      (log-inst gb opcode "ADC A, ~a" name)
      (let ((result (+ (register-a reg) val
                       (if (register-flag-carry reg) 1 0))))
        (setf (register-a reg) result)
        (set-flags reg :zero (zerop result)
                   :sub nil
                   :hc (> result #xf))
        :carry (> result #xff))))
  1)

(defun inst-and (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (op-ls3 (logand #x7 opcode)))
    (multiple-value-bind (name val)
        (select-register op-ls3
                         (b c d e h l (hl) a))
      (log-inst gb opcode "AND ~a" name)
      (let ((result (logand (register-a reg) val)))
        (setf (register-a reg) result)
        (set-flags reg :zero (zerop result) :sub nil :hc t :carry nil))))
  1)

(defun inst-xor (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (op-ls3 (logand #x7 opcode)))
    (multiple-value-bind (name val)
        (select-register op-ls3
                         (b c d e h l (hl) a))
      (progn
        (log-inst gb opcode "XOR ~a" name)
        (let ((result (logxor (register-a reg) val)))
          (setf (register-a reg) result)
          (set-flags reg :zero (zerop result) :sub nil :hc nil :carry nil)))))
  1)

(defun inst-sub (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (op-ls3 (logand #x7 opcode)))
    (multiple-value-bind (name val)
        (select-register op-ls3
                         (b c d e h l (hl) a))
      (log-inst gb opcode "SUB A, ~a" name)
      (let ((result (- (register-a reg) val)))
        (setf (register-a reg) result)
        (set-flags reg :zero (zerop result)
                   :sub t
                   :hc (< result #xf)
                   :carry (minusp result)))))
  1)

(defun inst-sbc (gb opcode)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (op-ls3 (logand #x7 opcode)))
    (multiple-value-bind (name val)
        (select-register op-ls3
                         (b c d e h l (hl) a))
      (log-inst gb opcode "SBC A, ~a" name)
      (let ((result (- (register-a reg) val
                       (if (register-flag-carry reg) 1 0))))
        (setf (register-a reg) result)
        (set-flags reg :zero (zerop result)
                   :sub t
                   :hc (< result #xf)
                   :carry (minusp result)))))
  1)

;;;; execution

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

(defmacro unknown-instruction ()
  `(error "unknown instruction: #x~x as pc = #x~x"
          opcode (register-pc (gameboy-register gb))))

;; decode and execute one instruction
;;
;; cf. - https://gbdev.io/pandocs/CPU_Instruction_Set.html
;;     - https://gbdev.io/gb-opcodes/optables/
(defun execute-1 (gb)
  (setf (debugger-break-p (gameboy-debugger gb)) nil)
  (let* ((reg (gameboy-register gb))
         (mem (gameboy-memory gb))
         (pc (register-pc reg))
         (opcode (memory-address mem pc))
         (op-ms4 (ash (logand opcode #xf0) -4))
         (op-ls4 (logand opcode #x0f)))
    (incf (register-pc reg))

    (when (test-on-breakpoint gb)
      (incf (register-pc reg) -1)
      (return-from execute-1))

    (flet ((operand-1 ()
             (memory-address mem (+ (register-pc reg) 0)))
           (operand-2 ()
             (memory-address mem (+ (register-pc reg) 1))))
      (let ((pc-diff
              (4bit-case (op-ms4 op-ls4)
                ((#x0 #x0) (inst-nop gb opcode))
                ((#x1 #x8) (inst-jump gb opcode (operand-1) nil))
                ((#x2 #x0) (inst-jump gb opcode (operand-1) :non-zero))
                ((#x2 #x8) (inst-jump gb opcode (operand-1) :zero))
                ((#xc #x3) (inst-jump gb opcode (operand-1) nil (operand-2)))
                ((#xc #x8) (inst-ret gb opcode :zero))
                ((#xc #x9) (inst-ret gb opcode nil))
                ((#xc #xb)
                 (let* ((pc (incf (register-pc reg)))
                        (opcode (memory-address mem pc))
                        (op-ms4 (ash (logand opcode #xf0) -4))
                        (op-ls4 (logand opcode #x0f)))
                     (4bit-case (op-ms4 op-ls4)
                       ((#x3 _)
                        (let ((idx (logand op-ls4 #x7)))
                          (multiple-value-bind (name val)
                              (select-register idx (b c d e h l (hl) a))
                            (if (zerop (logand op-ls4 #x8))
                                (let ((result (logior (ash (logand val #xf) 8)
                                                      (ash val -8))))
                                  (log-inst gb opcode "SWAP ~a" name)
                                  (set-register idx (b c d e h l (hl) a) result)
                                  (set-flags reg :zero (zerop result) :sub nil :hc nil :carry nil))
                                (let ((result (ash val -1)))
                                  (log-inst gb opcode "SRL ~a" name)
                                  (set-register idx (b c d e h l (hl) a) result)
                                  (set-flags reg :zero (zerop result) :sub nil :hc nil :carry (minusp result)))))
                          0))
                       ((#x8 _) (inst-set-bit gb opcode 0 t))
                       ((#x9 _) (inst-set-bit gb opcode 2 t))
                       ((#xa _) (inst-set-bit gb opcode 4 t))
                       ((#xb _) (inst-set-bit gb opcode 6 t))
                       ((#xc _) (inst-set-bit gb opcode 0 nil))
                       ((#xd _) (inst-set-bit gb opcode 2 nil))
                       ((#xe _) (inst-set-bit gb opcode 4 nil))
                       ((#xf _) (inst-set-bit gb opcode 6 nil)))))
                ((#xc #xd)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-inst gb opcode "CALL #x~x" a16)
                   (inst-call reg mem a16)))
                ((#xe #x0)
                 (log-inst gb opcode "LDH (#x~x), A" (operand-1))
                 (setf (memory-address mem (+ #xff00 (operand-1)))
                       (register-a reg))
                 1)
                ((#xe #xa)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-inst gb opcode "LD (#x~x), A" a16)
                   (setf (memory-address mem a16) (register-a reg))
                   2))
                ((#xf #x0)
                 (log-inst gb opcode "LDH A, (#x~x)" (operand-1))
                 (setf (register-a reg) (+ #xff00 (operand-1)))
                 1)
                ((#xf #x3)
                 (log-inst gb opcode "DI")
                 (setf (gameboy-interrupt-enabled gb) nil)
                 0)
                ((#xf #xa)
                 (let ((a16 (8bit->16bit (operand-1) (operand-2))))
                   (log-inst gb opcode "LD A, #x~x" a16)
                   (setf (register-a reg) a16))
                 2)
                ((#xf #xe)
                 (log-inst gb opcode "CP #x~x " (operand-1))
                 (let ((result (- (register-a reg) (operand-1))))
                   (set-flags reg :zero (zerop result)
                              :sub t
                              :hc (> result #x0f)
                              :carry (minusp result)))
                 1)
                ((#x3 #x8) (inst-jump gb opcode (operand-1) :carry))
                ((#x4 _)
                 (multiple-value-bind (name val)
                     (select-register (logand #x7 op-ls4)
                                      (b c d e h l (hl) a))
                   (if (zerop (logand #x8 op-ls4))
                       (progn
                         (log-inst gb opcode "LD B, ~a" name)
                         (setf (register-b reg) val))
                       (progn
                         (log-inst gb opcode "LD C, ~a" name)
                           (setf (register-c reg) val)))
                   0))
                ((#x5 _)
                 (multiple-value-bind (name val)
                     (select-register (logand #x7 op-ls4)
                                      (b c d e h l (hl) a))
                   (if (zerop (logand #x8 op-ls4))
                       (progn
                         (log-inst gb opcode "LD D, ~a" name)
                         (setf (register-d reg) val))
                       (progn
                         (log-inst gb opcode "LD E, ~a" name)
                           (setf (register-e reg) val)))
                   0))
                ((#x6 _)
                 (multiple-value-bind (name val)
                     (select-register (logand #x7 op-ls4)
                                      (b c d e h l (hl) a))
                   (if (zerop (logand #x8 op-ls4))
                       (progn
                         (log-inst gb opcode "LD H, ~a" name)
                         (setf (register-h reg) val))
                       (progn
                         (log-inst gb opcode "LD L, ~a" name)
                           (setf (register-l reg) val)))
                   0))
                ((#x7 _)
                 (if (zerop (logand #x8 op-ls4))
                     (if (= op-ls4 #x6)
                         (progn
                           (log-inst gb opcode "HALT")
                           (error "halt"))
                         (multiple-value-bind (name val)
                             (select-register (logand #x7 op-ls4)
                                              (b c d e h l (hl) a))
                           (log-inst gb opcode "LD (HL), ~a" name)
                           (setf (memory-address mem (register-hl reg)) val)))
                     (multiple-value-bind (name val)
                         (select-register (logand #x7 op-ls4)
                                          (b c d e h l (hl) a))
                       (log-inst gb opcode "LD A, ~a" name)
                           (setf (register-a reg) val)))
                 0)
                ((#x8 _)
                 (if (zerop (logand #x8 op-ls4))
                     (inst-add gb opcode)
                     (inst-adc gb opcode)))
                ((#x9 _)
                 (if (zerop (logand #x8 op-ls4))
                     (inst-sub gb opcode)
                     (inst-sbc gb opcode)))
                ((#xa _)
                 (if (zerop (logand #x8 op-ls4))
                     (inst-and gb opcode)
                     (inst-xor gb opcode)))
                ((#xb _)
                 (multiple-value-bind (name val)
                     (select-register (logand #x7 op-ls4)
                                      (b c d e h l (hl) a))
                   (if (zerop (logand #x8 op-ls4))
                       (progn
                         (log-inst gb opcode "OR ~a" name)
                         (let ((result (logior (register-a reg) val)))
                           (setf (register-a reg) result)
                           (set-flags reg :zero (zerop result) :sub nil :hc nil :carry nil)))
                       (progn
                         (log-inst gb opcode "CP ~a" name)
                         (let ((result (- (register-a reg) val)))
                           (set-flags reg :zero (zerop result)
                                      :sub t
                                      :hc (> result #x0f)
                                      :carry (minusp result)))))
                   0))
                ((_ #x0)
                 (case op-ms4
                   (#xc (inst-ret gb opcode :non-zero))
                   (t (unknown-instruction))))
                ((_ #x1)
                 (if (<= op-ms4 #x3)
                     (let ((d16 (8bit->16bit (operand-1) (operand-2))))
                       (let ((name (set-register op-ms4 (bc de hl sp) d16)))
                         (log-inst gb opcode "LD ~a, #x~x" name d16)
                         2))
                     (inst-pop gb opcode)))
                ((_ #x2)
                 (if (<= op-ms4 #x3)
                     (let ((name (set-register op-ms4 ((bc) (de) (hl) (af))
                                               (register-a reg))))
                       (log-inst gb opcode "LD ~a, A" name)
                       0)
                     (unknown-instruction)))
                ((_ #x3)
                 (if (<= op-ms4 #x3)
                     (ecase op-ms4
                       (#x0 (inst-inc-dec gb opcode 'bc t t))
                       (#x1 (inst-inc-dec gb opcode 'de t t))
                       (#x2 (inst-inc-dec gb opcode 'hl t t))
                       (#x3 (inst-inc-dec gb opcode 'sp t t)))
                     (unknown-instruction)))
                ((_ #x4)
                 (if (<= op-ms4 #x3)
                     (ecase op-ms4
                       (#x0 (inst-inc-dec gb opcode 'b t nil))
                       (#x1 (inst-inc-dec gb opcode 'd t nil))
                       (#x2 (inst-inc-dec gb opcode 'h t nil))
                       (#x3 (inst-inc-dec gb opcode '(hl) t nil)))
                     (unknown-instruction)))
                ((_ #x5)
                 (if (<= op-ms4 #x3)
                     (ecase op-ms4
                       (#x0 (inst-inc-dec gb opcode 'b nil nil))
                       (#x1 (inst-inc-dec gb opcode 'd nil nil))
                       (#x2 (inst-inc-dec gb opcode 'h nil nil))
                       (#x3 (inst-inc-dec gb opcode '(hl) nil nil)))
                     (multiple-value-bind (name val)
                         (select-register (- op-ms4 #xc) (bc de hl af))
                       (log-inst gb opcode "PUSH ~a" name)
                       (incf (register-sp reg) -2)
                      (let ((sp (register-sp reg)))
                        (setf (memory-address mem sp) (ash val -8)
                              (memory-address mem (1+ sp)) (logand val #xf)))
                      0)))
                ((_ #x6)
                 (if (<= op-ms4 #x3)
                     (let ((name (set-register op-ms4 (b d h (hl)) (operand-1))))
                       (log-inst gb opcode "LD ~a, #x~x" name (operand-1))
                       2)
                     (case op-ms4
                       (#xc (log-inst gb opcode "ADD #x~x" (operand-1))
                            (let ((result (+ (register-a reg) (operand-1))))
                              (set-flags reg  :zero (zerop result)
                                         :sub nil
                                         :hc (< result #x0f)
                                         :carry (> result #xff))
                              (setf (register-a reg) result)))
                       (#xd (log-inst gb opcode "SUB #x~x" (operand-1))
                            (let ((result (- (register-a reg) (operand-1))))
                              (set-flags reg :zero (zerop result)
                                         :sub t
                                         :hc (> result #x0f)
                                         :carry (minusp result))
                              (setf (register-a reg) result)))
                       (t (unknown-instruction))))
                 1)
                ((_ #x7)
                 (if (<= op-ms4 #x3)
                     (unknown-instruction)
                     (let ((addr (ecase op-ms4
                                   (#xc #x00)
                                   (#xd #x10)
                                   (#xe #x20)
                                   (#xf #x30))))
                       (log-inst gb opcode "RST #x~2,'0x" addr)
                       (inst-call reg mem addr)
                       0)))
                ((_ #x9)
                 (if (<= op-ms4 #x3)
                     (multiple-value-bind (name val)
                         (select-register op-ms4 (bc de hl sp))
                       (log-inst gb opcode "ADD HL, ~a" name)
                       (let ((result (+ (register-hl reg) val)))
                         (setf (register-a reg) result)
                         (set-flags reg :zero (zerop result)
                                    :sub nil
                                    :hc (> result #x0f)
                                    :carry (> result #xff)))
                       0)
                     (case op-ms4
                       (#xf (log-inst gb opcode "LD SP, HL")
                            (setf (register-sp reg) (register-hl reg))
                            0)
                       (t (unknown-instruction)))))
                ((_ #xa)
                 (if (<= op-ms4 #x3)
                     (multiple-value-bind (name val)
                         (select-register op-ms4 ((bc) (de) (hl) (hl)))
                       (cond ((= op-ms4 #x2)
                              (log-inst gb opcode "LD A, (HL+)")
                              (incf (register-hl reg)))
                             ((= op-ms4 #x3)
                              (log-inst gb opcode "LD A, (HL-)")
                              (incf (register-hl reg) -1))
                             (t (log-inst gb opcode "LD A, ~a" name)))
                       (setf (register-a reg) val)
                       0)
                     (unknown-instruction)))
                ((_ #xb)
                 (if (<= op-ms4 #x3)
                     (ecase op-ms4
                       (#x0 (inst-inc-dec gb opcode 'bc nil t))
                       (#x1 (inst-inc-dec gb opcode 'de nil t))
                       (#x2 (inst-inc-dec gb opcode 'hl nil t))
                       (#x3 (inst-inc-dec gb opcode 'sp nil t)))
                     (unknown-instruction)))
                ((_ #xc)
                 (if (<= op-ms4 #x3)
                     (ecase op-ms4
                       (#x0 (inst-inc-dec gb opcode 'c t nil))
                       (#x1 (inst-inc-dec gb opcode 'e t nil))
                       (#x2 (inst-inc-dec gb opcode 'l t nil))
                       (#x3 (inst-inc-dec gb opcode 'a t nil)))
                     (unknown-instruction)))
                ((_ #xd)
                 (if (<= op-ms4 #x3)
                     (ecase op-ms4
                       (#x0 (inst-inc-dec gb opcode 'c nil nil))
                       (#x1 (inst-inc-dec gb opcode 'e nil nil))
                       (#x2 (inst-inc-dec gb opcode 'l nil nil))
                       (#x3 (inst-inc-dec gb opcode 'a nil nil)))
                     (unknown-instruction)))
                ((_ #xe)
                 (if (<= op-ms4 #x3)
                     (let ((name (set-register op-ms4 (c e l a) (operand-1))))
                       (log-inst gb opcode "LD ~a, #x~x" name (operand-1))
                       1)
                     ;; #xCE ~ #xFE
                     (unknown-instruction)
                 )))))
        (incf (register-pc reg) pc-diff)))))

(defun run (gb)
  (loop
    :do (execute-1 gb)
    :when (debugger-break-p (gameboy-debugger gb))
    :do (return-from run)))

(defparameter *gameboy* nil)
(defparameter *rom* (make-array #x8000 :element-type '(unsigned-byte 8)))

(defun start-gb (pathname &optional gb)
  (unless gb
    (setf *gameboy* (make-gameboy)))
  (with-open-file (in pathname :direction :input
                      :element-type '(unsigned-byte 8))
    (read-sequence *rom* in))
  (load-rom *gameboy* *rom*)
  (initialize-gameboy *gameboy*)
  (run *gameboy*))

;;;; util

(defun 8bit->16bit (lsb msb)
  (logior (ash msb 8) lsb))

(defun i8-as-integer (byte)
  (if (zerop (logand #x80 byte))
      byte
      (- (1+ (- #xff byte)))))

;;;; debug util
(flet ((f (gb)
         (declare (ignorable gb))
         t))
  (defun set-breakpoint (gb addr &key (condition #'f) (hook #'f))
    (let* ((bp (make-breakpoint :addr addr :condition condition :hook hook))
           (new-bps (concatenate '(vector breakpoint)
                                 (debugger-breakpoints (gameboy-debugger gb))
                                 (vector bp))))
      (setf (debugger-breakpoints (gameboy-debugger gb)) new-bps))))

(defun dump-memory (offset length)
  (loop
    :for addr :from offset :below (+ offset length)
    :with n := 1
    :for ch := (memory-address (gameboy-memory *gameboy*) addr)
    :do (format t "~2,'0x " ch)
    :do (cond ((zerop (mod n #x10)) (format t "~%") (setf n 0))
              ((zerop (mod n #x08)) (format t " ")))
    :do (incf n)))


#|
(let ((gb (make-gameboy)))
  (setf *gameboy* gb)
  (vom:config t :warn)
  (flet ((hook (gb)
           (declare (ignore gb))
           (vom:config t :debug)))
    ;; このあたりのアドレスで正常なRET後なぞのRET NZが走っている…
    ;; コード的にはここ: https://github.com/retrio/gb-test-roms/blob/c240dd7d700e5c0b00a7bbba52b53e4ee67b5f15/cpu_instrs/source/common/runtime.s#L96
    (set-breakpoint gb #xc0a2 :hook #'hook))
  (start-gb "/home/grey/opt/gb-test-roms/blargg/cpu_instrs/individual/06-ld r,r.gb" gb))
|#
