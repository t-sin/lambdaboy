; (ql:quickload '("cl-opengl" "cl-glfw3" "varjo"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-normal-args (args)
    (loop
      :for arg :in args
      :until (eq arg '&uniform)
      :collect arg))

  (defun collect-uniforms (args)
    (loop
      :with uniform? := nil
      :for arg :in args
      :if uniform?
      :collect arg
      :do (when (eq arg '&uniform)
            (setf uniform? t))))

  (defmacro defshader (kind (&body args) &body body)
    (let ((input (collect-normal-args args))
          (uniforms (collect-uniforms args)))
      `(varjo:make-stage ,kind ',input ',uniforms '(:460) ',`(,@body))))

  (defmacro defvertex ((&body args) &body body)
    `(defshader :vertex (,@args) ,@body))

  (defmacro deffragment ((&body args) &body body)
    `(defshader :fragment (,@args) ,@body)))

(defun setup ()
  '(:title "test window"
    :width 800
    :height 600
    :cx 400 :cy 300 :th 0 :r 100))

(defun update (state)
  (incf (getf state :th) 0.04))

(defun draw (state)
  (gl:clear :color-buffer)
  (gl:color 1 0 0)
  (let ((cx (getf state :cx))
        (cy (getf state :cy))
        (th (getf state :th))
        (r (getf state :r)))
    (gl:rect (+ cx (* r (cos th)))
             (+ cy (* r (sin th)))
             (+ cx (* r (cos th)) 200)
             (+ cy (* r (sin th)) 200))))

(defun start-window ()
  (let ((state (setup)))
    (glfw:with-init-window (:title (getf state :title)
                            :width (getf state :width)
                            :height (getf state :height)
                            :resizable nil)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
      (gl:viewport 0 0 (getf state :width) (getf state :height))
      (gl:ortho 0 (getf state :width) 0 (getf state :height) -1 1)
      (gl:clear-color 0 0 0 1)
      (loop
        :until (glfw:window-should-close-p)
        :do (update state)
        :do (draw state)
        :do (glfw:swap-buffers)
        :do (glfw:poll-events)))))
