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

(defun create-shader ()
  (let ((vs-id (gl:create-shader :vertex-shader))
        (fs-id (gl:create-shader :fragment-shader))
        (shader-str
          (varjo:glsl-code
           (varjo:rolling-translate
            (list (defvertex ((pos :vec4) (uv :vec2))
                    (values pos uv))
                  (deffragment ((uv :vec2) &uniform (texture :sampler-2d))
                    (vari:texture texture uv)))))))
    (gl:shader-source vs-id (elt shader-str 0))
    (gl:compile-shader vs-id)
    (gl:shader-source fs-id (elt shader-str 1))
    (gl:compile-shader fs-id)
    (let ((program (gl:create-program)))
      (gl:attach-shader program vs-id)
      (gl:attach-shader program fs-id)
      (gl:link-program program)
      (gl:use-program program)
      program)))

(defun load-texture ()
  (let ((tex-id (gl:gen-texture)))
    (multiple-value-bind (data len)
        (with-open-file (in "./tex.data" ; 1024x1024 raw RGB file
                            :direction :input
                            :element-type '(unsigned-byte 8))
          (let* ((length (file-length in))
                 (seq (make-array length :element-type '(unsigned-byte 8))))
            (read-sequence seq in)
            (values seq length)))
      (gl:pixel-store :unpack-alignment 1)
      (gl:bind-texture :texture-2d tex-id)
      (let ((size (sqrt (/ len 3))))
        (gl:tex-image-2d :texture-2d 0 :rgb size size 0 :rgb :unsigned-byte data))
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:bind-texture :texture-2d 0)
      tex-id)))

(defun setup ()
  '(:title "test window"
    :width 800
    :height 600
    :cx 400 :cy 300 :th 0 :r 100))

(defun make-array-buffer (vec)
  (let* ((buffer (gl:gen-buffer))
         (arr (gl:alloc-gl-array :float (length vec))))
    (gl:bind-buffer :array-buffer buffer)
    (dotimes (i (length vec))
      (setf (gl:glaref arr i) (elt vec i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)
    buffer))

(defun setup-gl (state)
  (let ((vertex-pos (vector 0.5 0.5
                            -0.5 0.5
                            -0.5 -0.5
                            0.5 -0.5)))
    (setf (getf state :vertex-pos) (make-array-buffer vertex-pos)))
  (let ((vertex-uv (vector 1.0 0.0
                           0.0 0.0
                           0.0 1.0
                           1.0 1.0)))
    (setf (getf state :vertex-uv) (make-array-buffer vertex-uv)))
  (setf (getf state :texture) (load-texture))
  (setf (getf state :shader) (create-shader))

  state)

(defun update (state)
  (incf (getf state :th) 0.04))

(defun draw (state)
  (gl:clear-color 0.2 0.2 0.2 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:clear-depth 1.0)
  (let* ((program (getf state :shader))
         (position-loc (gl:get-attrib-location program "POS"))
         (uv-loc (gl:get-attrib-location program "UV"))
         (tex-loc (gl:get-uniform-location program "TEXTURE")))
    (gl:enable-vertex-attrib-array position-loc)
    (gl:enable-vertex-attrib-array uv-loc)
    (gl:uniformi tex-loc 0)
    (gl:bind-buffer :array-buffer (getf state :vertex-pos))
    (gl:vertex-attrib-pointer position-loc 2 :float nil 0 0)
    (gl:bind-buffer :array-buffer (getf state :vertex-uv))
    (gl:vertex-attrib-pointer uv-loc 2 :float nil 0 0)
    (gl:bind-texture :texture-2d (getf state :texture))
    (gl:draw-arrays :triangle-fan 0 4)))

(defun start-window ()
  (let ((state (setup)))
    (glfw:with-init-window (:title (getf state :title)
                            :width (getf state :width)
                            :height (getf state :height)
                            :resizable nil)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
      (setf state (setup-gl state))
      (loop
        :until (glfw:window-should-close-p)
        :do (update state)
        :do (draw state)
        :do (glfw:swap-buffers)
        :do (glfw:poll-events)))))
