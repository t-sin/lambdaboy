; (ql:quickload '("cl-opengl" "cl-glfw3" "varjo"))

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
