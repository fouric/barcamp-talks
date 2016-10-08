(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (declaim (optimize (debug 3)))
    (ql:quickload :sdl2)
    (ql:quickload :cl-opengl)))

(defun read-file (filename)
  (with-open-file (in filename
		      :direction :input
		      :if-exists :supersede)
    (with-standard-io-syntax
      (read in))))

(defun make-shader-from-file (type filename)
  (let ((source (read-file filename))
	(shader-id (gl:create-shader type)))
    (gl:shader-source shader-id source)
    (gl:compile-shader shader-id)
    (unless (gl:get-shader shader-id :compile-status)
      (format t "shader info log for type ~s: ~s~%shader: ~s~%~%" type (gl:get-shader-info-log shader-id) source)
      (exit))
    shader-id))

(defparameter *lisp-vertex-data* (list 0.75 0.75 0.0 1.0
				       0.75 -0.75 0.0 1.0
				       -0.75 -0.75 0.0 1.0))
(defparameter *vertex-data* (make-array (length *lisp-vertex-data*) :initial-contents *lisp-vertex-data*))

(defun make-gl-array (lisp-array type)
  (let ((gl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun initialize-buffer (target data type)
  (let* ((buffer (first (gl:gen-buffers 1)))
	 (array (make-gl-array data type)))
    (gl:bind-buffer target buffer)
    (gl:buffer-data target :static-draw array)
    (gl:bind-buffer target 0)
    (gl:free-gl-array array)
    buffer))

(defun opengl-1 ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :w 800 :h 600 :flags '(:opengl :resizable))
      (sdl2:with-gl-context (context window)
	(let ((program (gl:create-program)))
	  
	  ;; set up shader program
	  (let ((vertex-shader (make-shader-from-file :vertex-shader "vertex-shader-1.txt"))
		(fragment-shader (make-shader-from-file :fragment-shader "fragment-shader-1.txt")))
	    (gl:attach-shader program vertex-shader)
	    (gl:attach-shader program fragment-shader)
	    (gl:link-program program)
	    (unless (gl:get-program program :link-status)
	      (format t "program info log: ~s~%" (gl:get-program-info-log program))
	      (exit))
	    (gl:delete-shader vertex-shader)
	    (gl:delete-shader fragment-shader))
	  
	  (let* ((vertex-buffer (initialize-buffer :array-buffer *vertex-data* :float))
		 ;; get attribute indices
		 (position-index (gl:get-attrib-location program "position"))
		 (vao (gl:gen-vertex-array)))
	    (gl:bind-vertex-array vao)
		    
	    ;;(format t "OpenGL version: ~s~%" (gl:get-string :version))
	    ;; OpenGL version: "3.0 Mesa 11.2.0"
	    (sdl2:gl-make-current window context)
	    (gl:viewport 0 0 800 600)

	    (sdl2:with-event-loop (:method :poll)
	      (:keyup (:keysym keysym)
		      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
			(sdl2:push-event :quit)))
	      
	      (:idle ()
		     (gl:clear-color 0.0 0.0 0.0 0.0)
		     (gl:clear :color-buffer)

		     (gl:use-program program)

		     (gl:bind-buffer :array-buffer vertex-buffer)
		     (gl:enable-vertex-attrib-array position-index)
		     (gl:vertex-attrib-pointer 0 4 :float nil 0 0)

		     (gl:draw-arrays :triangles 0 3)

		     (gl:disable-vertex-attrib-array position-index)
		     (gl:use-program 0)

		     (sdl2:gl-swap-window window))
	      (:quit () t))))))))
