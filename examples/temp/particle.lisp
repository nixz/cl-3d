;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; particle.lisp --- 
;;;;
;;;; Copyright (c) 2011, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================
(ql:quickload "cl-opengl")
(ql:quickload "lispbuilder-sdl")

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

;; physical size of window in pixels
(defparameter *actual-screen-width* 320)
(defparameter *actual-screen-height* 200)
;; nominal size of window in pixels, in case we just want to scale the
;; scene to match the window instead of showing more of the world
(defparameter *nominal-screen-width* 320)
(defparameter *nominal-screen-height* 200)
;; extents of the window in GL coordinates
(defparameter *screen-width* nil)
(defparameter *screen-height* nil)
;; flag specifying how we want to handle chamging resolution:
;;  if T, always show a fixed amount of the world
;;  if NIL, keep 1 pixel on screen = 1 unit of world space, so more of the
;;    world shows when the window gets larger
(defparameter *use-nominal-size* t)

(defun setup-ortho-projection (width height)
  (setf *actual-screen-width* width)
  (setf *actual-screen-height* height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (if *use-nominal-size*
    (setf *screen-width* *nominal-screen-width*
          *screen-height* *nominal-screen-height*)
    (setf *screen-width* *actual-screen-width*
          *screen-height* *actual-screen-height*))
  ;; this sets up the world so the screen coordinates go from 0,0 at lower
  ;; left to width,height at upper right
  (gl:ortho 0 *screen-width*  *screen-height* 0  0 1)
  (gl:matrix-mode :modelview))

;; --------------------------------------------------------------particle-system
(defclass particle-system ()
  ())

(defgeneric draw-object (object)
  (:documentation "draw object to screen")
  (:method ((object particle-system))))

(defgeneric update-object (object delta-t)
  (:documentation "update state of object by delta-t milliseconds, return NIL if
object is no longer active and should be removed, T otherwise")
  (:method ((object particle-system) delta-t) nil))

;; -------------------------------------------------------------particle-manager
(defclass particle-manager ()
  ((systems :initform nil :accessor systems)))

(defmethod draw-object ((object particle-manager))
  (mapc 'draw-object (systems object)))

(defmethod update-object ((object particle-manager) delta-t)
  (setf (systems object)
        (delete-if-not (lambda (x) (update-object x delta-t))
                       (systems object))))

;; ---------------------------------------------------------------managed-texture
(defclass managed-texture ()
  ((name :initform nil :initarg :name :accessor name)
   (target :initform :texture-2d :accessor target)))

(defgeneric create-texture (texture)
  (:documentation "Create the GL texture name, and upload the texture data"))
(defgeneric bind-texture (texture)
  (:documentation "Bind the texture"))
(defgeneric delete-texture (texture)
  (:documentation "Release the GL texture name, if previously allocated"))

(defmethod bind-texture ((texture managed-texture))
  (gl:bind-texture (target texture) (name texture)))

(defmethod delete-texture ((texture managed-texture))
  (when (name texture)
    (let ((name (name texture)))
      (setf (name texture) nil)
      ;; newer versions of cl-opengl can automatically check for
      ;; errors, but this function might legitimately be called
      ;; after GL is shut down, so ignore that error if applicable
      (handler-case
          (gl:delete-textures (list name))
        ;; (putting :cl-opengl-checks-errors on *features* was added
        ;;  after the error checks, so if you get errors anyway, try
        ;;  updating cl-opengl)
        #+cl-opengl-checks-errors(%gl::opengl-error (c) (values nil c))))))

;; -----------------------------------------------------------------file-texture
(defclass file-texture (managed-texture)
  ((filename :initarg filename :accessor filename)))

(defmethod create-texture ((texture file-texture))
  ;; these could reuse the name if we were sure it was still valid,
  ;; but just to be safe we recreate it
  (when (name texture)
    (gl:delete-textures (list (name texture))))
  (setf (name texture) (load-a-texture (filename texture))))

;; --------------------------------------------------------------sequence-texure
(defclass sequence-texture (managed-texture)
  ;; we keep a copy of the image data so we can recreate the texture
  ;; if needed, for example after recreating the main window
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (image-data :initarg :image-data :accessor image-data)))

(defmethod create-texture ((texture sequence-texture))
  (when (name texture)
    (gl:delete-textures (list (name texture))))
  (let ((name (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d name)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-image-2d :texture-2d 0 :rgba (width texture) (height texture)
                     0 :rgba :unsigned-byte
                     (image-data texture))
    (setf (name texture) name)) )

;; --------------------------------------------------------------texture-manager
(defclass texture-manager ()
  ((id-map :initform (make-hash-table) :accessor id-map)))

(defgeneric reload-textures (manager)
  (:documentation
   "(re)create GL data for all textures managed by this texture manager"))

(defgeneric unload-textures (manager)
  (:documentation
   "release any GL resources associated with textures managed by this texture manager"))

(defgeneric get-texture-by-id (manager id)
  (:documentation
   "return the texture corresponding to the (EQL comparable) ID"))

(defgeneric (setf get-texture-by-id) (texture manager id)
  (:documentation
   "add or replace the texture corresponding tothe specified ID"))

(defmethod reload-textures ((manager texture-manager))
  (loop for i being the hash-values of (id-map manager)
        do (create-texture i)))

(defmethod unload-textures ((manager texture-manager))
  (loop for i being the hash-values of (id-map manager)
        do (delete-texture i)))

(defmethod get-texture-by-id ((manager texture-manager) id)
  (gethash id (id-map manager)))

(defmethod (setf get-texture-by-id) (texture (manager texture-manager) id)
  (let ((old-texture (get-texture-by-id manager id)))
    ;; if we had a texture with that ID already, and it wasn't listed
    ;; under any other ID, unload it before replacing it
    (when (and old-texture (not (eq texture old-texture))
               (loop for v being the hash-values of (id-map manager)
                  never (eq old-texture v)))
      (delete-texture old-texture)))
  (setf (gethash id (id-map manager)) texture))

;; -----------------------------------------------------------*texture-manager*
(defparameter *texture-manager* nil)

(defmethod bind-texture ((texture symbol))
  (let ((tex (get-texture-by-id *texture-manager* texture)))
    (when (and tex (not (symbolp tex)))
      (bind-texture tex))))

(defmethod create-texture ((texture symbol))
  (let ((tex (get-texture-by-id *texture-manager* texture)))
    (when (and tex (not (symbolp tex)))
      (create-texture tex))))

(defmethod delete-texture ((texture symbol))
  (let ((tex (get-texture-by-id *texture-manager* texture)))
    (when (and tex (not (symbolp tex)))
      (delete-texture tex))))


;; ---------------------------------------------------------with-texture-manager
(defmacro with-texture-manager (&body body)
  (let ((manager (gensym "MANAGER-")))
    `(let* ((,manager (make-instance 'texture-manager))
            (*texture-manager* ,manager))
       (unwind-protect
            (progn
              ,@body)
         (unload-textures ,manager)))))

(defun file-texture (id filename)
  (setf (get-texture-by-id *texture-manager* id)
        (make-instance 'file-texture :filename filename)))

(ql:quickload "vecto")
(defmacro vecto-texture ((id width height) &body body)
  `(vecto:with-canvas (:width ,width :height ,height)
     ;; run some vecto code
     ,@body
     ;; and create a texture with (a copy of) the results
     (setf (get-texture-by-id *texture-manager* ,id)
           (make-instance 'sequence-texture :width ,width :height ,height
                          :image-data (copy-seq (vecto::image-data vecto::*graphics-state*))))))


;; -----------------------------------------------------------------interaction
;; store the mouse position somewhere easy to get to
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

(ql:quickload "bordeaux-threads")
(defvar *next-frame-hook-mutex* (bt:make-lock "frame-hook-lock"))
(defparameter *next-frame-hook* nil)

(defun key-down (key state mod-key scancode unicode)
  (declare (ignore key state mod-key scancode unicode)))
(defun key-up (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (when (eql key :sdl-key-escape) (sdl:push-quit-event)))
(defun mouse-down (button state x y)
  (declare (ignore button state x y)))
(defun mouse-up (button state x y)
  (declare (ignore button state x y)))
(defun mouse-move (x y delta-x delta-y)
  (declare (ignore x y delta-x delta-y)))

(defun init ()
  ;; nothing here yet, we'll build it incrementally this time
  )

(defun draw ()
  (gl:clear :color-buffer-bit)
  ;; we'll update this later too
  (sdl:update-display))

(defun update (delta-t)
  (declare (ignore delta-t))
  ;; nothing here yet either
  )

;; ---------------------------------------------------------------------main-loop
(defun main-loop ()
  (sdl:with-init ()
    (sdl:window *nominal-screen-width* *nominal-screen-height*
                :flags (logior sdl:sdl-opengl
                               sdl:sdl-resizable))
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (let ((previous-tick (sdl:sdl-get-ticks)))
      (flet ((mx (x) ;; adjust mouse coordinates from screen to world
               (* x (/ (float *screen-width* 1.0) *actual-screen-width*)))
             (my (y) ;; adjust mouse coordinates from screen to world
               (* y (/ (float *screen-height* 1.0) *actual-screen-height*))))
        (with-texture-manager
          (init)
          (setup-ortho-projection *nominal-screen-width* *nominal-screen-height*)
          (sdl:with-events ()
            (:quit-event () t)
            (:video-resize-event (:w w :h h)
              (sdl:resize-window w h)
              (reload-textures *texture-manager*)
              (restartable (setup-ortho-projection w h)))
            (:key-down-event (:state state :scancode scancode :key key
                            :mod-key mod-key :unicode unicode)
              (restartable (key-down key state mod-key scancode unicode)))
            (:key-up-event (:state state :scancode scancode :key key
                            :mod-key mod-key :unicode unicode)
              (restartable (key-up key state mod-key scancode unicode)))

            (:mouse-button-up-event (:button button :state state :x x :y y)
              (restartable (mouse-up button state (mx x) (my y))))
            (:mouse-button-down-event (:button button :state state :x x :y y)
              (restartable (mouse-down button state (mx x) (my y))))
            (:mouse-motion-event (:x x :y y :x-rel delta-x :y-rel delta-y)
              (setf *mouse-x* (mx x)
                    *mouse-y* (my y))
              (restartable (mouse-move (mx x) (my y) (mx delta-x) (my delta-y))))
            (:idle ()
                   #+(and sbcl (not sb-thread))
                   (restartable
                     (sb-sys:serve-all-events 0))
                   (let ((delta-t (- (sdl:sdl-get-ticks) previous-tick)))
                     (setf previous-tick (sdl:sdl-get-ticks))
                     ;; we check for negative delta-t in case sdl's timer wraps
                     ;; after some amount of time, 0 for a frame is better
                     ;; a large negative number
                     (restartable (update (if (minusp delta-t) 0 delta-t))))
                   (restartable (draw))
                   (restartable
                     (bt:with-lock-held (*next-frame-hook-mutex*)
                       (loop for i in *next-frame-hook*
                             do (funcall i))
                       (setf *next-frame-hook* nil))))))))))

;(main-loop)                             
;; -------------------------------------------------------------------next-frame
;; and the wrapper macro for setting up something to run next frame
(defmacro next-frame (&body body)
  `(bt:with-lock-held (*next-frame-hook-mutex*)
     (progn (push (lambda () ,@body) *next-frame-hook*))))


;; change the background color
(next-frame
  (format t "got to next frame ...~%") (finish-output)
  (gl:clear-color 0 1 0 1))

;; and to put it back
(next-frame
  (gl:clear-color 0 0 0 1))


(defun init ()
  (vecto-texture (:click-particle 64 64)
    (vecto:set-rgba-fill 1 0 0 0)
    (vecto:clear-canvas)
    (vecto:set-rgba-stroke 1 0 0 0.75)
    (vecto:set-line-width 10)
    (vecto:centered-circle-path 32 32 20)
    (vecto:stroke))
  (reload-textures *texture-manager*))

;; and then run the init function in the GL thread to create the texture
(next-frame
  (init))
;; (next-frame   (bind-texture :click-particle))
;; ============================================================================
(defparameter *particle-manager* (make-instance 'particle-manager))

(defparameter *click-particles-lifetime-ms* 3000)
(defparameter *click-particles-count* 24)
(defparameter *click-particles-speed* 50) ;; pixels per second

(defclass click-particles (particle-system)
  (;; we store the particles in a flat array of N * x,y for position,
   ;; and another for velocity
   ;; *note: premature optimization, don't try this at home kids!
   (age :initform 0 :accessor age)
   (positions :accessor positions)
   (velocities :accessor velocities))
  (:default-initargs :count *click-particles-count*
    :speed *click-particles-speed*))

(defmethod initialize-instance :after ((o click-particles) &key count speed &allow-other-keys)
  (let ((positions
         (make-array (list (* 2 count)) :element-type 'single-float :initial-element 0.0))
        (velocities
         (make-array (list (* 2 count)) :element-type 'single-float :initial-element 0.0)))
    (loop
       for i below count
       for j = (* i 2)
       do (setf (aref positions (+ j 0)) (float *mouse-x* 1.0))
       do (setf (aref positions (+ j 1)) (float *mouse-y* 1.0))
       do (setf (aref velocities (+ j 0))
                (float (* speed (sin (* i (/ (* 2 pi) count)))) 1.0))
       do (setf (aref velocities (+ j 1))
                (float (* speed (cos (* i (/ (* 2 pi) count)))) 1.0)))
    (setf (positions o) positions)
    (setf (velocities o) velocities)))

(defun rectangle (x y width height &optional (u1 0) (v1 0) (u2 1) (v2 1))
  (let* ((w/2 (/ width 2.0))
         (h/2 (/ height 2.0))
         (x1 (- x w/2))
         (x2 (+ x w/2))
         (y1 (- y h/2))
         (y2 (+ y h/2)))
    (gl:tex-coord u1 v2)
    (gl:vertex x1 y1 0)
    (gl:tex-coord u2 v2)
    (gl:vertex x2 y1 0)
    (gl:tex-coord u2 v1)
    (gl:vertex x2 y2 0)
    (gl:tex-coord u1 v1)
    (gl:vertex x1 y2 0)))

(defmethod draw-object ((o click-particles))
  (bind-texture :click-particle)
  (gl:with-primitive :quads
    (loop with positions = (positions o)
          for i below (length positions) by 2
          for x = (aref positions i)
          for y = (aref positions (1+ i))
          do (rectangle x y 32 32))))

(defmethod update-object ((o click-particles) delta-t)
  (incf (age o) delta-t)
  (setf delta-t (float delta-t 1.0))
  (if (< (age o) *click-particles-lifetime-ms*)
      ;; not done, update particles, and return a non-nil value
      (map-into (positions o) (lambda (p v)
                                (+ p (* v delta-t 0.001)))
                (positions o) (velocities o))
      ;; particle system is done, remove it
      nil))


(defun draw ()
  (gl:clear :color-buffer-bit)
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (draw-object *particle-manager*)
  (sdl:update-display))

(defun update (delta-t)
 (update-object *particle-manager* delta-t))

(defun mouse-down (button state x y)
  (declare (ignore button state x y))
  (push (make-instance 'click-particles :count 2) (systems *particle-manager*)))

;; ============================================================================
;; VOB
(defclass vbo-click-particles (click-particles)
  ((vbo :initform (car (gl:gen-buffers 1)) :accessor vbo)))

(defun vbo-rectangle (pointer offset x y width height
                      &optional (u1 0) (v1 0) (u2 1) (v2 1))
  (let* ((w/2 (/ width 2.0))
         (h/2 (/ height 2.0))
         (x1 (- x w/2))
         (x2 (+ x w/2))
         (y1 (- y h/2))
         (y2 (+ y h/2)))
    (macrolet
        ((store-values (&rest v)
           `(progn ,@(loop for i in v
                        collect `(setf (cffi:mem-aref pointer
                                                      :float (1- (incf offset)))
                                       (float ,i 0.0))))))
      (store-values u1 v2)
      (store-values x1 y1)
      (store-values u2 v2)
      (store-values x2 y1)
      (store-values u2 v1)
      (store-values x2 y2)
      (store-values u1 v1)
      (store-values x1 y2))))

(defmethod draw-object ((o vbo-click-particles))
  (let ((components-per-vertex 4)
        (size-of-float 4)
        (vertices-per-sprite 4))
    ;; activate the VBO
    (gl:bind-buffer :array-buffer (vbo o))
    ;; allocate space for the data
    (%gl:buffer-data :array-buffer
                     (* (/ (length (positions o)) 2)
                        vertices-per-sprite
                        components-per-vertex
                        size-of-float)
                     (cffi:null-pointer) ;; just allocating space
                     :stream-draw)
    ;; get a pointer to the VBO memory
    (gl:with-mapped-buffer (p :array-buffer :write-only)
      ;; copy the vertex data to the buffer
      (loop with positions = (positions o)
         for i below (length positions) by 2
         for offset from 0 by (* components-per-vertex vertices-per-sprite)
         for x = (aref positions i)
         for y = (aref positions (1+ i))
         do (vbo-rectangle p offset x y 32 32)))
    ;; draw the object
    (%gl:tex-coord-pointer 2 :float
                           (* components-per-vertex size-of-float)
                           (cffi:null-pointer))
    (%gl:vertex-pointer 2 :float
                        (* components-per-vertex size-of-float)
                        (cffi:make-pointer (* 2 size-of-float)))
    (gl:enable-client-state :texture-coord-array)
    (gl:enable-client-state :vertex-array)
    (%gl:draw-arrays :quads 0 (* vertices-per-sprite
                                 (/ (length (positions o)) 2)))
    (gl:disable-client-state :vertex-array)
    (gl:disable-client-state :texture-coord-array))
  ;; finally disable the buffer
  (gl:bind-buffer :array-buffer 0))

(defgeneric delete-particle-system (object))
(defmethod delete-particle-system ((object vbo-click-particles))
  (format t "deleting vbo ~a ~%"(vbo object))
  (gl:delete-buffers (list (vbo object))))

(defgeneric delete-particle-systems (manager))
(defmethod delete-particle-systems ((manager particle-manager))
  (format t "deleting total particles ~a ~%" (length (systems manager)))
  (mapc 'delete-particle-system (systems manager)))

(defmacro with-particle-manager (&body body)
  (let ((manager (gensym "PARTICLE-MANAGER-")))
    `(let* ((,manager (make-instance 'particle-manager))
            (*particle-manager* ,manager))
       (unwind-protect
            (progn
              ,@body)
         (delete-particle-systems ,manager)))))

(defun main-loop ()
  (sdl:with-init ()
    (sdl:window *nominal-screen-width* *nominal-screen-height*
                :flags (logior sdl:sdl-opengl
                               sdl:sdl-resizable))
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (let ((previous-tick (sdl:sdl-get-ticks)))
      (flet ((mx (x) ;; adjust mouse coordinates from screen to world
               (* x (/ (float *screen-width* 1.0) *actual-screen-width*)))
             (my (y) ;; adjust mouse coordinates from screen to world
               (* y (/ (float *screen-height* 1.0) *actual-screen-height*))))
        (with-texture-manager
          (with-particle-manager
            (init)
            (setup-ortho-projection *nominal-screen-width*
                                    *nominal-screen-height*)
            (sdl:with-events ()
              (:quit-event () t)
              (:video-resize-event (:w w :h h)
                (sdl:resize-window w h)
                (reload-textures *texture-manager*)
                (restartable (setup-ortho-projection w h)))
              (:key-down-event (:state state :scancode scancode :key key
                              :mod-key mod-key :unicode unicode)
                (restartable (key-down key state mod-key scancode unicode)))
              (:key-up-event (:state state :scancode scancode :key key
                              :mod-key mod-key :unicode unicode)
                (restartable (key-up key state mod-key scancode unicode)))
              (:mouse-button-up-event (:button button :state state :x x :y y)
                (restartable (mouse-up button state (mx x) (my y))))
              (:mouse-button-down-event (:button button :state state :x x :y y)
                (restartable (mouse-down button state (mx x) (my y))))
              (:mouse-motion-event (:x x :y y :x-rel delta-x :y-rel delta-y)
                (setf *mouse-x* (mx x)
                      *mouse-y* (my y))
                (restartable (mouse-move (mx x) (my y)
                                         (mx delta-x) (my delta-y))))
              (:idle ()
                     #+(and sbcl (not sb-thread))(restartable
                                                   (sb-sys:serve-all-events 0))
                     (let ((delta-t (- (sdl:sdl-get-ticks) previous-tick)))
                       (setf previous-tick (sdl:sdl-get-ticks))
                       ;; we check for negative delta-t in case sdl's
                       ;; timer wraps after some amount of time, 0 for
                       ;; a frame is better a large negative number
                       (restartable (update (if (minusp delta-t) 0 delta-t))))
                     (restartable (draw))
                     (restartable
                       (bt:with-lock-held (*next-frame-hook-mutex*)
                         (loop for i in *next-frame-hook*
                               do (funcall i))
                         (setf *next-frame-hook* nil)))))))))))

(defun mouse-down (button state x y)
  (declare (ignore button state x y))
  (push (make-instance 'vbo-click-particles) (systems *particle-manager*)))

(main-loop)
;; ------------------------------------------------------------------perfomance
(defparameter *frame-count* 0)
(defparameter *last-fps-message-time* 0)
(defparameter *last-fps-message-frame-count* 0)
(defparameter *fps-message-interval* 2000) ;; in milliseconds

(next-frame
  (setf (sdl:frame-rate) 60))

(defun init ()
  (setf *frame-count* 0)
  (setf *last-fps-message-time* (sdl:sdl-get-ticks))
  (vecto-texture (:click-particle 64 64)
    (vecto:set-rgba-fill 0 0 0 0)
    (vecto:clear-canvas)
    (loop for i below 16
         do
         (vecto:set-rgba-stroke 1 1 1 (/ 1 16.0))
         (vecto:set-line-width i)
         (vecto:centered-circle-path 32 32 20)
         (vecto:stroke)))
  (reload-textures *texture-manager*))

(next-frame
  (init))

(defun update-fps ()
  ;; update the frame count
  (incf *frame-count*)
  ;; handle tick count wrapping to 0
  (when (< (sdl:sdl-get-ticks) *last-fps-message-time*)
    (setf *last-fps-message-time* (sdl:sdl-get-ticks)))
  ;; see if it is time for next message
  (when (>= (sdl:sdl-get-ticks)
           (+ *last-fps-message-time* *fps-message-interval*))
    (let ((frames (- *frame-count* *last-fps-message-frame-count*))
          (seconds (/ (- (sdl:sdl-get-ticks) *last-fps-message-time*) 1000.0)))
      (format t "~s seconds: ~s fps, ~s ms per frame~%"
              seconds
              (if (zerop seconds) "<infinite>" (/ frames seconds))
              (if (zerop frames) "<infinite>" (* 1000 (/ seconds frames)))))
    (setf *last-fps-message-time* (sdl:sdl-get-ticks))
    (setf *last-fps-message-frame-count* *frame-count*)))

(defun update (delta-t)
  (update-fps)
  (update-object *particle-manager* delta-t))


;; ===========================================================================
(defun main-loop ()
  (sdl:with-init ()
    (sdl:window *nominal-screen-width* *nominal-screen-height*
                :flags (logior sdl:sdl-opengl
                               sdl:sdl-resizable))
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (let ((previous-tick (sdl:sdl-get-ticks)))
      (flet ((mx (x) ;; adjust mouse coordinates from screen to world
               (* x (/ (float *screen-width* 1.0) *actual-screen-width*)))
             (my (y) ;; adjust mouse coordinates from screen to world
               (* y (/ (float *screen-height* 1.0) *actual-screen-height*))))
        (with-texture-manager
          (with-particle-manager
            (init)
            (setup-ortho-projection *nominal-screen-width*
                                    *nominal-screen-height*)
            (sdl:with-events ()
              (:quit-event () t)
              (:video-resize-event (:w w :h h)
                (sdl:resize-window w h)
                (reload-textures *texture-manager*)
                (restartable (setup-ortho-projection w h)))
              (:key-down-event (:state state :scancode scancode :key key
                              :mod-key mod-key :unicode unicode)
                (restartable (key-down key state mod-key scancode unicode)))
              (:key-up-event (:state state :scancode scancode :key key
                              :mod-key mod-key :unicode unicode)
                (restartable (key-up key state mod-key scancode unicode)))
              (:mouse-button-up-event (:button button :state state :x x :y y)
                (restartable (mouse-up button state (mx x) (my y))))
              (:mouse-button-down-event (:button button :state state :x x :y y)
                (restartable (mouse-down button state (mx x) (my y))))
              (:mouse-motion-event (:x x :y y :x-rel delta-x :y-rel delta-y)
                (setf *mouse-x* (mx x)
                      *mouse-y* (my y))
                (restartable (mouse-move (mx x) (my y)
                                         (mx delta-x) (my delta-y))))
              (:idle ()
                     #+(and sbcl (not sb-thread))(restartable
                                                   (sb-sys:serve-all-events 0))
                     (let ((delta-t (- (sdl:sdl-get-ticks) previous-tick)))
                       (setf previous-tick (sdl:sdl-get-ticks))
                       ;; we check for negative delta-t in case sdl's
                       ;; timer wraps after some amount of time, 0 for
                       ;; a frame is better a large negative number
                       (restartable (update (if (minusp delta-t) 0 delta-t))))
                     (restartable (draw))
                     (restartable
                       (bt:with-lock-held (*next-frame-hook-mutex*)
                         (loop for i in *next-frame-hook*
                               do (funcall i))
                         (setf *next-frame-hook* nil)))))))))))
