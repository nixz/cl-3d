;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; sdl.lisp --- 
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
(defmacro defagent (&body body)
  "For now does a function call"
  `(defun ,@body))

(defagent add (a b)
  (+ a b))


(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue"  )))

(defagent draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color 1 1 1)
    (gl:vertex 0 0 0)
    (gl:color 0 1 0)
    (gl:vertex 0.5 1 0)
    (gl:color 0 0 1)
    (gl:vertex 1 0 0))
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

(defagent draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color 1 0 0)
    (gl:vertex -1 -1 0) ;;  hmm, i need to add an option to the syntax
    (gl:color 0 1 0)
    (gl:vertex 0 1 0)   ;;  highlighting to show changed lines...
    (gl:color 0 0 1)
    (gl:vertex 1 -1 0)) ;; (this one changed too)
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

;; (defparameter *first* 1)
(defparameter 
(defagent toggle() t)
(defagent toggle() nil)

(enva (defagent draw ()
  "draw a frame"
  ;; (gl:clear :color-buffer-bit)
  ;; draw a triangle
  (if (toggle)
      (progn
        (gl:with-primitive :triangles
          (gl:color 1 1 1)
          (gl:vertex 0 0 0)
          (gl:color 0 1 0)
          (gl:vertex 0.5 1 0)
          (gl:color 0 0 1)
          (gl:vertex 1 0 0)))
      (progn 
        (gl:with-primitive :triangles
          (gl:color 1 0 0)
          (gl:vertex -1 -1 0) ;;  hmm, i need to add an option to the syntax
          (gl:color 0 1 0)
          (gl:vertex 0 1 0)   ;;  highlighting to show changed lines...
          (gl:color 0 0 1)
          (gl:vertex 1 -1 0)))) ;; (this one changed too)
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

(defun main-loop ()
  (sdl:with-init ()
    (sdl:window 320 240 :flags sdl:sdl-opengl)
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             ;; this lets slime keep working while the main loop is running
             ;; in sbcl using the :fd-handler swank:*communication-style*
             ;; (something similar might help in some other lisps, not sure which though)
             #+(and sbcl (not sb-thread)) (restartable
                                           (sb-sys:serve-all-events 0))
             (restartable (draw))))))


(main-loop)



