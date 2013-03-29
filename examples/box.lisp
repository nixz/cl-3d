;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; box.lisp --- Test box tutorial from x3dforwebauthors ch2.
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

;; This example is taken from the following link
;; http://x3dgraphics.com/examples/X3dForWebAuthors/Chapter02-GeometryPrimitives/_pages/page01.html

;; ------------------------------------------------------------------------lisp
(in-package #:cl-3d)
(render
<Scene>
  <Background skyColor='1 1 1'/>
  <Viewpoint description='Book View'
             orientation='-0.747 -0.624 -0.231 1.05'
             position='-1.81 3.12 2.59'/>
  <Shape>
  <Box size='1 2 3'/>
    <Appearance>
      <Material/>
    </Appearance>
  </Shape>
</Scene>)

;; ----------------------------------------------------------------------------
;; purely in lisp this should probably look like the following
;; (Scene
;;   (Background :skyColor '(1 1 1))
;;   (Viewpoint :description "Book View"
;;              :orientation '(-0.747 -0.624 -0.231 1.05)
;;              :position '(-1.81 3.12 2.59))
;;   (Shape :geometry (Box :size '(1 2 3))
;;          :appearance (Appearance :material (Material))))
