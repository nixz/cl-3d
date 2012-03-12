;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; cl-3d.asd --- Define the system
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
(defsystem cl-3d
  :description "x3d implementation in common lisp"
  :depends-on (cl-opengl sb-cga cl-glut cl-glu)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "types" :depends-on ("util" "package"))
               (:file "core" :depends-on ("util" "types" "package"))
               (:file "networking" :depends-on ("package"))
               (:file "grouping" :depends-on ("core" "util" "types" "package"))
               (:file "rendering" :depends-on ("core" "util" "types" "package"))
               (:file "shape" :depends-on ("core" "util" "types" "package"))
               (:file "geometry-3d" :depends-on ("rendering" "shape" "core" "util" "types" "package"))
               (:file "navigation" :depends-on ("core" "util" "types" "package"))
               (:file "env-effects" :depends-on ("core" "util" "types" "package"))
               (:file "programmable-shaders" :depends-on ("shape" "rendering" "networking" "core" "types" "package"))
               (:file "main" :depends-on ("core"
                                          "grouping"
                                          "rendering"
                                          "shape"
                                          "geometry-3d"
                                          "navigation"
                                          "env-effects"
                                          "util"
                                          "types"
                                          "package"))
               (:file "run" :depends-on ("core"
                                         "grouping"
                                         "rendering"
                                         "shape"
                                         "geometry-3d"
                                         "navigation"
                                         "env-effects"
                                         "util"
                                         "types"
                                         "package"))))
