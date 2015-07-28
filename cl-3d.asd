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
  :depends-on (#:cepl
               #:bordeaux-threads
               #:usocket
               #:cl-package-locks
               #:sb-cga)
  :components
  ((:module "XMLisp"
            :components ((:file "package")
                         (:file "XMLisp" :depends-on ("package"))))
   (:file "package")
   (:file "util"                 :depends-on ("package"))
   (:file "devices"              :depends-on ("package" "util"))
   (:file "types"                :depends-on ("util"))
   (:file "core"                 :depends-on ("types"))
   (:file "concepts"             :depends-on ("core"))
   (:file "environmental-effects" :depends-on ("core"))
   (:file "geometry-3d"          :depends-on ("rendering" "shape"))
   (:file "grouping"             :depends-on ("core"))
   (:file "navigation"           :depends-on ("core"))
   (:file "networking"           :depends-on ("package"))
   (:file "pointing-device-sensor" :depends-on ("core"))
   (:file "programmable-shaders" :depends-on ("shape"
                                              "rendering"
                                              "networking"))
   (:file "rendering"            :depends-on ("core"))
   (:file "shape"                :depends-on ("core"))
   (:file "text"                 :depends-on ("core"))
   (:file "time"                 :depends-on ("core"))
   (:file "sound"                :depends-on ("time"))
   (:file "texturing"            :depends-on ("shape"
                                              "rendering"
                                              "sound"))
   (:file "scene"                :depends-on ("package"))
   (:file "event-utilities"      :depends-on ("core"))
   (:file "scripting"            :depends-on ("core"))
   (:file "interpolation"        :depends-on ("core"))
   (:file "key-device-sensor"    :depends-on ("core"))
   (:file "lighting"             :depends-on ("core"))
   (:file "geometry-2d"          :depends-on ("rendering"))
   (:file "environment-sensor"   :depends-on ("core"))
   (:file "cad-geometry"         :depends-on ("grouping"))
   (:file "main"                 :depends-on ("concepts"
                                              "grouping"
                                              "geometry-3d"
                                              "navigation"
                                              "environmental-effects"
                                              "pointing-device-sensor"))
   (:file "run"                  :depends-on ("grouping"
                                              "geometry-3d"
                                              "navigation"
                                              "environmental-effects"
                                              "pointing-device-sensor"))
   (:file "opengl"               :depends-on ("package"))))
;; (:file "graphics" :depends-on("package"
;;                               "opengl"))
