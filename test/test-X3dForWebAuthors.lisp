;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; test-X3dForWebAuthors.lisp --- Load the various scenes from
;;;; X3DForWebAuthos repository
;;;;
;;;; Copyright (c) 2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
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

(in-package #:x3d)

(defun x3d-4wa (path)
  (concatenate 'cl:string "/home/nikhil/cl-3d/test/X3dForWebAuthors/" path))

;; ----------------------------------------------------------------------------
;; Loading x3d files form the X3dForWebAuthors repo
;; ........................................................................Ch-1
(load-object (x3d-4wa "Chapter01-TechnicalOverview/HelloWorld.x3d"))
(load-object (x3d-4wa "Chapter01-TechnicalOverview/HeaderProfileComponentMetaExample.x3d"))
(load-object (x3d-4wa "Chapter01-TechnicalOverview/HelloTriangle.x3d"))
(load-object (x3d-4wa "Chapter01-TechnicalOverview/newScene.x3d"))
;; ........................................................................Ch-2
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/GeometryPrimitiveNodesWhiteBackground.x3d"))
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/TextLengthMaxExtent.x3d"))
;; (load-object (x3d-4wa "Chapter02-GeometryPrimitives/TextSpecialCharacters.x3d")) ;; special character issue
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/Text.x3d"))
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/Sphere.x3d"))
;; (load-object (x3d-4wa "Chapter02-GeometryPrimitives/originals/pixel_perfect.x3d")) ; what is this?
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/GeometryPrimitiveNodes.x3d"))
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/Cylinder.x3d"))
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/Cone.x3d"))
(load-object (x3d-4wa "Chapter02-GeometryPrimitives/Box.x3d"))
;; ........................................................................Ch-3
(load-object (x3d-4wa "Chapter03-Grouping/CoordinateAxesNSEW.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/CoordinateAxesInlineExample.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/OrderOfOperations2.x3d"))
;; (load-object (x3d-4wa "Chapter03-Grouping/GroupAnimated.x3d")) ; Loop issue in time.lisp
(load-object (x3d-4wa "Chapter03-Grouping/Group.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/BoundingBoxIllustration.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/LOD.x3d"))
;; (load-object (x3d-4wa "Chapter03-Grouping/Switch.x3d")) ; loop issues (time.lisp)
(load-object (x3d-4wa "Chapter03-Grouping/Inline.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/CoordinateAxes.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/Transform.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/OrderOfOperations1.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/TransformCenterOffsetForRotation.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/StaticGroup.x3d"))
(load-object (x3d-4wa "Chapter03-Grouping/LodWithDifferentShapes.x3d"))
;; ........................................................................Ch-4
(load-object (x3d-4wa "Chapter04-ViewingNavigation/NavigationInfo.x3d"))
(load-object (x3d-4wa "Chapter04-ViewingNavigation/AnchorComparison.x3d"))
;;(load-object (x3d-4wa "Chapter04-ViewingNavigation/BindingOperations.x3d")) ; fields in (scripting.lisp)
(load-object (x3d-4wa "Chapter04-ViewingNavigation/AliasingExample.x3d"))
(load-object (x3d-4wa "Chapter04-ViewingNavigation/Viewpoint.x3d"))
(load-object (x3d-4wa "Chapter04-ViewingNavigation/Collision.x3d"))
(load-object (x3d-4wa "Chapter04-ViewingNavigation/Anchor.x3d"))
(load-object (x3d-4wa "Chapter04-ViewingNavigation/Billboard.x3d"))
;; ;; ........................................................................Ch-5
(load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TwoSidedMaterial.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TextureTransformScale.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/PixelTextureGaribaldi.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/Garibaldi.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/DiffuseColor.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/SpecularColor.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/Transparency.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/CircleFishPTPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/PixelTextureTransformScale.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/GeometryPrimitiveNodesImageTexture.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/PixelTextureSnowman.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/PixelTextureBW.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/CircleFishPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TextureLocalGaribaldi.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/LineProperties.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/PixelTextureComponentExamples.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/GaribaldiRemoteNoTexture.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/FillProperties.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/EmissiveColor.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/PixelTexture.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/GaribaldiRemote.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/MovieTextureAuthoringOptions.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TextureTransform.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/Table5_18-PixelTexture.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TextureTransformFull.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TextureTransformTranslation.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/GaribaldiLocalTexture.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/TextureRemoteGaribaldi.x3d"))
;; (load-object (x3d-4wa "Chapter05-AppearanceMaterialTextures/AmbientIntensity.x3d"))
;; ;; ........................................................................Ch-6
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/Color.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/IndexedLineSet.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/LineSet.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/ElevationGridNonPlanarQuadrilaterals.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/LineSetComparison.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/ColorPerVertexExamples.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/X3dBookWebsiteLogo.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/ElevationGrid.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/ExtrusionRoomWalls.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/IndexedFaceSet.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/PointSet.x3d"))
;; (load-object (x3d-4wa "Chapter06-GeometryPointsLinesPolygons/ExtrusionPentagon.x3d"))
;; ;; ........................................................................Ch-7
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/Garibaldi.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/HelloX3dAuthorsAnimationChain.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/DolphinMorpher.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/TimeSensorChaining.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/DolphinPose03.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/DolphinSwitcher.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/CircleFishPositionInterpolator.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/CircleFishPTPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/DolphinPose02.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/ScalarInterpolatorFishPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/ColorInterpolatorFishPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/CoordinateInterpolator2dExample.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/DolphinPose01.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/CircleFishPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/NormalInterpolator.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/OrientationInterpolatorFishExample.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/ColorInterpolatorExample.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/ScalarInterpolator.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/PositionOrientationInterpolatorsExample.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/MotionInterpolatorFishPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/PositionInterpolatorPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/PositionInterpolator2dExample.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/PositionOrientationInterpolatorsExampleTraced.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/OrientationInterpolatorFishPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter07-EventAnimationInterpolation/ScalarInterpolatorFishExample.x3d"))
;; ;; ........................................................................Ch-8
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/StringSensor.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/CylinderSensorPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/KeySensor-Lefty.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/UserInteractivitySensorNodes.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/PlaneSensor-PumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/SphereSensor-Lefty.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/TouchSensor-PumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter08-UserInteractivity/Doors.x3d"))
;; ;; ........................................................................Ch-9
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BooleanFilterPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/TimeTriggerTest.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/MoodSelector.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/IntegerSequencerRoadSignSwitcher.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BooleanToggle.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptEvents.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/IntegerTriggerPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptEventsPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptSimpleStateEvents.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BooleanSequencerPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/CircleLinesExample.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/CircleLinesExample60.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/TestScriptInitialization.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/TimeTriggerPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ElevationGridSimpleWaveAnimation.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BooleanTriggerPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptNodeEventOutControl-EcmaScript.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptNodeFieldControl-EcmaScript.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/IntegerSequencerPumpHouse.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptComplexStateEvents.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/ScriptControlEvents.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BackgroundColorArrayAnimation.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/IntegerTrigger.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BooleanSequencerIntegerSequencer.x3d"))
;; (load-object (x3d-4wa "Chapter09-EventUtilitiesScripting/BooleanTogglePumpHouse.x3d"))
;; ;; .......................................................................Ch-10
;; (load-object (x3d-4wa "Chapter10-Geometry2D/ArcClose2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Rectangle2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/TriangleSet2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Polypoint2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Circle2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Summary2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Polyline2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Arc2D.x3d"))
;; (load-object (x3d-4wa "Chapter10-Geometry2D/Disk2D.x3d"))
;; ;; .......................................................................Ch-11
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/BackgroundImagesOnly.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/BackgroundColorsOnly.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/DirectionalLight.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/PointLight.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/BackgroundSelector.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/SpotLightVisualization.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/TextureBackground-KelpForestMain.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/SpotLightColor.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/BackgroundTimeOfDay.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/Fog-KelpForestMain.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/BackgroundKelpForestMain.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/SpotLight.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/BackgroundSequencer.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/PointLightColor.x3d"))
;; (load-object (x3d-4wa "Chapter11-LightingEnvironmentalEffects/TextureBackground.x3d"))
;; ;; .......................................................................Ch-12
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/ProximitySensorNoOverlap.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/VisibilitySensor-KelpForestMain.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/SoundAudioClip.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/ProximitySensorOverlap.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/LoadSensor.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/LoadSensor-KelpForestMain.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/SoundLoadSensorTest.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/ProximitySensor.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/LoadSensor-KelpForestMain_timeOut.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/ProximitySensorMultiple.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/ProximitySensorSingle.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/SoundVisualization.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/ProximitySensor-KelpForestMain.x3d"))
;; (load-object (x3d-4wa "Chapter12-EnvironmentSensorSound/Collision-KelpForestMain.x3d"))
;; ;; .......................................................................Ch-13
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/IndexedTriangleStripSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/IndexedQuadSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/NonplanarPolygons.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/MorphingTriangleSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/SummaryIndexedTriangleSets.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/IndexedTriangleSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/QuadSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/Normal.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/TriangleStripSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/TriangleSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/IndexedTriangleFanSet.x3d"))
;; (load-object (x3d-4wa "Chapter13-GeometryTrianglesQuadrilaterals/TriangleFanSet.x3d"))
;; ;; .......................................................................Ch-14
;; (load-object (x3d-4wa "Chapter14-Prototypes/HeadsUpDisplayPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/ViewFrustumExample.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/MaterialModulator.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/ArtDecoExamplesExcerpt.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/WhereAmIPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/HudKelpForest.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/ArtDecoPrototypesExcerpt.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/HeadsUpDisplayExample.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/ViewFrustumPrototype.x3d"))
;; (load-object (x3d-4wa "Chapter14-Prototypes/TextStringPrototype.x3d"))
;; ;; .......................................................................Ch-15
;; (load-object (x3d-4wa "Chapter15-Metadata/MetadataExamples.x3d"))
;; ;; ........................................................................Kelp
;; (load-object (x3d-4wa "KelpForestExhibit/BlackSurfPerch.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpForestNoNancy.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/CircleFishLodPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/Garibaldi.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineSchoolTriple.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/FishExamples.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/ChangingFog.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SeaStarPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpTankExternalLights.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SeaStarSimple.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/BlueRockFish.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/CopperRockFishPlacardViewer.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/IntroductionMessage.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SeaStarHighResolutionExample.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/TreeFishPlaque.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SharkLucy.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/FishSchool.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/RubberLipGroup.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/HalfMoonGroup.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineSchoolMidterm.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpFlexibleStipe.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/Fish.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpBulb.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/CircleFishPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/FishPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/FishModelComparison.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineSchoolDouble.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/StripedSurfPerchPlaque.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpSurfPerch.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SeaStarHighResolutionPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpTank.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineShape.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/StripedSurfPerch.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/RockFloor.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpExamplesNoBase.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/PumpHouse.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/CircleFishExample.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SharkLucyLocale.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/CircleFishLodExample.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/Sardine.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SharkLefty.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineSchoolFinal.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpTankWaterSurface.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpTankExternalLight.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineSchool.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpBass.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/HalfMoonPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineWagging.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineSchoolOriginal.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpPrototype.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SeaStarGroup.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/KelpForestMain.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SardineX.x3d"))
;; (load-object (x3d-4wa "KelpForestExhibit/SharkLeftyLocale.x3d"))
