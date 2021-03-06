;============================================================================
; OM-Chant
; Control CHANT synthesis from OpenMusic
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
;;; CHANT path / om# preferences
;;; Jean Bresson, IRCAM 2019
;;;===================================================


(in-package :om)

(defmethod default-chant-path () 
  (let ((libpath (mypathname (find-library "OM-Chant"))))
    (om-make-pathname :directory (append (pathname-directory libpath) 
                                     '("resources" "bin" #+macosx "macos" #+win32 "win" #+linux "linux"))
                      :host (pathname-host libpath) :device (pathname-device libpath)
                      :name "chant" #+win32 :type #+win32 "exe")))


;(add-preference-section :externals "OM-Chant" nil '(:chant-path))
;(add-preference :externals :pm2-path "Chant exec" :file 'default-chant-path)

;;; redefined form OM6/om#

(defun get-chant-exec-path ()
  ;(om::real-exec-pathname (om::get-pref-value :externals :chant-path))
  (om::real-exec-pathname (default-chant-path))
  )


(defun open-param-visualizer (visualization-params ch-events trans-function) 
  (declare (ignore visualization-params ch-events trans-function))
  nil)


(defun chant-get-default-audio-format ()
  (get-pref-value :audio :format))

(defun chant-get-default-audio-res ()
  (get-pref-value :audio :resolution))

(defun chant-get-default-audio-sr () 
  (get-pref-value :audio :samplerate))

(defun chant-get-default-audio-normalization ()
  (and (get-pref-value :audio :normalize)
       ; (get-pref-value :audio :normalize-level)
       t))
