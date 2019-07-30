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
; CHANT path / OM preferences
; Jean Bresson, IRCAM 2010
;===================================================


(in-package :om)


(defun get-chant-exec-path ()
  (let ((libpath (lib-pathname (find-library "OM-Chant"))))
    (om-make-pathname :directory (append (pathname-directory libpath) 
                                         '("resources" "bin" #+macosx "macos" #+win32 "win" #+linux "linux"))
                      :host (pathname-host libpath) :device (pathname-device libpath)
                      :name "chant" #+win32 :type #+win32  "exe")
    ))


(defun chant-get-default-audio-format () *def-snd-format*)
(defun chant-get-default-audio-res () *audio-res*)
(defun chant-get-default-audio-sr () *audio-sr*)
(defun chant-get-default-audio-normalization () (and *normalize* *normalize-level*))

#|
;; no real need to make CHANT-PATH A user preference...
;; (pushr 'chant *external-prefs*)

(defvar *chant-path* nil)

(defmethod get-external-name ((module (eql 'chant))) "CHANT")

(defmethod get-external-module-vals ((module (eql 'chant)) modulepref) (get-pref modulepref :chant-options))
(defmethod get-external-module-path ((module (eql 'chant)) modulepref) (get-pref modulepref :chant-path))
(defmethod set-external-module-vals ((module (eql 'chant)) modulepref vals) (set-pref modulepref :chant-options vals))
(defmethod set-external-module-path ((module (eql 'chant)) modulepref path) 
  (set-pref modulepref :chant-path path))


(defmethod get-external-def-vals ((module (eql 'chant))) 
    (list :chant-path (get-chant-exec-path)))

(defmethod save-external-prefs ((module (eql 'chant))) 
  `(:chant-path ,(om-save-pathname *chant-path*)))

(defmethod put-external-preferences ((module (eql 'chant)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :chant-options)))
    (when (get-pref moduleprefs :chant-path)
      (setf *chant-path* (find-true-external (get-pref moduleprefs :chant-path))))
    ))
|#


