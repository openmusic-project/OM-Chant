;;;===================================================
;;; OM-CHANT
;;; Control CHANT synthesis from OpenMusic
;;;
;;; CHANT path / OM7 preferences
;;; Jean Bresson, IRCAM 2019
;;;===================================================


(in-package :om)

(defmethod default-chant-path () 
  (let ((libpath (mypathname (find-library "OM-Chant"))))
    (om-make-pathname :directory (append (pathname-directory libpath) 
                                     '("resources" "bin" #+macosx "macos" #+win32 "win"))
                      :host (pathname-host libpath) :device (pathname-device libpath)
                      :name "chant" #+win32 :type #+win32 "exe")))


;(add-preference-section :externals "OM-Chant" nil '(:chant-path))
;(add-preference :externals :pm2-path "Chant exec" :file 'default-chant-path)

;;; redefined form OM6/om7

(defun get-chant-exec-path ()
  ;(om::real-exec-pathname (om::get-pref-value :externals :chant-path))
  (om::real-exec-pathname (default-chant-path))
  )


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
  

;;; temp
(defun chant-forum-protec (path lib) t)