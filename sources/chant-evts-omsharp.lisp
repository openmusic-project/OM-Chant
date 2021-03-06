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
; J. Bresson, M. Stroppa (2010-2019)
;============================================================================

(in-package :om)

(defclass chant-evt (om::timed-object) ((id :accessor id :initform 0)))

(defmethod initialize-instance :after ((self chant-evt) &rest initargs)
  (setf (slot-value self 'id) (string (gensym))))

(defmethod ch-evt-fade-in ((self chant-evt))
  (and (listp (dur self)) (nth 1 (dur self))))

(defmethod ch-evt-fade-out ((self chant-evt))
  (and (listp (dur self)) (nth 2 (dur self))))
  
(defmethod ch-evt-duration ((self chant-evt))
  (car (list! (dur self))))

(defmethod set-dur-fade-in ((self chant-evt) fade-in)
  (list (ch-evt-duration self) fade-in (ch-evt-fade-out self)))

(defmethod set-dur-fade-out ((self chant-evt) fade-out)
  (list (ch-evt-duration self) (ch-evt-fade-in self) fade-out))


;=================================
; SIMPLE EVENTS
;=================================

(defclass! chant-simple-evt (chant-evt SynthesisEvt)
  ((action-time :initform 0 :accessor action-time :initarg :action-time :documentation "start time [sec]")
   (dur :accessor dur :initform 0  :initarg :dur :documentation "duration [sec]")))

(defmethod get-obj-dur ((self chant-simple-evt)) 
  (max 1 (round (* 1000 (+ (action-time self) (event-dur self))))))

(defmethod get-absolute-time ((self chant-evt))
  (+ (action-time self) (/ (onset self) 1000.0)))

(defmethod end-time ((self chant-evt))
  (+ (action-time self) (event-dur self)))

(defmethod get-channels-num ((self chant-simple-evt)) 1)




;================================
;;; Some chant-simple-evt objects with single value can be edited through a BPF editor:
(defmethod get-chant-bpf-slot ((self t)) nil)

(defclass ch-bpfeditor (bpf-editor) ())

(defmethod object-value ((self ch-bpfeditor)) 
  (let ((obj (call-next-method)))
    (and (get-chant-bpf-slot obj) 
         (slot-value obj (get-chant-bpf-slot obj)))))

(defmethod update-to-editor ((self ch-bpfeditor) (from omboxeditcall))
  (unless (bpf-p (object-value self))
    (close-editor from)))

(defmethod display-modes-for-object ((self chant-simple-evt))
  (append '(:hidden :text)
          (if (get-chant-bpf-slot self) '(:mini-view) nil)))

(defmethod get-cache-display-for-draw ((self chant-simple-evt) box) 
  (if (get-chant-bpf-slot self)
      (get-cache-display-for-draw (slot-value self (get-chant-bpf-slot self)) box)
    (call-next-method)))

(defmethod draw-mini-view ((self chant-simple-evt) (box t) x y w h &optional time)
  (if (get-chant-bpf-slot self)
      (let ((bpfslotval (slot-value self (get-chant-bpf-slot self))))
        (if (bpf-p bpfslotval)
            (draw-mini-view bpfslotval box x y w h time)
          (om-draw-string (+ x 4) (+ y (/ h 2) 3) (format NIL "~A = ~A" (get-chant-bpf-slot self) bpfslotval)
                          :font (om-def-font :font1b))))
    (call-next-method)))


;================================
; GLOBAL UTILS
;================================

(defun apply-fade-in-out (pts in out)
  (remove nil
          (append 
           (if in 
               (list (list (first (car pts)) 0)
                     (if (< (+ (first (car pts)) in) (first (cadr pts)))
                          (list (+ (first (car pts)) in) (second (car pts)))))
             (list (car pts)))
           
           (butlast (cdr pts))      
           (if out 
               (list 
                (if (> (- (first (car (last pts))) out) (first (car (last pts 2))))
                    (list (- (first (car (last pts))) out) (second (car (last pts 1)))))
                (list (first (car (last pts))) 0))
             (last pts))
           )))
  
;=================================
; CHANT MODULE : F0
;=================================

(defclass! ch-f0 (chant-simple-evt)
  ((action-time :initform 0 :accessor action-time :initarg :action-time :documentation "start time [sec]")
   (dur :accessor dur :initform 0  :initarg :dur :documentation "duration [sec]")
   (f0 :initform 110.0 :accessor f0  :initarg :f0 :documentation "fundamental frequency value(s) [Hz]"))
   (:icon 602)
   (:documentation "A Chant fundamental frequency event. 

Represents the evolution of the f0 in the Chant FOF generator during the interval determined by <action-time> and <dur>.

<dur> can be either a duration (in seconds) or a list (duration fade-in fade-out) where fade-in and fade-out (in seconds) will be applied on the f0.

<f0> can be either a constant value or a BPF.

Note that CH-F0 MUST BE USED ALONG WITH AT LEAST ONE CH-FOF OBJECT, which determines the values of the FOF.
"))

;;; TEST SUBCLASS CONTAINING VIBRATO AND JITTER VALUES
(defclass! ch-f0-vib-jit (ch-f0)
  ((vib-freq :accessor vib-freq :initarg :vib-freq :initform nil)
   (vib-amp :accessor vib-amp :initarg :vib-amp :initform nil)
   (jit-freq :accessor jit-freq :initarg :jit-freq :initform nil)
   (jit-amp :accessor jit-amp :initarg :jit-amp :initform nil))
  (:icon 955))

(defmethod f0-p ((self ch-f0)) t)
(defmethod f0-p ((self t)) nil)

(defmethod event-dur ((self Ch-F0))
  (let ((duree (ch-evt-duration self)))
  (if (and (bpf-p (f0 self))
           (not (and (numberp duree) (plusp duree))))
      (car (last (x-points (f0 self))))
    (or duree 0))))

(defmethod evt-type-str ((self Ch-F0)) "FQ0")
(defmethod evt-type-sid ((self Ch-F0)) 0)

(defmethod continuous-event-p ((self Ch-F0))
  (bpf-p (f0 self)))


(defmethod evt-to-sdif ((self Ch-F0))
  
  (cond 
   
   ((numberp (f0 self))
    
    (remove nil
            (append 
             (if (ch-evt-fade-in self) 
                 
                 (list (make-instance 'sdifframe :signature "1FOB" :streamid 0 
                                      :ftime (get-absolute-time self) 
                                      :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list 0)))))
                       (make-instance 'sdifframe :signature "1FOB" :streamid 0 
                                      :ftime (+ (get-absolute-time self) (ch-evt-fade-in self))
                                      :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list (f0 self)))))))
               
               (list (make-instance 'sdifframe :signature "1FOB" :streamid 0
                                    :ftime (get-absolute-time self) 
                                    :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list (f0 self))))))))
                    
             (when (plusp (ch-evt-duration self))
               (if (ch-evt-fade-out self) 
                   (list
                    (make-instance 'sdifframe :signature "1FOB" :streamid 0 
                                   :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self))
                                   :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list (f0 self))))))
                    (make-instance 'sdifframe :signature "1FOB" :streamid 0 
                                   :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                   :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list 0))))))
                 (list
                  (make-instance 'sdifframe :signature "1FOB" :streamid 0 
                                 :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                 :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list (f0 self))))))
                  )))
             )))
          
   ((bpf-p (f0 self))
    (let* ((pts (point-pairs (f0 self)))
           (xpts (mapcar 'car pts)))
      (setf pts (apply-fade-in-out pts (ch-evt-fade-in self) (ch-evt-fade-out self)))
      (loop for time in (if (and (numberp (ch-evt-duration self)) (plusp (ch-evt-duration self))
                                 (not (and (= (get-absolute-time self) (car xpts))
                                           (= (+ (get-absolute-time self) (ch-evt-duration self)) (last-elem xpts)))))
                            (om-scale xpts
                                      (get-absolute-time self)
                                      (+ (get-absolute-time self) (ch-evt-duration self)))
                          (om+ (get-absolute-time self) xpts))
            for f0 in (mapcar 'cadr pts) collect
            (make-instance 'sdifframe :signature "1FOB" :ftime time :streamid 0 
                           :lmatrix (list (om-make-sdifmatrix "1FQ0" (list (list f0)))))
            )))
    
   ))



;;;=======================
;;; CH-F0 BOX AND EDITOR
;;;=======================

(defmethod object-has-editor ((self CH-F0)) (bpf-p (f0 self)))
(defmethod get-editor-class ((self CH-F0)) 'ch-bpfeditor)
(defmethod get-chant-bpf-slot ((self CH-F0)) 'f0)


;=================================
; CHANT MODULE : NOISE
;=================================

(defclass! ch-noise (chant-simple-evt)
   (; (dist :initform 0 :initarg :dist :type number :accessor dist)
    (action-time :initform 0 :accessor action-time :initarg :action-time :documentation "start time [sec]")
    (dur :accessor dur :initform 0  :initarg :dur :documentation "duration [sec]")
    (amp :initform 1.0 :initarg :amp :type number :accessor amp :documentation "!! Only linear [0.0 -> 1.0]")
    )
   (:icon 604)
   (:documentation "A Chant Noise event. 

Represents the evolution of the noise amplitude during the interval determined by <action-time> and <dur>.

<dur> can be either a duration (in seconds) or a list (duration fade-in fade-out) where fade-in and fade-out (in seconds) will be applied on the noise amplitude.

<amp> can be either a constant value or a BPF.
"))

(defmethod evt-type-str ((self ch-noise)) "NOISE")
(defmethod evt-type-sid ((self ch-noise)) 2)

(defmethod noi-p ((self ch-noise)) t)
(defmethod noi-p ((self t)) nil)


(defmethod event-dur ((self ch-noise))
  (let ((duree (ch-evt-duration self)))
    (if (and (bpf-p (amp self))
             (not (and (numberp duree) (plusp duree))))
      (car (last (x-points (amp self))))
    (or duree 0))))

(defmethod continuous-event-p ((self ch-noise))
  (bpf-p (amp self)))


(defmethod evt-to-sdif ((self ch-noise))
  
  (cond 

   ((numberp (amp self))
    (remove nil
            (append 
             (if (ch-evt-fade-in self) 
                      
                 (list (make-instance 'sdifframe :signature "1NOI" :ftime (get-absolute-time self) :streamid 2 
                                      :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 0)))))
                       (make-instance 'sdifframe :signature "1NOI" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid 2 
                                      :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 (amp self)))))))
                    
               (list (make-instance 'sdifframe :signature "1NOI" :ftime (get-absolute-time self) :streamid 2 
                                    :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 (amp self))))))))
                    
             (when (plusp (ch-evt-duration self))
               (if (ch-evt-fade-out self) 
                   (list
                    (make-instance 'sdifframe :signature "1NOI" 
                                   :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) 
                                   :streamid 2 
                                   :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 (amp self))))))
                    (make-instance 'sdifframe :signature "1NOI" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                   :streamid 2 
                                   :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 0))))))
                 (list
                  (make-instance 'sdifframe :signature "1NOI" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                 :streamid 2 
                                 :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 (amp self))))))
                  )))
             )
            ))
        
   ((bpf-p (amp self))
    (let* ((pts (point-pairs (amp self)))
           (xpts (mapcar 'car pts)))
      (setf pts (apply-fade-in-out pts (ch-evt-fade-in self) (ch-evt-fade-out self)))
      (loop for time in (if (and (numberp (ch-evt-duration self)) (plusp (ch-evt-duration self))
                                 (not (and (= (get-absolute-time self) (car xpts))
                                           (= (+ (get-absolute-time self) (ch-evt-duration self)) (last-elem xpts)))))
                            (om-scale xpts
                                      (get-absolute-time self)
                                      (+ (get-absolute-time self) (ch-evt-duration self)))
                          (om+ (get-absolute-time self) xpts))
            for a in (mapcar 'cadr pts) collect
            (make-instance 'sdifframe :signature "1NOI" :ftime time :streamid 2 
                           :lmatrix (list (om-make-sdifmatrix "1DIS" (list (list 0 a)))))
            )))

   ))


;;;=================================================================
;;; CH-NOISE BOX AND EDITOR
;;;=================================================================

(defmethod object-has-editor ((self CH-NOISE)) (bpf-p (amp self)))
(defmethod get-editor-class ((self CH-NOISE)) 'ch-bpfeditor)
(defmethod get-chant-bpf-slot ((self CH-NOISE)) 'amp)



;=================================
; CHANT MODULE : SOUND
;=================================

(defclass! ch-snd (chant-simple-evt)
   ((action-time :initform 0 :accessor action-time :initarg :action-time :documentation "start time [sec]")
    (dur :accessor dur :initform 0  :initarg :dur :documentation "duration [sec]")
    (afil :initform nil :accessor afil :initarg :afil :documentation "audio file"))
   (:icon 603)
   (:documentation "Chant SOUND event.

Instanciates a sound file player in the interval determined by <action-time> and <dur> as a source signal in a Chant synthesis process.

<afil> = the sound file to play (pathname, SOUND object...)
"))

(defmethod initialize-instance ((self ch-snd) &rest args)
  (call-next-method)
  (when (and (null (ch-evt-duration self)) (afil self))
    (setf (dur self) (sound-dur (afil self))))
  self)

(defmethod ch-evt-duration ((self ch-snd))
  (when (listp (dur self))
    (om-beep-msg "Warning: CH-SND does not support fade-in/fade-out in CHANT"))
  (call-next-method))

(defmethod event-dur ((self ch-snd)) 
  (or (and (ch-evt-duration self) (plusp (ch-evt-duration self)) (ch-evt-duration self))
      (sound-dur (afil self))))

(defmethod evt-type-sid ((self ch-snd)) 3)
(defmethod evt-type-str ((self ch-snd)) "SOUND")

(defmethod snd-p ((self ch-snd)) t)
(defmethod snd-p ((self t)) nil)

(defmethod get-str-for-sound ((self t)) nil)
(defmethod get-str-for-sound ((self string)) self)
(defmethod get-str-for-sound ((self pathname)) (namestring self))
(defmethod get-str-for-sound ((self sound)) (namestring (om-sound-file-name self)))

(defmethod get-str-for-sound ((self ch-snd)) (get-str-for-sound (afil self)))

;;; CH-SND is just written in the SDIF header IDS
(defmethod evt-to-sdif ((self ch-snd)) nil)



;==================================================================
; ARRAY CLASSES : FOB and REB
;==================================================================

(defclass! chant-matrix-evt (chant-evt om::class-array) 
 ((dur :accessor dur :initform 0)
  (kt :accessor kt :initform nil)
  (user-fun :initform nil :accessor user-fun))
 (:icon 600))

(defmethod continuous-event-p ((self chant-matrix-Evt))
  (let ((continuous nil))
     (loop for field in (data self) 
          while (not continuous) do
          (when (find 'bpf (get-array-field-data field) :test 'subtypep :key 'type-of)
            (setf continuous t)))   
    continuous))

(defmethod get-obj-dur ((self chant-matrix-Evt)) 
   (round (* 1000
             (+ (max (ch-evt-duration self)
                     (event-dur self))
                (action-time self)))))


;;; duration of the event in seconds
(defmethod event-dur ((self chant-matrix-Evt))
  (let ((dur 0))
    (loop for field in (data self) do
          (loop for elt in (get-array-field-data field) do
                (when (subtypep (type-of elt) 'bpf)
                  (setf dur (max dur (car (last (x-points elt))))))
                ))
    (max (ch-evt-duration self) dur)))


  
;;; + 1 matrix after dur
(defun continuous-values (ch-evt)
  (let* ((times (cond ((bpf-p (kt ch-evt))
                       (remove-duplicates 
                        (loop for pts on (point-pairs (kt ch-evt)) by #'cdr
                              when (cadr pts) 
                              collect 
                              (arithm-ser (car (first pts)) (car (second pts)) (cadr (first pts))))
                        :test #'=))
                      ((numberp (kt ch-evt)) 
                       (arithm-ser 0.0 (event-dur ch-evt) (kt ch-evt)))
                      
                      (t (remove nil (sort (remove-duplicates 
                                            (cons (ch-evt-duration ch-evt)
                                                  (loop for field in (data ch-evt) append
                                                        (loop for val in (get-array-field-data field) 
                                                              append (when (bpf-p val) (x-points val))))
                                                  )) '<)))))
         (data (loop for timetag in times collect
                     (loop for elt from 0 to (1- (elts ch-evt)) collect
                           (loop for field in (data ch-evt)  collect
                                 (let ((val2 (nth elt (get-array-field-data field))))
                                   (if (bpf-p val2) 
                                       (if (< timetag (car (last (x-points val2))))
                                           (x-transfer val2 timetag)
                                         (car (last (y-points val2))))
                                     val2
                                     )))))))
    (values times (append data (car (last data))))))




;=================================
; CHANT MODULE : FOB
;=================================

(defclass! ch-fof (chant-matrix-Evt)
  ((elts :accessor elts :initform 1 :initarg :elts :documentation "number of elements (lines)")
   (action-time :accessor action-time :initform 0 :initarg :action-time :documentation "start time [sec]")
   (dur :accessor dur :initform 0 :initarg :dur :documentation "duration [sec]")
   (kt :accessor kt :initform nil :initarg :kt :documentation  "control period [sec]")
   (user-fun :accessor user-fun :initform nil :initarg :user-fun :documentation "lambda function applied to each component")
   
   (freq :initform 609.0 :type number :accessor freq :documentation "FOF frequency(-ies) [Hz]")
   (amp :initform 1.0 :type number :accessor amp  :documentation "FOF amplitudes !! Only linear [0.0 -> 1.0]") 
   (bw :initform 77.0 :type number :accessor bw :documentation "FOF bandwidth(s) [Hz]")
   (win :initform 0.003 :type number :accessor win :documentation "attack time(s) of the FOFs (aka. Tex) [sec]")      
   (wdur :initform 0.02 :type number :accessor wdur :documentation "duration(s) of the FOFs (aka. DebAtt) [sec]")
   (wout :initform 0.007 :type number :accessor wout :documentation "decay time(s) of the FOFs (aka. Atten) [sec]")
   (phs :initform 0 :type number :accessor phs :documentation "FOF phase")
   )
   (:icon 607)
   (:default-initargs :field-names '("freq" "amp" "bw" "win" "wdur" "wout" "phs"))
   (:documentation "A Chant FOF event represents the evolution of the FOF generator parameters during the interval specified by <action-time> and <dur>.

GENERAL CHANT-EVENTS PARAMETERS:

<elts> determines the number of FOFs in the generator.
<dur> can be either a duration (in seconds) or a list (duration fade-in fade-out) where fade-in and fade-out (in seconds) will be applied on the amplitude of the FOFs (<amp>).

<kt> allows to set a control rate for the homogeneous sampling of the continuous parameters.
<user-fun> allows to apply a function (lambda) to each component before to write the control file for the synthesizer.
  
CH-FOF SPECIFIC PARAMETERS (keywords):

<freq> = FOF frequency(-ies) [Hz]   
<amp> = FOF amplitudes !! Only linear [0.0 -> 1.0]     
<bw> = FOF bandwidth(s) [Hz]
<win> = attack time(s) of the FOFs [sec]
<wdur> = duration(s) of the FOFs [sec]
<wout> = decay time(s) of the FOFs [sec]
<phs> = FOF phase

Add keyword controls and name them ':channelX' (x = 1, 2,... n) in order to add channel distribution control for the different FOFs.

Note that CH-FOF MUST BE USED ALONG WITH AT LEAST ONE CH-F0 OBJECT, which determines the fundamental frequency of the FOF generator.
"))

;;; allows for these slots to be immediately visible on the box (even if the value/data) is not yet set
(defmethod additional-class-attributes ((self CH-FOF)) '(freq amp bw win wdur wout phs))

(defmethod evt-type-str ((self ch-fof)) "FOB")
(defmethod evt-type-sid ((self ch-fof)) 0)

(defmethod fob-p ((self ch-fof)) t)
(defmethod fob-p ((self t)) nil)


(defmethod get-obj-dur ((self ch-fof)) 
  (let ((dura (max (event-dur self)
                   (loop for onset in (get-field self "wdur")
                         for dur in (get-field self "wout")
                         maximize  (+ (if (bpf-p onset)
                                          (list-max (mapcar '+ (x-points onset) (y-points onset)))
                                        onset)
                                      (if (bpf-p dur) 
                                          (list-max (mapcar '+ (x-points dur) (y-points dur)))
                                        dur)) 
                         into max-elt-dur
                         finally (return max-elt-dur)))))
    (round (* (+ (action-time self) dura) 1000))))

     
#|
(defmethod synth-dur ((self ch-fof))
  (+ (call-next-method) 
     (loop for fofdur in (get-field self "wdur")
           maximize  (if (bpf-p fofdur)
                         (list-max (mapcar '+ (x-points fofdur) (y-points fofdur)))
                       fofdur)
           into max-fof-dur
           finally (return max-fof-dur))))


(defmethod synth-dur ((self ch-fof))
  (+ (call-next-method) 
     (loop for fofdur in (get-field self "wdur")
           for fofout in (get-field self "wout")
           maximize  (let ((dur 
                            (if (bpf-p fofdur)
                                (list-max (mapcar '+ (x-points fofdur) (y-points fofdur)))
                              fofdur))
                           (out
                             (if (bpf-p fofout)
                                 (list-max (mapcar '+ (x-points fofout) (y-points fofout)))
                               fofout)))
                       (+ dur out))
           into max-fof-dur
           finally (return max-fof-dur))))
|#

      
(defmethod evt-to-sdif ((self CH-FOF))
  (if (continuous-event-p self)
        
      (multiple-value-bind (times array-lists) 

          (continuous-values self)

        (loop for time in times
              for framevalues in array-lists 
              collect 
              (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) time) :streamid 0 
                             :lmatrix (remove nil
                                              (list 
                                               (om-make-sdifmatrix 
                                                "1FOF"
                                                (loop for i from 0 to (1- (elts self)) collect (first-n (nth i framevalues) 7)))
                                               (continuous-channel-panning-matrix self framevalues)
                                               )))))
    (remove nil 
            (append
             (if (ch-evt-fade-in self) 
                 (list (make-instance 'sdifframe :signature "1FOB" :ftime (get-absolute-time self) :streamid 0 
                                      :lmatrix (remove nil
                                                       (list (om-make-sdifmatrix 
                                                              "1FOF" 
                                                              (loop for i from 0 to (1- (elts self)) collect 
                                                                    (let ((data (first-n (get-array-element self i) 7)))
                                                                      (cons (car data) (cons 0 (cddr data))))))
                                                              
                                                             (static-channel-panning-matrix self)
                                                             )))
                       (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid 0 
                                      :lmatrix (remove nil
                                                       (list (om-make-sdifmatrix 
                                                              "1FOF" 
                                                              (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 7)))
                                                             (static-channel-panning-matrix self)
                                                             ))))
               (list
                (make-instance 'sdifframe :signature "1FOB" :ftime (get-absolute-time self) :streamid 0 
                               :lmatrix (remove nil
                                                (list (om-make-sdifmatrix 
                                                       "1FOF" 
                                                       (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 7)))
                                                      (static-channel-panning-matrix self)
                                                      )))))

             (if (plusp (ch-evt-duration self))
                 (if (ch-evt-fade-out self) 
                     (list 
                      (make-instance 'sdifframe :signature "1FOB" :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) :streamid 0 
                                     :lmatrix (remove nil 
                                                      (list (om-make-sdifmatrix 
                                                             "1FOF" 
                                                             (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 7)))
                                                            (static-channel-panning-matrix self)
                                                            )))
                      (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 0 
                                     :lmatrix (remove nil
                                                      (list (om-make-sdifmatrix 
                                                             "1FOF" 
                                                             (loop for i from 0 to (1- (elts self)) collect 
                                                                   (let ((data (first-n (get-array-element self i) 7)))
                                                                     (cons (car data) (cons 0 (cddr data))))))
                                                            (static-channel-panning-matrix self)
                                                            ))))

                   (list (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 0 
                                        :lmatrix (remove nil
                                                         (list (om-make-sdifmatrix 
                                                                "1FOF"  
                                                                (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 7)))
                                                               (static-channel-panning-matrix self)
                                                               ))))
                   )
               )))
    ))




    

;=================================
; CHANT MODULE : REB
;=================================

(defclass! ch-flt (chant-matrix-Evt) 
   ((elts :accessor elts :initform 1 :initarg :elts :documentation "number of elements (lines)")
    (action-time :accessor action-time :initform 0 :initarg :action-time :documentation "start time [sec]")
    (dur :accessor dur :initform 0 :initarg :dur :documentation "duration [sec]")
    (kt :accessor kt :initform nil :initarg :kt :documentation  "control period [sec]")
    (user-fun :accessor user-fun :initform nil :initarg :user-fun :documentation "lambda function applied to each component")

    (freq :initform 609.0 :type number :accessor freq :documentation "central frequency(-ies) [Hz]")   
    (amp :initform 1.0 :type number :accessor amp  :documentation "amplitude(s) of the filters(s) !! Only linear [0.0 -> 1.0]")
    (bw :initform 77.0 :type number :accessor bw :documentation "bandwidths [Hz]")
    (saliance :initform 1 :type number :accessor saliance) 
    (correction :initform 0 :type number :accessor correction)
   ) 
   (:icon 608)
   (:default-initargs :field-names '("freq" "amp" "bw" "saliance" "correction"))
   (:documentation "A Chant FILTER event represents the evolution of a filter bank parameters during the interval specified by <action-time> and <dur>.

GENERAL CHANT-EVENTS PARAMETERS:

<elts> determines the number of filters.
<dur> can be either a duration (in seconds) or a list (duration fade-in fade-out) where fade-in and fade-out (in seconds) will be applied on the amplitude of the filters (<amp>).

<kt> allows to set a control rate for the homogeneous sampling of the continuous parameters.
<user-fun> allows to apply a function (lambda) to each component before to write the control file for the synthesizer.
  
CH-FILTER SPECIFIC PARAMETERS (keywords):

<freq> = central frequency(-ies) [Hz]
<amp> = amplitude(s) of the filters(s) !! Only linear [0.0 -> 1.0]
<bw> = bandwidths [Hz]
<saliance>
<correction>  

Add keyword controls and name them ':channelX' (x = 1, 2,... n) in order to add channel distribution control for the different filters.
"))

;;; allows for these slots to be immediately visible on the box (even if the value/data) is not yet set
(defmethod additional-class-attributes ((self ch-flt)) '(freq amp bw saliance correction))

(defmethod evt-type-str ((self ch-flt)) "FILTER")
(defmethod evt-type-sid ((self ch-flt)) 1)

(defmethod reb-p ((self ch-flt)) t)
(defmethod reb-p ((self t)) nil)

(defvar *filt-limit* 1000)

(defmethod get-obj-dur ((self ch-flt)) 
   (round (* 1000
             (+ (max ;(/ (log *filt-limit*) (* pi (apply 'min (get-field self "bw") ))) 
                     (ch-evt-duration self)
                     (event-dur self))
                (action-time self)))))


(defmethod evt-to-sdif ((self ch-flt))
  
  (if (continuous-event-p self)
        
        (multiple-value-bind (times array-lists) 
            (continuous-values self)
          (loop for time in times
                for framevalues in array-lists 
                collect 
                (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) time) :streamid 1 
                               :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                           "1RES" 
                                                           (loop for i from 0 to (1- (elts self)) collect (first-n (nth i framevalues) 5)))
                                                          (continuous-channel-panning-matrix self framevalues)
                                                          )))))

      (remove nil
              (append
               (if (ch-evt-fade-in self) 
                   (list 
                    (make-instance 'sdifframe :signature "1REB" :ftime (get-absolute-time self) :streamid 1 
                                   :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                               "1RES" 
                                                               (loop for i from 0 to (1- (elts self)) collect 
                                                                     (let ((data (first-n (get-array-element self i) 5)))
                                                                       (cons (car data) (cons 0 (cddr data))))))
                                                              (static-channel-panning-matrix self)
                                                              )))

                    (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid 1 
                                   :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                               "1RES" 
                                                               (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 5)))
                                                              (static-channel-panning-matrix self)
                                                              )))
                    )
                 (list
                  (make-instance 'sdifframe :signature "1REB" :ftime (get-absolute-time self) :streamid 1 
                                 :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                             "1RES" 
                                                             (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 5)))
                                                            (static-channel-panning-matrix self)
                                                            )))))

               (if (plusp (ch-evt-duration self))
                   (if (ch-evt-fade-out self) 
                       (list 
                        (make-instance 'sdifframe :signature "1REB" :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) :streamid 1 
                                       :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                                   "1RES" 
                                                                   (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 5)))
                                                                  (static-channel-panning-matrix self)
                                                                  )))
                        (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 1 
                                       :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                                   "1RES" 
                                                                   (loop for i from 0 to (1- (elts self)) collect 
                                                                         (let ((data (first-n (get-array-element self i) 5)))
                                                                           (cons (car data) (cons 0 (cddr data))))))
                                                                  (static-channel-panning-matrix self)
                                                                  )))
                        )

                     (list (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 1 
                                          :lmatrix (remove nil (list (om-make-sdifmatrix 
                                                                      "1RES" 
                                                                      (loop for i from 0 to (1- (elts self)) collect (first-n (get-array-element self i) 5)))
                                                                     (static-channel-panning-matrix self)
                                                                     )))
                           )
                     )
                 )))
      ))




;=================================
; CHANT PATCH
;=================================

;;; this variable will be set during synthesis, depending on teh other events...
(defvar *chant-patch* nil)

; 0 = FOF
; 1 = FILTER
; 2 = NOISE
; 3 = SND 
(defun ev-type-in-patch (n)
  (case n
    (0 '(0 1 3 6 7 8 9 10))
    (1 '(1 2 3 4 5 6 7 8 9 10))
    (2 '(1 2 3 4 8 9))
    (3 '(2 3 5 7 8 10))))

(defun deduce-patch-from-evts (evts)
  (let ((grouped-evts (cons (append (car evts) (cadr evts))  ;; group fof and f0
                            (cddr evts))) 
         (ev-types nil))
    (dotimes (i (length grouped-evts) ev-types) (if (nth i grouped-evts) (pushr i ev-types)))
    (patch-from-ev-types ev-types)))

(defun patch-from-ev-types (ev-types)
  (cond ((equal ev-types '(0)) 0)
        ((equal ev-types '(0 1 2)) 1) ;;; or 9
        ((equal ev-types '(1 2 3)) 2)
        ((equal ev-types '(0 1 2 3)) 3)  ;;; or 8
        ((equal ev-types '(1 2)) 4)
        ((equal ev-types '(1 3)) 5)
        ((equal ev-types '(0 1)) 6)
        ((equal ev-types '(0 1 3)) 7) ;;; or 10
        (t nil)))

(defun patch-ev-types (patchn)
  (case patchn
    (0 '(0))
    (1 '(0 1 2))
    (2 '(1 2 3))
    (3 '(0 1 2 3))
    (4 '(1 2))
    (5 '(1 3))
    (6 '(0 1))
    (7 '(0 1 3))
    (8 '(0 1 2 3))
    (9 '(0 1 2))
    (10 '(0 1 3))))

(defun type-to-frame-signature (typenum)
  (case typenum 
    (0 "1FOB")
    (1 "1REB")
    (2 "1NOI")
    (otherwise NIL)))

(defun check-events-for-patch (sorted-events patch)
  (let ((fof (first sorted-events))
        (f0 (second sorted-events))
        (reb (third sorted-events))
        (noi (fourth sorted-events))
        (snd (fifth sorted-events)))
    
    (cond ((and fof (not (find patch (ev-type-in-patch 0) :test '=)))
           (om-beep-msg (format nil "Warning : FOFevents are NOT USED in Chant patch ~D !" patch)))
          ((and (not f0) (find patch (ev-type-in-patch 0) :test '=))
           (om-beep-msg (format nil "Warning : FOF events are REQUIRED in Chant patch ~D !" patch)))
          )

    (cond ((and f0 (not (find patch (ev-type-in-patch 0) :test '=)))
           (om-beep-msg (format nil "Warning : F0 events are NOT USED in Chant patch ~D !" patch)))
          ((and (not f0) (find patch (ev-type-in-patch 0) :test '=))
           (om-beep-msg (format nil "Warning : F0 events are REQUIRED in Chant patch ~D !" patch)))
          )
          
    (cond ((and reb (not (find patch (ev-type-in-patch 1) :test '=)))
           (om-beep-msg (format nil "Warning : FILTER events are NOT USED in Chant patch ~D !" patch)))
          ((and (not reb) (find patch (ev-type-in-patch 1) :test '=))
           (om-beep-msg (format nil "Warning : FILTER events are REQUIRED in Chant patch ~D !" patch))))

    (cond ((and noi (not (find patch (ev-type-in-patch 2) :test '=)))
           (om-beep-msg (format nil "Warning : NOISE events are NOT USED in Chant patch ~D !" patch)))
          ((and (not noi) (find patch (ev-type-in-patch 2) :test '=))
           (om-beep-msg (format nil "Warning : NOISE events are REQUIRED in Chant patch ~D !" patch))))

    (cond ((and snd (not (find patch (ev-type-in-patch 3) :test '=)))
           (om-beep-msg (format nil "Warning : SOUND is NOT USED in Chant patch ~D !" patch)))
          ((and (not snd) (find patch (ev-type-in-patch 3) :test '=))
           (om-beep-msg (format nil "Warning : SOUND is REQUIRED in Chant patch ~D !" patch)))
          ((> (length snd) 1)
           (om-beep-msg (format nil "Warning : Only one SOUND can be used at a time in Chant patch ~D !" patch))))
    
    (values fof reb noi snd)
    ))


(defun split-list (list)
   (let (lfof lf0 lreb lnoi lsnd)
     (loop for item in list do
           (cond
            ((reb-p item) (push item lreb))
            ((fob-p item) (push item lfof))
            ((f0-p item) (push item lf0))
            ((noi-p item) (push item lnoi))
            ((snd-p item) (push item lsnd)) 
            ))
     (mapcar #'(lambda (list) (sort list '< :key 'get-absolute-time))
             (list lfof lf0 lreb lnoi lsnd))
      ))


(defmethod synth-dur ((self t)) (event-dur self))

(defun write-CHANT-IDStables (file patch sortedevents durtot)
  
  (declare (ignore durtot))
  
  (multiple-value-bind (fofs rebs nois snds) 
    
      (check-events-for-patch sortedevents patch)
    
    (let (nc st en)
      
      (when fofs
        (loop for evt in fofs 
              maximize (elts evt) into n
              minimize (get-absolute-time evt) into b
              maximize (+ (get-absolute-time evt) (synth-dur evt)) into e
              finally (setf st b en e nc n))
        
        (sdif-write-IDS file 0 "Chant" (format nil "Patch~D/1/FOB/~D/~D/~F/~F" patch 1 nc st en)))
                                              
      (when rebs
        (loop for evt in rebs 
              maximize (elts evt) into n
              minimize (get-absolute-time evt) into b
              maximize (+ (get-absolute-time evt) (event-dur evt)) into e
              finally (setf st b en e nc n))
        
        (sdif-write-IDS file 1 "Chant" (format nil "Patch~D/1/REB/~D/~D/~F/~F" patch 1 nc st en)))
                                              
      (when nois
        (loop for evt in nois 
              minimize (get-absolute-time evt) into b
              maximize (+ (get-absolute-time evt) (event-dur evt)) into e
              finally (setf st b en e))
        
        (sdif-write-IDS file 2 "Chant" (format nil "Patch~D/1/NOI/~D/0/~F/~F" patch 1 st en)))
                                    
      (when snds
        (loop for evt in snds 
              minimize (action-time evt) into b
              maximize (+ (action-time evt) (event-dur evt)) into e
              finally (setf st b en e))
       
        (sdif-write-IDS file 3 "Chant" (format nil "Patch~D/1/SND/~D/1/~F/~F/~S" patch 1 st en
                                               (get-str-for-sound (car snds))))
        )
      
      )))


;=================================
; CHANNELS INSIDE CH-EVENTS
;=================================

(defmethod extra-channel-fields ((self chant-matrix-Evt))
  (let ((rep ()))
    (loop for field in (data self) 
          for i from 0 do
          (let ((ctlname (array-field-name field)))
            (when (and (> (length ctlname) 4)
                       (string-equal "chan" (subseq ctlname 0 4)))
              (push i rep))))
    (reverse rep)))

(defmethod get-channels-num ((self chant-matrix-Evt)) 
   (max 1 (length (extra-channel-fields self))))

;;; this variable is set during sythesis.
;;; if there are no explicit channel events, we add a default one
(defvar *channel-events* nil)

(defmethod continuous-channel-panning-matrix ((self chant-matrix-Evt) framevalues)
  (let ((chans (extra-channel-fields self)))
    (if chans 
        (om-make-sdifmatrix 
         "1CHA" 
         (loop for i from 0 to (1- (elts self)) collect 
               (loop for ch in chans collect (or (nth ch (nth i framevalues)) 1))))
      (unless *channel-events*
        (om-make-sdifmatrix "1CHA" (make-list (elts self) :initial-element (list 1)))
        ))
    ))

(defmethod static-channel-panning-matrix ((self chant-matrix-Evt))
  (let ((chans (extra-channel-fields self)))
    (if chans 
        (om-make-sdifmatrix 
         "1CHA" 
         (loop for i from 0 to (1- (elts self)) collect 
               (loop for ch in chans collect (or (nth i (get-array-field-data (nth ch (data self)))) 1))))
      (unless *channel-events*
        (om-make-sdifmatrix "1CHA" (make-list (elts self) :initial-element (list 1)))
        ))
    ))



;=================================
; CHANT SPECIAL MODULE : CHANNELS
;=================================

(defclass! ch-channels (chant-matrix-Evt) 
  ((elts :accessor elts :initform 1 :initarg :elts :documentation "number of elements (lines)")
   (action-time :accessor action-time :initform 0 :initarg :action-time :documentation "start time [sec]")
   (dur :accessor dur :initform 0 :initarg :dur :documentation "duration [sec]")
   (kt :accessor kt :initform nil :initarg :kt :documentation  "control period [sec]")
   (user-fun :accessor user-fun :initform nil :initarg :user-fun :documentation "lambda function applied to each component"))
   (:icon 610)
   (:documentation "CH-CHANNEL is a special Chant event allowing to control the distribution of the chant modules' output on a given time interval, and independently of these modules.

Add :chanX (e.g. :channel1, :channel2,...) keyword controls for the different channels and determine the distribution of the different components on these channels (using constant alues of BPFs)."))



(defmethod evt-to-sdif ((self ch-channels))
  
  (let ((frametypes (remove '(2 3) (patch-ev-types *chant-patch*) 
                            :test #'(lambda (test item) (find item test)))))  
    ;;; we don't want noise or snd frames for channels
   
    (if (continuous-event-p self)
        (multiple-value-bind (times array-lists) 
            (continuous-values self)
          (loop for time in times
                for framevalues in array-lists 
                append
                (loop for sid in frametypes collect
                      (make-instance 'sdifframe 
                                     :signature (type-to-frame-signature sid) 
                                     :ftime (+ (get-absolute-time self) time) 
                                     :streamid sid 
                                     :lmatrix (list (continuous-channel-panning-matrix self framevalues))))
                ))

      (remove nil
              (loop for sid in frametypes append
                    (append
                     (if (ch-evt-fade-in self) 
                         (list 
                          (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                         :ftime (get-absolute-time self) 
                                         :streamid sid 
                                         :lmatrix (list (static-channel-panning-matrix self)))
                          (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                         :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) 
                                         :streamid sid 
                                         :lmatrix (list (static-channel-panning-matrix self)))
                          )
                       (list
                        (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                       :ftime (get-absolute-time self) 
                                       :streamid sid 
                                       :lmatrix (list (static-channel-panning-matrix self)
                                                      ))))
                     
                     (if (plusp (ch-evt-duration self))
                         (if (ch-evt-fade-out self) 
                             (list 
                              (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                             :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) 
                                             :streamid sid 
                                             :lmatrix (list (static-channel-panning-matrix self)))
                              (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                             :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                             :streamid sid 
                                             :lmatrix (list (static-channel-panning-matrix self)))
                              )

                           (list (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                                :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                                :streamid sid 
                                                :lmatrix (list (static-channel-panning-matrix self)))
                                 )
                           )
                       ))))
      )))



(defmethod! gen-chant-channels (list &optional nc)
  :icon 610
  :initvals '((((0 (0 1))) ((0 (1 0)))) nil)
  :indoc '("formatted list of formant distribution" "number of components (formants)")
  :outdoc '("a CH-CHANNEL instance")
  :doc "Generates a CH-CHANNELS instance starting from a list of formants' distribution over a number of speakers.

Each formant's distribution is itself formatted as a list of timed distributions.

E.G.:
'(
 ; formant 1
 ((t0 (speaker1 speaker2 speaker3 speaker4))
  (t1 (speaker1 speaker2 speaker3 speaker4))
 ; ...
  (tn (speaker1 speaker2 speaker3 speaker4)))
 ; formant 2
 ((t0 (speaker1 speaker2 speaker3 speaker4))
  (t1 (speaker1 speaker2 speaker3 speaker4))
 ; ...
  (tn (speaker1 speaker2 speaker3 speaker4)))
 ; ...
 )

Each speakerN is a value between 0.0 aand 1.0.
"
  
  (let ((add-fields (loop for i from 1 to (length (cadr (caar list))) ;;; = the first n-speakers 
                          collect (string-upcase (format nil "chan~D" i)))))
    (om-init-instance 
     (make-instance 'ch-channels
                    :elts (or nc (length list)) 
                    :dur (car (last (car list)))  ;;; = the first tn
                    :field-names add-fields)
     
     (loop for chan-field in add-fields 
           for i from 1 
           collect (list (intern chan-field :keyword)
                      (loop for formant in list collect
                            (make-instance 'bpf :x-points (mapcar 'car formant) ;; (t0 t1 ...)
                                           :y-points (mapcar #'(lambda (state) (nth (1- i) (cadr state))) formant)  
                                           :decimals 3))
                      ))
     )
    ))
    



;=================================
; GEN-INTER-EVENTS
;=================================

;;; ev1 et ev2 should be chant-matrix-evt
(defmethod! gen-inter-event ((ev1 t) (ev2 t) rules &optional (delta *min-delta-frames*))
  :indoc '("event 1" "event 2" "rules (list)")
  :outdoc '("an new transition event")
  :icon 645
  :doc "Generates an event between <ev1> and <ev2> (which must be of the same type) following transition rules.

<rules> can be one or several lists formatted as (PARAM VALUE)
where PARAM can be one of the slot or a specific element in this slot (e.g. :freq or (:freq 2))
and VALUE is either av constant value or a function (patch) of 2 arguments specifying the transition from the end value of ev1 to the start of ev2.

All non-specified transitions are linear.
"
  (let* ((start (+ (get-absolute-time ev1) (event-dur ev1) delta))
         (dur (- (get-absolute-time ev2) start delta))
         (args (loop for slot in (mapcar #'array-field-name (data ev1)) collect
                     ;;; ex. for each slot in '(freq amp bw ...)
                     (let* ((slot-k (intern-k slot))
                            (slotrules (loop for rule in rules 
                                             when (if (listp (car rule))
                                                      (equal (intern-k (car (car rule))) slot-k)
                                                    (equal (intern-k (car rule)) slot-k))
                                             collect rule))
                            ;;; slot rules = (freq 400) or (freq 'my-freq-fun) or ((freq 1) ...)
                            (slotvals (loop for v1 in (get-field ev1 slot) 
                                            for v2 in (get-field ev2 slot) collect
                                            (list (if (bpf-p v1) (last-elem (y-points v1)) v1)
                                                  (if (bpf-p v2) (car (y-points v2)) v2)
                                                  nil ;;; HERE WILL BE STORED THE RESULT OF THE RULE
                                                  ))))

                       (when slotrules 
                         (om-print-dbg  (format nil "RULE(S) for ~A:" slot) nil "OM-CHANT")
                         (loop for r in slotrules do (om-print-dbg (format nil "~A" r) nil "OM-CHANT")))

                       ;;; at this point slotvals = ((f1a f1b) (f2a f2b) ...)
                       (loop for slotrule in slotrules do
                             (if (listp (car slotrule)) ;;; Ex. ((:freq 2) ...) 
                                 (setf (third (nth (cadr (car slotrule)) slotvals))
                                       (if (or (functionp (cadr slotrule))
                                               (and (symbolp (cadr slotrule)) (fboundp (cadr slotrule))))
                                           (apply (cadr slotrule) (list (car (nth (cadr (car slotrule)) slotvals))
                                                                        (cadr (nth (cadr (car slotrule)) slotvals))))
                                      
                                         (cadr slotrule)))
                               (setf slotvals (loop for element in slotvals collect
                                                    (list (car element) (cadr element)
                                                          (if (or (functionp (cadr slotrule))
                                                                  (and (symbolp (cadr slotrule)) (fboundp (cadr slotrule))))
                                                              (apply (cadr slotrule) 
                                                                     (list (car element) (cadr element)))
                                                            (cadr slotrule)))))))
                       ;;; this is what is actually collected :
                       (list slot-k
                             (if (listp slotvals) 
                                 (loop for item in slotvals collect 
                                       (if (third item)
                                           (cond ((bpf-p (third item))
                                                  (bpf-scale (third item) :x1 0 :x2 dur))
                                                 (t (third item)))
                                                                  
                                         (om-make-bpf 'bpf (list 0 dur) (list (car item) (cadr item)) 5))
                                       )
                               slotvals))))))
    
     (om-init-instance (make-instance (type-of ev1)
                                      :elts (elts ev1) 
                                      :action-time start
                                      :dur dur)
                       args)
     ))
                      



;=================================
; SYNTHESIS
;=================================


(defparameter *auto-delta-frames* t)
(defparameter *min-delta-frames* 0.001)

(defun adjust-frames (frameslist)
  (if *auto-delta-frames*
      (let ((newlist nil))
        (loop for fr in (sort frameslist '< :key 'ftime)
              do (let ((duplicate (find fr newlist :test #'(lambda (f1 f2) 
                                                             (and (= (streamid f1) (streamid f2))
                                                                  (= (ftime f1) (ftime f2))
                                                                  (string-equal (matrixtype (car (lmatrix f1))) (matrixtype (car (lmatrix f2))))
                                                                  )))))
                   (when duplicate 
                     (om-beep-msg (format nil "WARNING: duplicate time for ~A at ~D" (matrixtype (car (lmatrix fr))) (ftime fr)))
                     (setf (ftime fr) (+ (ftime fr) *min-delta-frames*)))
                   (setf newlist (append newlist (list fr)))))
        newlist)
    frameslist))

(defun pack-frames (frameslist)
  (let ((frameslist (adjust-frames frameslist))
        (tmpfobframes nil)
        (fobframes nil)
        (otherframes nil))
    (loop for fr in frameslist 
          do (if (= (streamid fr) 0) 
                 (push fr tmpfobframes)
               (push fr otherframes)))
    (loop for fobfr in tmpfobframes do
          (let ((pos (position (ftime fobfr) fobframes :test '= :key 'ftime)))
            (if pos 
                (setf (lmatrix (nth pos fobframes)) (append (lmatrix (nth pos fobframes)) (lmatrix fobfr)))
              (push fobfr fobframes))))
    (sort (append otherframes fobframes) '< :key 'ftime)))
     

(defmethod apply-user-fun ((self t)) self)

(defmethod apply-user-fun ((self chant-matrix-evt))
  (if (user-fun self)
      (let ((rep (clone self)))
        (loop for f in (list! (user-fun self)) do
              (loop for i from 0 to (1- (elts self)) do
                    (funcall f rep i)))
        rep)
    self))
                 

(defmethod collect-chant-events ((self t) num) nil)
(defmethod collect-chant-events ((self chant-evt) num) self)
(defmethod collect-chant-events ((self list) num) 
   (remove-if 'null (loop for object in self collect (collect-chant-events object num))))


;; Call this with identified objects, before to send to synthesize
;; Returns FOFs and F0s
;; sorts input items, removes duplicates.
(defmethod filter-ch-ids ((elts list))
  (let* ((fofs nil) (f0s nil) (others nil))
    (loop for elt in elts do
          (cond ((subtypep (type-of elt) 'ch-fof) (push elt fofs))
                ((typep elt 'ch-f0) (push elt f0s))
                (t (push elt others))))
    ;; elimine une fof si son id est inclus dans l'id d'une autre fof (morphing ou trajectoire)
                ;(print (mapcar 'id fofs))

    (setf fofs (remove-if #'(lambda (x) 
                              (find x fofs 
                                    :test #'(lambda (x y) (and (search (id x) (id y)) 
                                                               (not (string-equal (id x) (id y))))))
                              ) fofs))
    ;; elimine une f0 si son id est inclus dans l'id d'une autre f0 
    (setf f0s (remove-if #'(lambda (x) 
                             (find x f0s 
                                   :test #'(lambda (x y) (and (search (id x) (id y)) 
                                                              (not (string-equal (id x) (id y))))))
                             ) f0s))
    (sort (append fofs f0s others) '< :key 'get-absolute-time)
    ))


(defclass! chant-patch-init ()
  ((patch-duration :accessor patch-duration :initarg :patch-duration :initform nil)
   (patch-num :accessor patch-num :initarg :patch-num :initform nil))
  (:icon 600))
   
(defmethod get-patch-num ((val integer)) val)
(defmethod get-patch-num ((val chant-patch-init)) (patch-num val))
(defmethod get-patch-num ((val t)) nil)
(defmethod get-patch-duration ((val float)) val)
(defmethod get-patch-duration ((val chant-patch-init)) (patch-duration val))
(defmethod get-patch-duration ((val t)) nil)


(defmethod! ch-synthesize ((events t) 
                           &key (name "my-synth") (run t) format
                           inits tables
                           resolution normalize 
                           sr kr)
  
  (declare (ignore kr tables))

  (let* ((patch-num (get-patch-num inits)) ;;; by convention in chant-synthesize: inits is the patch number (if specified)
         (patch-dur (get-patch-duration inits))
         (sound-format (or format (chant-get-default-audio-format)))
         (path-sdif (handle-new-file-exists (tmpfile (string+ (if (pathnamep name) (pathname-name name) name) ".sdif"))))
         (path-aiff (handle-new-file-exists (cond ((pathname-directory (pathname name)) (pathname name))
                                                  ((pathname-type (pathname name)) (om::outfile name))
                                                  (t (om::outfile name :type (string-downcase sound-format)))))))
                                                  
    
    (setf events (filter-ch-ids (collect-chant-events (remove nil (list! events)) patch-num)))

    (if (null events)
                  
        (om-beep-msg "No synthesis events !")
                
      ;;; main body:
      (let* ((thefile (sdif::sdif-open-file (namestring path-sdif) sdif::eWriteFile))
             (ch-num (loop for item in events maximize (get-channels-num item)))
             (samplerate (or sr (chant-get-default-audio-sr) 44100))
             (resol (or resolution (chant-get-default-audio-res) 16))
             (sortedevents (split-list events))
                       
             (*chant-patch* (or patch-num (deduce-patch-from-evts sortedevents) 0))
             (*channel-events* (find 'ch-channels events :key 'type-of))
             (total-dur  (or patch-dur
                             (loop for item in events maximize
                                   (+ (get-absolute-time item) (synth-dur item))
                                             ;(/ (get-obj-dur item) 1000.0)
                                   )))
             (nvt  (mat-trans (list (list "BufferSize" "512")
                                    (list "NumberOfChannels" (number-to-string ch-num))
                                    (list "EndTime" (format nil "~D" total-dur))
                                    (list "Author" (string+ "OM " *version-string*))
                                    (list "SamplingRate" (format nil "~F" samplerate))
                                    )))
                       ;(sortedevents (split-list events))
                       ;(events-and-silence (handle-inter-events sorted-events))
             (sdifframes (pack-frames (loop for item in events append (evt-to-sdif (apply-user-fun item))))))
                
        (om-print "=========================" "OM-Chant")
        (om-print (format nil "CHANT synthesis patch: ~D" *chant-patch*) "OM-Chant")
                       
        (sdif::SdifFWriteGeneralHeader thefile)
        
        (when (> ch-num 2)
          ;;; need to declare all channels > 2
          (let ((typedef-str "{ 1MTD 1CHA {"))
            (loop for c = 3 then (+ c 1) while (<= c ch-num) do
                  (setf typedef-str (string+ typedef-str " Channel" (number-to-string c) ",")))
            (setf typedef-str (string+ typedef-str " } }"))
            (write-sdif-types thefile typedef-str)
            ))
        
        (write-1nvt-table thefile (car nvt) (cadr nvt))
        (write-CHANT-IDStables thefile *chant-patch* sortedevents total-dur)
        (sdif::SdifFWriteAllASCIIChunks thefile)

        (sdif-write sdifframes thefile)

        (sdif::SDIFFClose thefile)
                  
        (if run
                      
            (progn
              (add-tmp-file path-sdif)
              (setf path-aiff (chant-synth path-sdif 
                                           :outfile path-aiff 
                                           :resolution resol 
                                           :normalize-level normalize
                                           :out-format sound-format))
              (maybe-clean-tmp-files)
              path-aiff)
                       
          (probe-file path-sdif)
          )
        ))))


;;;==========================
;;; OM-SYNTHESIZE INTERFACE
;;;==========================

(defmethod synthesize-method ((self chant-evt)) 'ch-synthesize)


