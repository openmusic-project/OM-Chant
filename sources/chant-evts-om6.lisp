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

(defclass chant-evt () ((id :accessor id :initform 0)))

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

(defclass! chant-simple-evt (chant-evt internSynthEvt)
           ((action-time :initform 0 :accessor action-time :initarg :action-time :documentation "start time [sec]")
            (dur :accessor dur :initform 0  :initarg :dur :documentation "duration [sec]")))

(defmethod get-obj-dur ((self chant-simple-evt)) 
  (max 1 (round (* 1000 (+ (action-time self) (event-dur self))))))

(defmethod get-absolute-time ((self chant-evt))
  (+ (action-time self) (/ (offset self) 1000.0)))

(defmethod end-time ((self chant-evt))
  (+ (action-time self) (event-dur self)))

(defmethod get-channels-num ((self chant-simple-evt)) 1)
(defmethod extra-channels ((self chant-simple-evt)) 0)
(defmethod numcols ((self chant-simple-evt)) 1)


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

(defclass! ch-f0 (chant-simple-evt simple-container)
   ((f0 :initform 110.0 :accessor f0  :initarg :f0 :documentation "fundamental frequency value(s) [Hz]"))
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
  ;(let ((duree (* 1000 (dur self)))) ; (extent->ms self)
    (cond ((numberp (f0 self))
           (remove nil
                   (append 
                    (if (ch-evt-fade-in self) 
                      (list (make-instance 'sdifframe :signature "1FOB" :ftime (get-absolute-time self) 
                                           :streamid 0 
                                           :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                                         :data (list 0))))
                            (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self))
                                   :streamid 0 
                                   :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                                 :data (list (f0 self))))))
                      (list (make-instance 'sdifframe :signature "1FOB" :ftime (get-absolute-time self) 
                                           :streamid 0 
                                           :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                                         :data (list (f0 self)))))))
                    
                    (when (plusp (ch-evt-duration self))
                       (if (ch-evt-fade-out self) 
                           (list
                            (make-instance 'sdifframe :signature "1FOB" :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self))
                                           :streamid 0 
                                           :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                                         :data (list (f0 self)))))
                            (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                           :streamid 0 
                                           :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                                         :data (list 0)))))
                         (list
                          (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                         :streamid 0 
                                         :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                                       :data (list (f0 self)))))
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
                         :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1FQ0" :num-elts 1 :num-fields 1
                                                       :data (list f0))))
               ))
    
        )))

(defmethod draw-obj-in-rect ((self Ch-F0) x x1 y y1 edparams view)
  (draw-obj-in-rect (f0 self) x x1 y y1 edparams view))

(defclass ch-bpfeditor (bpfeditor) ())

(defmethod class-has-editor-p ((self Ch-F0)) t)
(defmethod get-editor-class ((self Ch-F0)) 'ch-bpfeditor)

(defmethod make-editor-window ((class (eql 'ch-bpfeditor)) (object ch-f0) name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
  (if (bpf-p (f0 object))
      (call-next-method class (f0 object) name ref :winsize winsize :winpos winpos :resize resize 
                        :close-p close-p :winshow winshow :resize resize
                        :retain-scroll retain-scroll :wintype wintype)
    (progn (om-beep)
      nil)))

(defmethod update-editor-after-eval ((Self Bpfeditor) (val ch-f0))
  (if (bpf-p (f0 val))
      (call-next-method self (f0 val))
    (om-close-window (window self))))


;=================================
; CHANT MODULE : NOISE
;=================================

(defclass! ch-noise (chant-simple-evt simple-container)
   (; (dist :initform 0 :initarg :dist :type number :accessor dist)
    (amp :initform 1.0 :initarg :amp :type number :accessor amp :documentation "!! Only linear [0.0 -> 1.0]"))
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

(defmethod draw-obj-in-rect ((self ch-noise) x x1 y y1 edparams view)
  (draw-obj-in-rect (amp self) x x1 y y1 edparams view))


(defmethod evt-to-sdif ((self ch-noise))
  (let ((duree (* 1000 (ch-evt-duration self)))) ; (extent->ms self)
    (cond ((numberp (amp self))
           (remove nil
                   (append 
                    (if (ch-evt-fade-in self) 
                      (list (make-instance 'sdifframe :signature "1NOI" :ftime (get-absolute-time self) :streamid 2 
                                 :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                               :data (list 0 0))))
                            (make-instance 'sdifframe :signature "1NOI" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid 2 
                                 :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                               :data (list 0 (amp self))))))
                      (list (make-instance 'sdifframe :signature "1NOI" :ftime (get-absolute-time self) :streamid 2 
                                 :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                               :data (list 0 (amp self)))))))
                    
                    (when (plusp (ch-evt-duration self))
                       (if (ch-evt-fade-out self) 
                           (list
                            (make-instance 'sdifframe :signature "1NOI" :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) 
                                     :streamid 2 
                                     :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                                   :data (list 0 (amp self)))))
                            
                            (make-instance 'sdifframe :signature "1NOI" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                     :streamid 2 
                                     :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                                   :data (list 0 0)))))
                         (list
                          (make-instance 'sdifframe :signature "1NOI" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) 
                                     :streamid 2 
                                     :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                                   :data (list 0 (amp self)))))
                          )))
                    )
                 )
         )
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
                         :lmatrix (list (make-instance 'raw-sdifmatrix :signature "1DIS" :num-elts 1 :num-fields 2
                                                       :data (list 0 a))))
               )))

        )))

(defmethod class-has-editor-p ((self ch-noise)) t)
(defmethod get-editor-class ((self ch-noise)) 'ch-bpfeditor)

(defmethod make-editor-window ((class (eql 'ch-bpfeditor)) (object ch-noise) name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
  (if (bpf-p (amp object))
      (call-next-method class (amp object) name ref :winsize winsize :winpos winpos :resize resize 
                        :close-p close-p :winshow winshow :resize resize
                        :retain-scroll retain-scroll :wintype wintype)
    (progn (om-beep)
      nil)))

(defmethod update-editor-after-eval ((Self Bpfeditor) (val ch-noise))
  (if (bpf-p (amp val))
      (call-next-method self (amp val))
    (om-close-window (window self))))



;=================================
; CHANT MODULE : SOUND
;=================================

(defclass! ch-snd (chant-simple-evt simple-container)
   ((afil :initform nil :accessor afil :initarg :afil :documentation "audio file"))
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
(defmethod get-str-for-sound ((self sound)) (namestring (om-sound-file-name  self)))

(defmethod get-str-for-sound ((self ch-snd)) (get-str-for-sound (afil self)))

(defmethod evt-to-sdif ((self ch-snd)) nil)

;=================================
; ARRAY CLASSES : FOB and REB
;=================================

(defclass! chant-matrix-evt (SynthesisEvt chant-evt) 
 ((dur :accessor dur :initform 0)
  (kt :accessor kt :initform nil)
  (user-fun :initform nil :accessor user-fun))
 (:icon 600))

(defmethod fixed-slots-list ((self chant-matrix-Evt)) '(numcols action-time dur kt user-fun))

;;; temp compatibility
(defmethod (setf dur) (dur (self t)) t)
(defmethod (setf kr) (kr (self t)) t)

(defmethod omNG-copy ((self chant-matrix-Evt))
   `(let ((copy ,(call-next-method)))
        (when copy 
          (setf (dur copy) ',(dur self))
          (setf (kt copy) ,(kt self))
          copy)))

(defmethod omNG-save ((self chant-matrix-Evt) &optional (values? nil))
    `(let ((copy ,(call-next-method)))
       (when copy 
         (setf (dur copy) ',(dur self))
         (setf (kt copy) ,(kt self)))
      copy))
        
;;; + 2 slots for duration and kr
;(defmethod matrix-gen-code ((self arrayBox) (val chant-matrix-Evt))
;   (let* ((params (decode self))
;          (fixinputs (length (fixed-slots-list val)))
;          (theval (if (connected? (first (inputs self)))
;                    `(objFromObjs ,(gen-code (first (inputs self)) (second (inputs self))) ,val)
;                   `(funcall 'cons-array ,val 
;                             (list nil ,(second params) ,(third params) ,(fourth params) ,(fifth params))
;                             (list ,.(nthcdr (1+ fixinputs) params))))))
;     `(let ((array ,theval))
;        (set-data array)
;        array)))
      

(defmethod continuous-event-p ((self chant-matrix-Evt))
  (let ((continuous nil))
    (loop for i from 0 to (1- (numrows self))
          while (not continuous) do
          (when (find 'bpf (get-array-row self i) :test 'subtypep :key 'type-of)
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
    (loop for i from 0 to (1- (numrows self)) do
          (loop for elt in (get-array-row self i) do
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
                      
                      (t (remove nil (sort (remove-duplicates (cons (ch-evt-duration ch-evt)
                                                           (loop for row from 0 to (1- (numrows ch-evt)) append
                                                                 (loop for col from 0 to (1- (numcols ch-evt)) 
                                                                       append (let ((val (get-array-val ch-evt row col)))
                                                                                (when (bpf-p val) (x-points val)))))
                                                           )) '<)))))
         (data (loop for timetag in times collect
                     (loop for col2 from 0 to (1- (numcols ch-evt)) collect
                           (loop for row2 from 0 to (1- (numrows ch-evt)) collect
                                 (let ((val2 (get-array-val ch-evt row2 col2)))
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
   ((freq :initform 609.0 :initarg :freq :type number :accessor freq :documentation "FOF frequency(-ies) [Hz]")   ; Frequency
    (amp :initform 1.0 :initarg :amp :type number :accessor amp  :documentation "FOF amplitudes !! Only linear [0.0 -> 1.0]")        ; Amplitude
    (bw :initform 77.0 :initarg :bw :type number :accessor bw :documentation "FOF bandwidth(s) [Hz]")          ; Bandwidth
    (win :initform 0.003 :initarg :win :type number :accessor win :documentation "attack time(s) of the FOFs [sec]")      ; Tex
    (wdur :initform 0.02 :initarg :wdur :type number :accessor wdur :documentation "duration(s) of the FOFs [sec]")    ; DebAtt
    (wout :initform 0.007 :initarg :wout :type number :accessor wout :documentation "decay time(s) of the FOFs [sec]")  ; Atten
    (phs :initform 0 :initarg :phs :type number :accessor phs :documentation "FOF phase")          ; FofPhase
    ;(Channel1 :initform 1 :initarg :Channel1 :type number :accessor Channel1)  
    )
   (:icon 607)
   (:documentation "A Chant FOF event represents the evolution of the FOF generator parameters during the interval specified by <action-time> and <dur>.

GENERAL CHANT-EVENTS PARAMETERS:

<numcols> determines the number of FOFs in the generator.
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

Note that CH-FOF MUST BE USED ALONG WITH AT LEAST ONE CH-F0 OBJECT, which determines the fundamental frequency of the generator.
"))

;;; OM 6.6 - TO BUILD A CH-FOF (or other class-array) in Lisp, use :
;;; (om-make-array 'ch-fof 3 0 0 nil nil :freq (make-instance 'bpf))

(defmethod get-slot-in-out-names ((self ch-fof))
   (values '("self" "numcols" "action-time" "dur" "kt" "user-fun") 
           '(nil 1 0 0 nil nil)
           '("synthesis event" "number of components (FOFs)" "start time [sec]" "duration [sec]" "control period [sec]" "lambda function applied to each component")
           '(nil nil nil nil nil nil)))

(defmethod evt-type-str ((self ch-fof)) "FOB")
(defmethod evt-type-sid ((self ch-fof)) 0)


(defmethod fob-p ((self ch-fof)) t)
(defmethod fob-p ((self t)) nil)


(defmethod get-obj-dur ((self ch-fof)) 
  (let ((dura (max (event-dur self)
                   (loop for onset in (get-array-row self 4)
                         for dur in (get-array-row self 5)
                         maximize  (+ (if (bpf-p onset)
                                          (list-max (mapcar '+ (x-points onset) (y-points onset)))
                                        onset)
                                      (if (bpf-p dur) 
                                          (list-max (mapcar '+ (x-points dur) (y-points dur)))
                                        dur)) 
                         into max-elt-dur
                         finally (return max-elt-dur)))))
    (round (* (+ (action-time self) dura) 1000))))

(defmethod event-dur ((self ch-fof)) (call-next-method))


(defmethod synth-dur ((self t)) (event-dur self))

#|
(defmethod synth-dur ((self ch-fof))
  (+ (call-next-method) 
     (loop for fofdur in (get-array-row self 'wdur)
           maximize  (if (bpf-p fofdur)
                         (list-max (mapcar '+ (x-points fofdur) (y-points fofdur)))
                       fofdur)
           into max-fof-dur
           finally (return max-fof-dur))))


(defmethod synth-dur ((self ch-fof))
  (+ (call-next-method) 
     (loop for fofdur in (get-array-row self 'wdur)
           for fofout in (get-array-row self 'wout)
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

(defmethod synth-dur ((self ch-fof))
  (call-next-method))


(defmethod evt-to-sdif ((self ch-fof))
  (if (continuous-event-p self)
        (multiple-value-bind (times array-lists) 
            (continuous-values self)
          (loop for time in times
                for framevalues in array-lists 
                collect 
                (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) time) :streamid 0 
                               :lmatrix (remove nil
                                                (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                     :data (loop for i from 0 to (1- (numcols self)) append (first-n (nth i framevalues) 7)))
                                                      (continuous-channel-panning-matrix self framevalues)
                                                      )))))
      (remove nil 
              (append
               (if (ch-evt-fade-in self) 
                   (list (make-instance 'sdifframe :signature "1FOB" :ftime (get-absolute-time self) :streamid 0 
                                        :lmatrix (remove nil
                                                         (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                              :data (loop for i from 0 to (1- (numcols self)) append 
                                                                                          (let ((data (first-n (get-array-col self i) 7)))
                                                                                            (cons (car data) (cons 0 (cddr data)))))) 
                                                              
                                                               (static-channel-panning-matrix self)
                                                               )))
                         (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid 0 
                                        :lmatrix (remove nil
                                                         (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                              :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 7)))
                                                               (static-channel-panning-matrix self)
                                                               ))))
                 (list
                  (make-instance 'sdifframe :signature "1FOB" :ftime (get-absolute-time self) :streamid 0 
                                 :lmatrix (remove nil
                                                  (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                       :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 7)))
                                                        (static-channel-panning-matrix self)
                                                        )))))

               (if (plusp (ch-evt-duration self))
                   (if (ch-evt-fade-out self) 
                       (list 
                        (make-instance 'sdifframe :signature "1FOB" :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) :streamid 0 
                                       :lmatrix (remove nil 
                                                        (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                             :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 7)))
                                                              (static-channel-panning-matrix self)
                                                              )))
                        (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 0 
                                       :lmatrix (remove nil
                                                        (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                             :data (loop for i from 0 to (1- (numcols self)) append 
                                                                                         (let ((data (first-n (get-array-col self i) 7)))
                                                                                           (cons (car data) (cons 0 (cddr data))))))
                                                              (static-channel-panning-matrix self)
                                                              ))))

                     (list (make-instance 'sdifframe :signature "1FOB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 0 
                                          :lmatrix (remove nil
                                                           (list (make-instance 'raw-sdifmatrix :signature "1FOF" :num-elts (numcols self) :num-fields 7
                                                                                :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 7)))
                                                                 (static-channel-panning-matrix self)
                                                                 ))))
                     )
                 )))
      ))



;=================================
; CHANT MODULE : REB
;=================================

(defclass! ch-flt (chant-matrix-Evt) 
   ((freq :initform 609.0 :initarg :freq :type number :accessor freq :documentation "central frequency(-ies) [Hz]")   ; Frequency
    (amp :initform 1.0 :initarg :amp :type number :accessor amp  :documentation "amplitude(s) of the filters(s) !! Only linear [0.0 -> 1.0]")  ; Amplitude
    (bw :initform 77.0 :initarg :bw :type number :accessor bw :documentation "bandwidths [Hz]")  ; BandWidth
    (saliance :initform 1 :initarg :saliance :type number :accessor saliance)      ; saliance
    (correction :initform 0 :initarg :correction :type number :accessor correction)  ; correction
    ;(Channel1 :initform 1 :initarg :Channel1 :type number :accessor Channel1)
    ) 
   (:icon 608)
   (:documentation "A Chant FILTER event represents the evolution of a filter bank parameters during the interval specified by <action-time> and <dur>.

GENERAL CHANT-EVENTS PARAMETERS:

<numcols> determines the number of filters.
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

(defmethod evt-type-str ((self ch-flt)) "FILTER")
(defmethod evt-type-sid ((self ch-flt)) 1)

(defmethod reb-p ((self ch-flt)) t)
(defmethod reb-p ((self t)) nil)

(defvar *filt-limit* 1000)

(defmethod get-obj-dur ((self ch-flt)) 
   (round (* 1000
             (+ (max ;(/ (log *filt-limit*) (* pi (apply 'min (get-array-row self 2) ))) 
                     (ch-evt-duration self)
                     (event-dur self))
                (action-time self)))))

(defmethod get-num-of-channel ((self ch-flt)) 2)

(defmethod get-slot-in-out-names ((self ch-flt))
   (values '("self" "numcols" "action-time" "dur" "kt" "user-fun") 
           '(nil 1 0 0 nil nil)
           '("syntehsis event" "number of components (Filters)" 
             "start time [sec]" "duration [sec]" "control period [sec]" "lambda function applied to each component")
           '(nil nil nil nil nil nil)))


(defmethod evt-to-sdif ((self ch-flt))
  (let ((chans (extra-channels self)))
    (if (continuous-event-p self)
        (multiple-value-bind (times array-lists) 
            (continuous-values self)
          (loop for time in times
                for framevalues in array-lists 
                collect 
                (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) time) :streamid 1 
                               :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                             :data (loop for i from 0 to (1- (numcols self)) append (first-n (nth i framevalues) 5)))
                                              (continuous-channel-panning-matrix self framevalues)
                                              )))))

      (remove nil
              (append
               (if (ch-evt-fade-in self) 
                   (list 
                         (make-instance 'sdifframe :signature "1REB" :ftime (get-absolute-time self) :streamid 1 
                              :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                            :data (loop for i from 0 to (1- (numcols self)) append 
                                                                        (let ((data (first-n (get-array-col self i) 5)))
                                                                          (cons (car data) (cons 0 (cddr data))))))
                                             (static-channel-panning-matrix self)
                                             )))

                         (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid 1 
                              :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                            :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 5)))
                                             (static-channel-panning-matrix self)
                                             )))
                         )
                 (list
                  (make-instance 'sdifframe :signature "1REB" :ftime (get-absolute-time self) :streamid 1 
                              :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                            :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 5)))
                                             (static-channel-panning-matrix self)
                                             )))))

               (if (plusp (ch-evt-duration self))
                   (if (ch-evt-fade-out self) 
                       (list 
                        (make-instance 'sdifframe :signature "1REB" :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) :streamid 1 
                              :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                            :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 5)))
                                             (static-channel-panning-matrix self)
                                             )))
                        (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 1 
                              :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                            :data (loop for i from 0 to (1- (numcols self)) append 
                                                                        (let ((data (first-n (get-array-col self i) 5)))
                                                                          (cons (car data) (cons 0 (cddr data))))))
                                             (static-channel-panning-matrix self)
                                             )))
                       )

                   (list (make-instance 'sdifframe :signature "1REB" :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid 1 
                              :lmatrix (remove nil (list (make-instance 'raw-sdifmatrix :signature "1RES" :num-elts (numcols self) :num-fields 5
                                                            :data (loop for i from 0 to (1- (numcols self)) append (first-n (get-array-col self i) 5)))
                                             (static-channel-panning-matrix self)
                                             )))
                         )
                     )
               ))

              )
      )))



;=================================
; CHANT PATCH
;=================================

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


(defun write-CHANT-IDStables (file patch sortedevents durtot)
   (multiple-value-bind (fofs rebs nois snds) 
       (check-events-for-patch sortedevents patch)
     (let (nc dur st en)
       
       (when fofs
         (loop for evt in fofs 
               maximize (numcols evt) into n
               minimize (get-absolute-time evt) into b
               maximize (+ (get-absolute-time evt) (synth-dur evt)) into e
               finally (setf dur (- e b) st b en e nc n))
       (sdif-write-IDS file 0 "Chant" (format nil "Patch~D/1/FOB/~D/~D/~F/~F" patch 1 nc st en)))
                                              
                                              
     (when rebs
       (loop for evt in rebs 
               maximize (numcols evt) into n
               minimize (get-absolute-time evt) into b
               maximize (+ (get-absolute-time evt) (event-dur evt)) into e
               finally (setf dur (- e b) st b en e nc n))
       (sdif-write-IDS file 1 "Chant" (format nil "Patch~D/1/REB/~D/~D/~F/~F" patch 1 nc st en)))
                                              
     (when nois
       (loop for evt in nois 
               minimize (get-absolute-time evt) into b
               maximize (+ (get-absolute-time evt) (event-dur evt)) into e
               finally (setf dur (- e b) st b en e))
       (sdif-write-IDS file 2 "Chant" (format nil "Patch~D/1/NOI/~D/0/~F/~F" patch 1 st en)))
                                    
     (when snds
       (loop for evt in snds 
               minimize (action-time evt) into b
               maximize (+ (action-time evt) (event-dur evt)) into e
               finally (setf dur (- e b) st b en e))
       
       (sdif-write-IDS file 3 "Chant" (format nil "Patch~D/1/SND/~D/1/~F/~F/~S" patch 1 st en
                                              (get-str-for-sound (car snds))
                                              )))
     )))


;=================================
; CHANNELS INSIDE CH-EVENTS
;=================================


(defmethod extra-channels ((self chant-matrix-Evt))
   (let (rep)
     (loop for item in (Lcontrols self) do
           (let ((ctlname (string (car item))))
           (when (and (> (length ctlname) 4)
                      (string-equal "chan" (subseq ctlname 0 4)))
             (push (label2index self ctlname) rep))))
     (reverse rep)))

(defmethod get-channels-num ((self chant-matrix-Evt)) 
   (max 1 (length (extra-channels self))))


(defvar *channel-events* nil)

(defmethod continuous-channel-panning-matrix ((self chant-matrix-Evt) framevalues)
  (let ((chans (extra-channels self)))
    (if chans 
        (make-instance 'raw-sdifmatrix :signature "1CHA" :num-elts (numcols self) :num-fields (length chans)
                       :data (loop for i from 0 to (1- (numcols self)) append 
                                   (loop for ch in chans collect (or (nth ch (nth i framevalues)) 1))))
      (unless *channel-events*
        (make-instance 'raw-sdifmatrix :signature "1CHA" :num-elts (numcols self) :num-fields 1
                       :data (make-list (numcols self) :initial-element 1))
        ))))

(defmethod static-channel-panning-matrix ((self chant-matrix-Evt))
  (let ((chans (extra-channels self)))
    (if chans 
        (make-instance 'raw-sdifmatrix :signature "1CHA" :num-elts (numcols self) :num-fields (length chans)
                       :data (loop for i from 0 to (1- (numcols self)) append 
                                   (loop for ch in chans collect (or (nth ch (get-array-col self i)) 1))))
      (unless *channel-events*
        (make-instance 'raw-sdifmatrix :signature "1CHA" :num-elts (numcols self) :num-fields 1
                       :data (make-list (numcols self) :initial-element 1))
        ))))


;=================================
; CHANT SPECIAL MODULE : CHANNELS
;=================================

(defclass! ch-channels (chant-matrix-Evt) ()
   (:icon 610)
   (:documentation "CH-CHANNEL is a special Chant event allowing to control the distribution of the chant modules' output on a given time interval, and independently of these modules.

Add :chanX (e.g. :channel1, :channel2,...) keyword controls for the different channels and determine the distribution of the different components on these channels (using constant alues of BPFs)."))

(defmethod get-slot-in-out-names ((self ch-channels))
   (values '("self" "numcols" "action-time" "dur" "kt" "user-fun") 
           '(nil 1 0 0 nil nil)
           '("synthesis event" "number of components" "start time [sec]" "duration [sec]" "control period [sec]" "lambda function applied to each component")
           '(nil nil nil nil nil nil)))


(defmethod evt-to-sdif ((self ch-channels))
  (let ((chans (extra-channels self))
        (frametypes (remove '(2 3) (patch-ev-types *chant-patch*) :test #'(lambda (test item) (find item test)))))  ;;; we don't want noise or snd frames for channels
    (if (continuous-event-p self)
        (multiple-value-bind (times array-lists) 
            (continuous-values self)
          (loop for time in times
                for framevalues in array-lists 
                append
                (loop for sid in frametypes collect
                      (make-instance 'sdifframe :signature (type-to-frame-signature sid) :ftime (+ (action-time self) (/ (offset self) 1000.0) time) :streamid sid 
                                     :lmatrix (list (continuous-channel-panning-matrix self framevalues))))
                ))

      (remove nil
              (loop for sid in frametypes append
                    (append
                     (if (ch-evt-fade-in self) 
                         (list 
                          (make-instance 'sdifframe :signature (type-to-frame-signature sid) :ftime (get-absolute-time self) :streamid sid 
                                         :lmatrix (list (static-channel-panning-matrix self)))
                          (make-instance 'sdifframe :signature (type-to-frame-signature sid) :ftime (+ (get-absolute-time self) (ch-evt-fade-in self)) :streamid sid 
                                         :lmatrix (list (static-channel-panning-matrix self)))
                          )
                       (list
                        (make-instance 'sdifframe :signature (type-to-frame-signature sid) :ftime (get-absolute-time self) :streamid sid 
                                       :lmatrix (list (static-channel-panning-matrix self)
                                                      ))))
                     
                     (if (plusp (ch-evt-duration self))
                         (if (ch-evt-fade-out self) 
                             (list 
                              (make-instance 'sdifframe :signature (type-to-frame-signature sid) 
                                             :ftime (- (+ (get-absolute-time self) (ch-evt-duration self)) (ch-evt-fade-out self)) :streamid sid 
                                             :lmatrix (list (static-channel-panning-matrix self)))
                              (make-instance 'sdifframe :signature (type-to-frame-signature sid) :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid sid 
                                             :lmatrix (list (static-channel-panning-matrix self)))
                              )

                           (list (make-instance 'sdifframe :signature (type-to-frame-signature sid) :ftime (+ (get-absolute-time self) (ch-evt-duration self)) :streamid sid 
                                                :lmatrix (list (static-channel-panning-matrix self)))
                                 )
                           )
                       ))))
      )))



(defmethod! gen-chant-channels (list &optional nc)
  :icon 610
  :initvals '((((0 (1 1))) ((0 (1 1)))) nil)
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

  (let ((channels (cons-array 
                   (make-instance 'ch-channels)
                   (list nil (or nc (length list)) 0 (car (last (car list))) nil nil)
                   (loop for i from 1 to (length (cadr (caar list))) append
                         (list (intern (format nil "chan~D" i) :keyword)
                               (loop for formant in list collect
                                     (om-make-bpf 'bpf (mapcar 'car formant) 
                                                  (mapcar #'(lambda (state) (nth (1- i) (cadr state))) formant)  
                                                  3)))))))
    (set-data channels)
    channels))
                  

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
  (let* ((inter-evt (make-instance (type-of ev1)))
         (start (+ (get-absolute-time ev1) (event-dur ev1) delta))
         (dur (- (get-absolute-time ev2) start delta))
         (args (loop for slot in (mapcar 'internk (mapcar 'name (get-all-initargs-of-class 'ch-fof))) append
                     ;;; ex. for each slot in '(freq amp bw ...)
                     (let ((slotrules (loop for rule in rules 
                                            when (if (listp (car rule))
                                                    (equal (internk (car (car rule))) slot)
                                                   (equal (internk (car rule)) slot))
                                            collect rule))
                           ;;; slot rules = (freq 400) or (freq 'my-freq-fun) or ((freq 1) ...)
                           (slotvals (loop for v1 in (get-array-row ev1 slot) 
                                           for v2 in (get-array-row ev2 slot) collect
                                           (list (if (bpf-p v1) (last-elem (y-points v1)) v1)
                                                 (if (bpf-p v2) (car (y-points v2)) v2)
                                                 nil ;;; HERE WILL BE STORED THE RESULT OF THE RULE
                                                 ))))
                       (when slotrules
                         (print (format nil "RULE(S) for ~A:" (string slot)))
                         (loop for r in slotrules do (print r)))

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
                       (list slot
                             (if (listp slotvals) 
                                 (loop for item in slotvals collect 
                                       (if (third item)
                                           (cond ((bpf-p (third item))
                                                  (bpf-scale (third item) :x1 0 :x2 dur))
                                                 (t (third item)))
                                                                  
                                         (om-make-bpf 'bpf (list 0 dur) (list (car item) (cadr item)) 5))
                                       )
                               slotvals))))))
    (setf inter-evt (cons-array inter-evt (list nil (numcols ev1) start dur nil nil) args))
    (set-data inter-evt)
    inter-evt))
                      

                             

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
                                                                  (string-equal (signature (car (lmatrix f1))) (signature (car (lmatrix f2))))
                                                                  )))))
                   ;(when (= 1.6 (ftime fr)) (print (signature (car (lmatrix fr)))))
                   (when duplicate 
                     (om-beep-msg (format nil "WARNING: duplicate time for ~A at ~D" (signature (car (lmatrix fr))) (ftime fr)))
                     ;(print (list (ftime fr) (signature (car (lmatrix fr))) (signature (car (lmatrix duplicate)))))
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

(defmethod apply-user-fun ((self chant-evt))
  (if (parsing-fun self)
      (let ((rep (clone self)))
        (loop for f in (list! (parsing-fun self)) do
              (loop for i from 0 to (1- (numcols self)) do
                    (funcall f rep i)))
        rep)
    self))
                   
(defmethod collect-chant-events ((self t) num) nil)
(defmethod collect-chant-events ((self list) num) 
   (remove-if 'null (loop for object in self collect (collect-chant-events object num))))
(defmethod collect-chant-events ((self container) num) 
   (remove-if 'null (loop for object in (inside self) collect (collect-chant-events object num))))

(defmethod collect-chant-events ((self chant-Evt) num) self)
;(if (in-chant-patch-p self num) self (om-beep-msg (string+ "Warning: events of type " (evt-type-str self) " are not allowed in CHANT patch " (integer-to-string num))))
               

;; A utiliser avec les objets identifiés, juste avant l'envoi à synthesize
;; renvoie les fofs et les f0s
; Trie les elements entrants, et élimine les doublons

;(let ((list '("a" "b" "c" "d" "ca" "bc")))
;  (remove-if #'(lambda (x) 
;                 (find x list :test #'(lambda (x y) (and (search x y) (not (string-equal x y)))))
;                 ) list))

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



(defmethod! ch-synthesize ((events t) &key 
                           (patch nil) 
                           (name "my_synth") (resolution nil)
                           (rescale  0.0) (sr nil) (run t) duration
                           &allow-other-keys)
  
  (setf events (filter-ch-ids (collect-chant-events (remove nil (list! events)) patch)))

  (let* ((path-sdif (handle-new-file-exists (tmpfile (string+ (if (pathnamep name) (pathname-name name) name) ".sdif"))))
         (path-aiff (if (pathnamep name) name (corrige-sound-filename (string+ name "." (string-downcase *def-snd-format*)) *om-outfiles-folder*))))
    (unless events (om-beep-msg "No synthesis events !"))      
              
    (when events
                
      (let* ((thefile (sdif::sdif-open-file (om-namestring path-sdif) :eWriteFile))
             (ch-num (loop for item in events maximize (get-channels-num item)))
             (samplerate (or sr *audio-sr* 44100))
             (resol (or resolution *audio-res* 16))
             (sortedevents (split-list events))
                       
             (*chant-patch* (or patch (deduce-patch-from-evts sortedevents) 0))
             (*channel-events* (find 'ch-channels events :key 'type-of))
             (total-dur  (or duration
                             (loop for item in events maximize
                                   (+ (get-absolute-time item) (synth-dur item))
                                             ;(/ (get-obj-dur item) 1000.0)
                                   )))
             (nvt  (mat-trans (list (list "BufferSize" "512")
                                    (list "NumberOfChannels" (integer-to-string ch-num))
                                    (list "EndTime" (format nil "~D" total-dur))
                                    (list "Author" (string+ "OM " *version-string*))
                                    (list "SamplingRate" (format nil "~F" samplerate))
                                    )))
                       ;(sortedevents (split-list events))
                       ;(events-and-silence (handle-inter-events sorted-events))
             (sdifframes (pack-frames (loop for item in events append (evt-to-sdif (apply-user-fun item))))))
                
        (om-print "==========================" "OM-Chant ::")
        (om-print (format nil "CHANT synthesis patch: ~D" *chant-patch*) "OM-Chant ::")
                       
        (sdif::SdifFWriteGeneralHeader thefile)
        (when (> ch-num 2)
          (let ((typedef-str "{ 1MTD 1CHA {"))
            (loop for c = 3 then (+ c 1) while (<= c ch-num) do
                  (setf typedef-str (string+ typedef-str " Channel" (number-to-string c) ",")))
            (setf typedef-str (string+ typedef-str " } }"))
            (write-sdif-types thefile typedef-str)))
        (write-1nvt-table thefile (car nvt) (cadr nvt))
        (write-CHANT-IDStables thefile *chant-patch* sortedevents total-dur)
        (sdif::SdifFWriteAllASCIIChunks thefile)
        (save-sdif sdifframes thefile)
        (sdif::sdif-close-file thefile)
                  
        (if run
            (progn
              (when *delete-inter-file* (add-tmp-file path-sdif))
              (setf path-aiff (chant-synth path-sdif :outfile path-aiff :resolution resol :normalize-level rescale))
              (when *delete-inter-file* (clean-tmp-files))
              path-aiff
              )
          (probe-file path-sdif)
          )
        ))))

;;;==========================
;;; OM-SYNTHESIZE INTERFACE
;;;==========================

(defmethod! synthesize ((elements chant-evt) &key (name "my_synt") sr (rescale nil) (run t) (evt-test nil) resolution 
                        kr tables (nchnls nil) inits ;For Csound
                        patch duration ;For Chant
                        )        
            (ch-synthesize elements :name name :sr sr :patch patch
                           :duration duration :rescale rescale :run run :resolution resolution))

(defmethod synthesize-method ((self chant-evt)) 'ch-synthesize)

(pushr 'chant-evt *synthesis-element-types*)





