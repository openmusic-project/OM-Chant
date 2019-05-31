(in-package :om)

(defun load-def-maq-patch (name)
  (let ((newpatch (omNG-make-new-patch name)))
    (setf (mypathname newpatch) (om-make-pathname :directory (append (pathname-directory (lib-resources-folder (find-library "OM-Chant")))
                                                                     '("patches"))
                                                  :name name :type "omp"))
    (setf (loaded? newpatch) nil)    
    (load-patch newpatch)
    (setf (mypathname newpatch) nil)
    newpatch
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methodes utilitaires pour la creation de boites temporelles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; crée une temporalbox de manière aisée
(defun make-temporal-box (reference &key name offset posy extend sizey strech-fact value free-store)
  (let ((rep (omNG-make-tempobj reference (om-make-point (or offset 0) (or posy 10))
                                (or name ""))))
    (when sizey (setf (slot-value rep 'sizey) sizey))
    (when extend (setf (slot-value rep 'extend) extend))
    (when strech-fact (setf (slot-value rep 'strech-fact) strech-fact))
    (when value (setf (slot-value rep 'value) value))
    (when free-store (setf (slot-value rep 'free-store) free-store))
    rep))


;; donne les dimensions d'une boite de morphing
(defmethod! get-morph-bounds ((elt1 temporalbox) (elt2 temporalbox))
            :numouts 3
            (let* ((foflist (sort (list elt1 elt2) '< :key 'offset))
                   (fof1 (first foflist))
                   (fof2 (second foflist))
                   (beg (offset fof1))
                   (foflist (sort (list elt1 elt2) '< :key 'posy)) ;;tri vertical
                   (fof1 (first foflist))
                   (fof2 (second foflist))
                   (sizey 10)
                   (posy (- (- (posy fof1) (sizey fof1)) 5))
                   (sizey 5))

              (values (round beg) (round posy) sizey)
              )

            )

;; donne les dimensions d'une boite de transition
(defmethod! get-cons-bounds ((elt1 temporalbox) (elt2 temporalbox))
            :numouts 4
            (let* ((foflist (sort (list elt1 elt2) '< :key 'offset))
                   (fof1 (first foflist))
                   (fof2 (second foflist))
                   (beg (+ (offset fof1) (* (dur (first (value fof1))) 1000)))
                   (dur (- (offset fof2) beg))
                   (foflist (sort (list elt1 elt2) '< :key 'posy)) ;;tri vertical
                   (fof1 (first foflist))
                   (fof2 (second foflist))
                   (posy (- (posy fof2) (/ (sizey fof2) 2)))
                   (sizey (- posy (posy fof1))))

              (values (round beg) (round dur) (round posy) sizey)
              )
            )


;;;;;;;;;;;;;;;;
;; A ajouter à om-color ???
;;melange deux couleurs
(defmethod! om-color-blend ((elt1 temporalbox) (elt2 temporalbox))
            (let* ((color1 (colorframe elt1))
                   (color2 (colorframe elt2)))
              (om-make-color 
               (/ (+ (om-color-r color1) (om-color-r color2)) 2)
               (/ (+ (om-color-g color1) (om-color-g color2)) 2)
               (/ (+ (om-color-b color1) (om-color-b color2)) 2)
               )
              )
            )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CREATION D'OBJETS FOF-ID et F0-ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OBJETS FOF

;; crée l'objet fof-id correspondant aux parametres formantiques passés en entrée
(defmethod! maq-fof ((tb temporalbox) (fmts-data list) &key (color (list 0.5 0.6 0.3)))
            :indoc '("associated temporal box" "list of formantic parameters" "optionnal : color of the box")
            :icon 646
            :outdoc '("a CH-FOF event")
            :doc "Generates a FOF event, which has the same duration than the temporalbox given as an input.

The list of formantic parameters must follow the syntax : ( (frequency amplitude bandwidth) (f a bw) ... )"
            (let* ((dur (ms->sec (* (strech-fact tb) (extend tb))))
                   (fmts (mat-trans fmts-data))
                   (array (cons-array (make-instance 'ch-fof)  
                                  (list nil (length (first fmts)) 0 dur nil nil(string (gensym)))
                                  (list :freq (first fmts)
                                        :amp (second fmts)
                                        :bw (third fmts)
                                  ))
                   ))

              (setf (colorframe tb) (om-make-color (first color) (second color) (third color)))
              (set-data array)
              array
              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methodes de creation de morphings/transitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! maq-fof-transition ((self t) (fof1 t) (fof2 t) (traj-profile t) &optional sr)
     (om-beep-msg "Transition module connections are wrong"))

;; crée l'objet de transition entre fof-id donnés en entrée
(defmethod! maq-fof-transition ((self temporalbox) (fof1 temporalbox) (fof2 temporalbox) (traj-profile list) &optional sr)
            :indoc '("associated temporal box" "temporalbox which embeds the fist FOF event" "temporalbox which embeds the second FOF event"
                                               "trajectory profile")
            :icon 642
            :outdoc '("a CH-FOF event")
            :doc "Generates a FOF event, which represents the transition between the two distant FOF events given as an input. The trajectory follows the profile, which defines the trajectory of each single formant.

The trajectory profile must follow the syntax : ( ([frequency trajectory of 1st formant] [frequency trajectory of 2nd formant] ...)
                                                  ([amplitude trajectory of 1st formant] [amplitude trajectory of 2nd formant] ...)
                                                  ([bandwidth trajectory of 1st formant] [bandwidth trajectory of 2nd formant] ...)
                                                  trajectory of the interpolation between the two FOF events ).

Each single trajectory is represented by a BPF object, which y-values represent multiplicative values applied to the formant parameters.
The BPF will be rescaled, thus there are no constraints concerning the abscissa."

           (let* ((ctrl (if sr (if (integerp sr) sr (round (/ dur sr))) 50));; nombre de points de sampling
                  (bounds (multiple-value-list (get-cons-bounds fof1 fof2)))
                  (bd1 (first bounds)) ;; dimensions spatiales
                  (bd2 (second bounds))
                  (bd3 (third bounds))
                  (bd4 (fourth bounds))
                  (dur (ms->sec bd2))  
                  (onset (ms->sec bd1))
                  (val1 (first (value fof1))) ;; resultats des temporalbox en entrée
                  (val2 (first (value fof2)))
                  (id (concatenate 'string (id val1) (id val2)))
                  (color (om-color-blend fof1 fof2))
                  (onset1 (offset fof1))
                  (onset2 (offset fof2))
                  (fof1 (if (< onset1 onset2) val1 val2)) 
                  (fof2 (if (< onset1 onset2) val2 val1))
                  )

             (setf (offset self) bd1)
             (setf (extend self) bd2)
             (setf (posy self) bd3)
             (setf (sizey self) (max bd4 10))
             (setf (colorframe self) color)

             (if (<= (max onset1 onset2) (+ (min onset1 onset2) (sec->ms (dur fof1)))) 
                 (om-beep-msg "The two input fofs must'nt be overlapping !")
               (apply-rule-id (list fof1 fof2) dur (first traj-profile) (second traj-profile) (third traj-profile) (fourth traj-profile) ctrl onset)
           )))

(defmethod! maq-fof-transition ((self temporalbox) (fof1 temporalbox) (fof2 temporalbox) (traj-profile phoneme) &optional sr)
         
           (let* ((ctrl (if sr (if (integerp sr) sr (round (/ dur sr))) 50));; nombre de points de sampling
                  (bounds (multiple-value-list (get-cons-bounds fof1 fof2)))
                  (bd1 (first bounds)) ;; dimensions spatiales
                  (bd2 (second bounds))
                  (bd3 (third bounds))
                  (bd4 (fourth bounds))
                  (dur (ms->sec bd2))  
                  (onset (ms->sec bd1))
                  (val1 (first (value fof1))) ;; resultats des temporalbox en entrée
                  (val2 (first (value fof2)))
                  (id (concatenate 'string (id val1) (id val2)))
                  (color (om-color-blend fof1 fof2))
                  (onset1 (offset fof1))
                  (onset2 (offset fof2))
                  (fof1 (if (< onset1 onset2) val1 val2)) 
                  (fof2 (if (< onset1 onset2) val2 val1))
                  )

             (setf (offset self) bd1)
             (setf (extend self) bd2)
             (setf (posy self) bd3)
             (setf (sizey self) (max bd4 10))
             (setf (colorframe self) color)

             (if (<= (max onset1 onset2) (+ (min onset1 onset2) (sec->ms (dur fof1)))) 
                 (om-beep-msg "The two input fofs must'nt be overlapping !")
               (apply-rule-id (list fof1 fof2) dur 
                              (or (get-array-row traj-profile :freq) (make-list (numcols traj-profile) :initial-element (om-make-bpf 'bpf '(0 1) '(1 1) 6)))
                              (or (get-array-row traj-profile :amp) (make-list (numcols traj-profile) :initial-element (om-make-bpf 'bpf '(0 1) '(1 1) 6)))
                              (or (get-array-row traj-profile :bw) (make-list (numcols traj-profile) :initial-element (om-make-bpf 'bpf '(0 1) '(1 1) 6)))
                              (or (transition traj-profile) (om-make-bpf 'bpf '(0 1) '(0 1) 6))
                              ctrl onset)
           )))



(defmethod! maq-fof-transition ((self temporalbox) (fof1 temporalbox) (fof2 temporalbox) (traj-profile phoneme) &optional sr)
 (maq-fof-transition self fof1 fof2 
                     (list (get-array-row traj-profile :freq)
                           (get-array-row traj-profile :amp)
                           (get-array-row traj-profile :bw)
                           (transition traj-profile))
                     sr))



(defmethod! maq-fof-morph ((self t) (fof1 t) (fof2 t) (profile t) &optional sr)
     (om-beep-msg "Morphing module connections are wrong"))

;; crée l'objet de transition entre fof-id donnés en entrée
(defmethod! maq-fof-morph ((self temporalbox) (fof1 temporalbox)(fof2 temporalbox)(profile bpf) &optional sr)
            :indoc '("associated temporal box" "temporalbox which embeds the fist FOF event" "temporalbox which embeds the second FOF event"
                                               "morphing profile")
            :icon 643
            :outdoc '("a CH-FOF event")
            
            :doc "Generates a FOF event, which represents the morphing morphing between the two overlapping FOF events given as an input. The morphing follows the profile defined by the user.

The morphing profile is represented by a BPF object, which y-values define the 'morphing coefficient'.
The BPFs will be rescaled, thus there are no constraints concerning the values, which will be normalized."

            (let* ((ctrl (if sr (if (integerp sr) sr (round (/ dur sr))) 50));; nombre de points de sampling
                   (profile (bpf-scale profile :y1 0 :y2 1))
                   (bounds (multiple-value-list (get-morph-bounds fof1 fof2)))
                   (bd1 (first bounds)) ;; dimensions spatiales
                   (bd2 (second bounds))
                   (bd3 (third bounds))
                   (onset (ms->sec bd1))
                   (val1 (first (value fof1))) ;; resultats des temporalbox en entrée
                   (val2 (first (value fof2)))
                   (id (concatenate 'string (id val1) (id val2)))
                   (color (om-color-blend fof1 fof2))
                   (onset1 (offset fof1))
                   (onset2 (offset fof2))
                   (the-fof1 (if (< onset1 onset2) val1 val2)) 
                   (the-fof2 (if (< onset1 onset2) val2 val1))
                   )
             
              (setf (offset self) bd1)
              (setf (posy self) bd2)
              (setf (sizey self) bd3)
              (setf (colorframe self) color)

              (if (>= (max onset1 onset2) (+ (min onset1 onset2) (sec->ms (dur the-fof1)))) 
                  (om-beep-msg "The two input fofs are not overlapping !") 
                (fof-morph-maquette (list the-fof1 the-fof2) (list onset1 onset2) profile ctrl)
                )))


;; morphing specifique à l'environnement des maquettes
;; renvoie une fof-id identifiée, qui remplace les deux fof-id données en entrée
(defmethod fof-morph-maquette ((fofs list) (onsets list) (morph-profile bpf) (ctrl real))            
            (let* ((ctrl (if (= 1 (rem ctrl 2)) (+ ctrl 1) ctrl)) ;; contrainte : nombre pair ! (division par deux ci-dessous)
                   (morph-prof (third (multiple-value-list (om-sample morph-profile ctrl))))
                   (beg1 (ms->sec (first onsets)))
                   (beg2 (ms->sec (second onsets)))
                   (fof1 (first fofs))
                   (end1 (+ beg1 (dur fof1)))
                   (slots1 (list (freq fof1)(amp fof1)(bw fof1)))
                   (xslots (list nil nil nil))
                   (yslots (list nil nil nil))
                   (fof2 (second fofs))
                   (end2 (+ beg2 (dur fof2)))
                   (slots2 (list (freq fof2)(amp fof2)(bw fof2)))
                   (nfmts (max (length (first slots1)) (length (first slots2))))
                   (tmp nil)
                   (epsilon 0.00002) ;;evite que deux points soient superposés dans les bpf
                   (res nil)
                   )

                   (if (< beg2 end1)
                       (progn
                        ;;take 1st part
                        (setf tmp (append-vals slots1 0.0 (float (- (- beg2 beg1) epsilon)) (create-list 3 (create-list nfmts nil)) (create-list 3 (create-list nfmts nil))))
                        (setf xslots (first tmp))
                        (setf yslots (second tmp))
                        ;;morph                            
                        (if (< end1 end2)
                            (setf tmp (morph-vals slots1 slots2 (float (- beg2 beg1)) (float (- end1 beg1)) xslots yslots morph-prof ctrl))
                          (progn (setf morph (x-append (first-n morph-prof (/ ctrl 2)) (last-n (om- 1 morph-prof) (/ ctrl 2))))
                          (setf tmp (morph-vals slots1 slots2 (float (- beg2 beg1)) (float (- end2 beg1)) xslots yslots morph ctrl))) ;; change la 2nde partie de la courbe de morphing
                          )
                        ;;(setf tmp (morph-vals slots1 slots2 (float (- beg2 beg1)) (float (- end2 beg1)) xslots yslots morph ctrl))
                        (setf xslots (first tmp))
                        (setf yslots (second tmp))
                        ;;take 2nd part
                        
                        (if (< end1 end2)  
                            (setf tmp (append-vals slots2 (float (+ epsilon (- end1 beg1))) (float (- end2 beg1)) xslots yslots));; cas1
                          (setf tmp (append-vals slots1 (float (+ epsilon (- end2 beg1))) (float (- end1 beg1)) xslots yslots));; cas2
                          )
                        
                        (setf xslots (first tmp))
                        (setf yslots (second tmp))

                        (setf res (om-make-array 'ch-fof
                                               (length (first xslots)) 
                                               0 (- end2 beg1) nil nil
                                               :freq (loop for x in (first xslots) for y in (first yslots) collect  (simple-bpf-from-list x y 'bpf 5))
                                               :amp (loop for x in (second xslots) for y in (second yslots) collect  (simple-bpf-from-list x y 'bpf 5))
                                               :bw (loop for x in (third xslots) for y in (third yslots) collect  (simple-bpf-from-list x y 'bpf 5))
                                               ))
                        (setf (id res) (concatenate 'string (id fof1) "-" (id fof2)))
                        res
                        )

                       nil
                     )))


;; applique des trajectoires aux formants, renvoie une fof-id (identifiée)
(defmethod apply-rule-id ((fofs list)(dur real)(fmt-curves list)(amp-curves list)(bw-curves list)(morph-curve bpf)(ctrl integer)(action-time real))
    (if (om/= dur 0)
        (let* ((x-list (third (multiple-value-list (om-sample (list 0 dur) ctrl))))
               (fof1 (first fofs))
               (fof2 (second fofs))
               (array (cons-array (make-instance 'ch-fof)  
                                  (list nil 
                                        (max (slot-value fof1 'numcols) (slot-value fof2 'numcols)) 
                                        0
                                        dur 
                                        nil 
                                        nil
                                        (string (gensym)))
                                  (list :freq (make-bpfs x-list fmt-curves (inter-trans (slot-value fof1 'freq) (slot-value fof2 'freq) morph-curve ctrl) ctrl)
                                        :amp (make-bpfs x-list amp-curves (inter-trans (slot-value fof1 'amp) (slot-value fof2 'amp) morph-curve ctrl) ctrl)
                                        :bw (make-bpfs x-list bw-curves (inter-trans (slot-value fof1 'bw) (slot-value fof2 'bw) morph-curve ctrl) ctrl))
                                  )))
          (set-data array)
          array)  
      ))



;;;======================================================================
;; OBJETS F0

;; crée l'objet f0-id correspondant à la f0 + vibrato, et la temporalbox associée
(defmethod! maq-f0 ((tb temporalbox))
            :indoc '("associated temporal box")
            :icon 640
            :doc "Generates a F0 event, which has the same duration than the temporalbox given as an input.

It's frequency is defined by the y-position of the temporalbox."
            :icon 640
            :outdoc '("a CH-F0 event")
            
            (let* ((dur (ms->sec (* (extend tb) (strech-fact tb))))
                   (freq (posy tb)))
              (make-instance 'ch-f0 :dur dur :f0 freq)
              ))


;; crée l'objet f0-id correspondant à la f0 + vibrato, et la temporalbox associée
(defmethod! maq-f0-vib ((tb temporalbox) vibfreq vibamp)
            :indoc '("associated temporal box" "vibrato frequency" "vibrato amplitude/enveloppe")
            :initvals '(nil 6 0.2)
            :icon 640
            :outdoc '("a CH-F0 event")
           :doc "Generates a F0 event, with vibrato , which has the same duration than the temporalbox given as an input.

It's central frequency is defined by the y-position of the temporalbox.
The frequency given as an input can be a constant of a BPF.
If 'vibamp' is a contant, the amplitude of the vibrato is given by it's value.
If 'vibamp' is a bpf, the amplitude of the vibrato is given by the size of the temporalbox, divided by 100, and mutliplied by 'vibamp' (which acts as an enveloppe)"
            (let* ((dur (ms->sec (* (extend tb) (strech-fact tb))))
                   (kt 0.01)
                   (freq (param-process (posy tb) 
                                        (vibrato vibfreq (if (typep vibamp 'bpf) (bpf-scale vibamp :y1 0 :y2 (/ (sizey tb) 100)) vibamp))
                                        dur kt 'a))
                   (id (string (gensym))))

                   (make-instance 'ch-f0 :dur dur :f0 freq)
             ))


;; crée l'objet f0-id correspondant à la f0 + jittre, et la temporalbox associée
(defmethod! maq-f0-jit ((tb temporalbox) jitfreq jitamp)
            :indoc '("associated temporal box" "jitter frequency" "jitter amplitude/enveloppe")
            :icon 640
            :outdoc '("a CH-F0 event")
            :doc "Generates a F0 event, with jitter, which has the same duration than the temporalbox given as an input.

It's central frequency is defined by the y-position of the temporalbox.
The frequency given as an input can be a constant of a BPF.
If 'jitamp' is a contant, the amplitude of the jitter is given by it's value.
If 'jitamp' is a bpf, the amplitude of the jitter is given by the size of the temporalbox, divided by 100, and mutliplied by 'jitamp' (which acts as an enveloppe"
            (let* ((dur (ms->sec (* (extend tb) (strech-fact tb))))
                   (kt 0.01)
                   (freq (param-process (posy tb) 
                                        (jitter jitfreq (if (typep jitamp 'bpf) (bpf-scale jitamp :y1 0 :y2 (/ (sizey tb) 100)) (float jitamp)))
                                        dur kt 'a)))
              (make-instance 'ch-f0 :dur dur :f0 freq)
              ))

;; crée l'objet f0-id correspondant à la f0-bpf, et la temporalbox associée
(defmethod! maq-f0-bpf ((tb temporalbox) (f0 bpf))
            :indoc '("associated temporal box" "f0 curve")
            :icon 640
            :outdoc '("a CH-F0 event")
            :doc "Generates a F0 event, defined by a curve, which has the same duration than the temporalbox given as an input.

The frequency is defined by the 'f0' input, a BPF, which is scaled to fit the duration of the event."
            (let* ((dur (ms->sec (* (extend tb) (strech-fact tb)))))

              (setf (posy tb) (round (first (y-points f0))))

              (make-instance 'ch-f0 :dur dur :f0 f0)
              ))


;; crée l'objet f0-id correspondant à la trajectoire, et al temporalbox associée
(defmethod! maq-f0-transition ((self temporalbox) (f01 temporalbox) (f02 temporalbox) (profile bpf) &optional sr)
            :indoc '("associated temporal box" "temporalbox which embeds the fist f0 event" "temporalbox which embeds the second f0 event"
                                               "trajectory profile")
             :icon 644
            :outdoc '("a CH-F0 event")
           :doc "Generates a F0 event, which represents the transition between the two distant f0 events given as an input. The trajectory follows the trajectory profile.

The trajectory profile is represented by a BPF object, which y-values represent multiplicative values applied to, respectively, the last and the first values of the f0 input events, which are interpolated linearly.
The BPF will be rescaled, thus there are no constraints concerning the abscissa."

           (let* ((ctrl (if sr (if (integerp sr) sr (round (/ dur sr))) 50));; nombre de points de sampling
                  (bounds (multiple-value-list (get-cons-bounds f01 f02)))
                  (bd1 (print (first bounds))) ;; dimensions spatiales
                  (bd2 (second bounds))
                  (bd3 (third bounds))
                  (bd4 (fourth bounds))
                  (dur (/ bd2 1000))  
                  (val1 (first (value f01))) ;; resultats des temporalbox en entrée
                  (val2 (first (value f02)))
                  (onset1 (offset f01))
                  (onset2 (offset f02))
                  (f1 (if (< onset1 onset2) (f0 val1) (f0 val2))) ;; fréquences ; verifier si on doit CAR
                  (f2 (if (< onset1 onset2) (f0 val2) (f0 val1)))
                  (dur1 (if (< onset1 onset2) (dur val1) (dur val2)))
                  (f1 (if (typep f1 'bpf) (car (last (y-points f1))) f1)) ;; si f1 ou f2 sont des bpf, on prend resp. le dernier et premier  point
                  (f2 (if (typep f2 'bpf) (first (y-points f2)) f2)) ;; si f1 ou f2 sont des bpf, on prend resp. le dernier et premier  point
                  (sampled-freq (om-sample (bpf-scale (simple-bpf-from-list (list 0 1) (list 0 1) 'bpf 3) :y1 f1 :y2 f2) ctrl)) ;; valeurs samplées, de l'interpolation linéaire entre f1 et f2
                  (freq (simple-bpf-from-list (x-points sampled-freq)
                                              (om* 
                                                 (y-points sampled-freq) 
                                                 (third (multiple-value-list (om-sample profile ctrl)))) 'bpf 5))

                  (id (string (gensym)))
                  (color (om-color-blend f01 f02)))

             (setf (offset self) bd1)
             (setf (extend self) bd2)
             (setf (posy self) bd3)
             (setf (sizey self) (max bd4 10))
             (setf (colorframe self) color)
             
             (if (<= (max onset1 onset2) (+ (min onset1 onset2) (sec->ms dur1))) 
                 (om-beep-msg "The two input f0s must'nt be overlapping !")
               (let ((evt (make-instance 'ch-f0 :dur dur :f0 freq)))
                 (setf (id evt) id)
                 evt))
             ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methodes liées à la base de données des symboles/etats/transitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base de données symboles/etats/transitions

;; BD des couples symboles-infos (formants, trajets), classés
(defclass! phoneme_DB ()
           ((stationarys :accessor stationarys :initform "" :initarg :stationarys)
            (transitionz :accessor transitionz :initform "" :initarg :transitionz))
           (:icon 600)
           (:documentation "Database for phonemes and transitions")
           )


(defmethod get-slot-in-out-names ((self phoneme_DB))
   (values '("self" "stationarys" "transitions") 
           '(nil "" "")
           '("DB ?" "stationarys" "transitions")
           '(nil nil nil)))


;;ce symbole est-il une voyelle ?
(defmethod! isStationary ((in string) (bd phoneme_DB))
            (let* ((found nil))
            (loop for entry in (stationarys bd) do
                  (if (string= (first entry) in) (return t))
                  )
            ))


;;renvoie les informations liées à un symbole
(defmethod! getStationary ((in string)(bd phoneme_DB))
            (loop for entry in (stationarys bd) do
                  (if (string= (first entry) in) (return (second entry)))
                  ))

;;renvoie les informations liées à un symbole
(defmethod! getTransition ((in string)(bd phoneme_DB))
            (loop for entry in (transitionz bd) do
                  (if (string= (first entry) in) (return (second entry)))
                  ))



;; convertit une string en symboles simples (1 caractère)
(defmethod! sentence-to-simple-phonemes ((in string))
            (loop for c across in collect
                  (string c))
            )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methodes de remplissage automatique de maquettes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; crée les temporal box correpsondnt aux processus de f0, sans tenir compte du texte (la liste des durées de transition est donc absolue
;; et non relative au mélange (voyelles/consonnes)
;; HYPOTHESE : les durées de transition sont compatibles avec les durées des notes
;; index-offset : offset à ajouter aux index pour les connections (par exemple, si la fonction fonctionne avec make-temporal-fof)
(defmethod! make-temporal-f0 ((cseq chord-seq) trans-dur (f0-ref ompatch) (transf0-ref ompatch) &optional (index-offset 0))
  :numouts 3 
  :icon 642
  (let* (;; valeurs de resultat
         (times nil)
         (objs nil)
         (connections nil)
         (nevts (length (lmidic cseq)))
         (tmpdur 0)
         (nextonset 0)
         (trans-dur (list! trans-dur))
         (index 0))
    
    (when (< (length trans-dur) (1- nevts))
      (setf trans-dur (append trans-dur (create-list (- nevts (length trans-dur) 1) (last-elem trans-dur)))))

    (loop for midic in (lmidic cseq)
          for index from 0 to (- (length (lmidic cseq)) 1 )
          for onset in (lonset cseq)
          for dur in (ldur cseq)
          for tdur in (append trans-dur '(0)) do
          (setf nextonset (if (< (+ index 1) nevts) (nth (+ index 1) (lonset cseq)) 0))

          ;; durée de l'élément stationnaire, avant transitions..
          (setf tmpdur (- (if (and (< (+ index 1) nevts) (> (+ onset (car dur)) nextonset )) (- nextonset onset) (car dur)) tdur)) ;;change la durée pour eviter les overlap, puis soustrait la durée de la transition

          ;; ajouter la f0 et la transition de f0
          (setf objs (x-append objs (make-temporal-box f0-ref :offset 0 :posy (round (car (mc->f midic)))  :extend tmpdur :sizey 10)))
          (if (> tdur 0)
              (setf objs (x-append objs (make-temporal-box transf0-ref :offset 0 :posy (round (car (mc->f midic))) :extend (- nextonset (+ onset tmpdur)) :sizey 10)))
            )

          ;;ajout des temps d'onset de la f0 et de la transition 
          (setf times (x-append times (list onset)))
          (if (> tdur 0)
              (setf times (x-append times (+ onset tmpdur)))
            )
          )

    ;; données de connection, obtenues par parsing de la phrase (et des durées de transition)
    (loop for dur in trans-dur do
          (if (> dur 0) 
              (progn
                (setf connections (x-append connections (list (list (+ index index-offset) 0 (+ (+ index index-offset) 1) 0 nil 0) 
                                                              (list (+ (+ index index-offset) 2) 0 (+ (+ index index-offset) 1) 1 nil 0))))
                (setf index (+ index 1))
                )
          )
          (setf index (+ index 1))
          )

    (values times objs connections)
    ))


;; crée les temporal box correpsondnt aux processus de fof
;; HYPOTHESE : les durées de transition sont compatibles avec les durées des notes (il y a une sécurité mais mieux vaut utiliser des données vérifiées)
;; trans-dur = liste des durées de transitions
(defmethod! make-temporal-fof ((cseq chord-seq) (text list) (bd phoneme_DB) trans-dur)
  :numouts 3 
  :icon 642
  (let* (;; valeurs de resultat
         (times nil)
         (objs nil)
         (connections nil)
         (transInfo nil) ;; liste qui permettra de construire les connexions : suite de durées, 0 indiquant la fin d'un element stationnaire : abaabba -> ( X 0 0 X X 0) 
         (nevts (length (lmidic cseq)))
         (tmpdur 0)
         (nextonset 0)
         (trans-index 0) ;; index dans la liste des durées de transition
         (transelts nil)
         (textindex 0) ;; offset dans le texte
         (curStationary 0) ;; indice de la voyelle en cours de traitement, dans le texte
         (transnb 0) ;; nombre de transitions dans une itération
         (trans-inter 20) ;; 20 ms entre deux transitions consécutives (aussi, durée minimale d'une voyelle)
         (tmponset 0)
         (transtimes nil)
         (tmptransdurs nil)
         (tmp 0)
         (index 0)
         )

    (loop for midic in (lmidic cseq)
          for index from 0 to (- (length (lmidic cseq)) 1 )
          for onset in (lonset cseq)
          for dur in (ldur cseq)
          for letter in text do
          (setf nextonset (if (< (+ index 1) nevts) (nth (+ index 1) (lonset cseq)) 0))
                
          ;; durée de l'élément stationnaire, avant transitions..
          (setf tmpdur (if (and (< (+ index 1) nevts) (> (+ onset (car dur)) nextonset )) (- nextonset onset) (car dur))) ;;change la durée pour eviter les overlap
          (setf curStationary textindex) 


          ;; regarder en avant pour ajouter les consonnes/articulations avant la fin de la note, si possible compte tenu de la durée de la note
          (setf transelts nil)
          (setf transtimes nil)
          (setf transnb 0)
          (setf tmptransdurs nil)
          (loop while (and (nth (+ textindex 1) text) (not (isStationary (nth (+ textindex 1) text) bd))) do
                (let ((trdur (if (listp trans-dur) (or (nth trans-index trans-dur) (last-elem trans-dur)) trans-dur)))
                        
                (if (> (- tmpdur trdur) trans-inter)    
                    (progn 
                      (setf tmpdur (- tmpdur trdur))             
                      ;;ajouter un état stationnaire entre deux transitions (sa durée est égale au paramètre (variable test) "trans-inter") [AJOUT DE L'OBJET]
                      (if (> transnb 0)
                            (setf transelts (x-append transelts (make-temporal-box (getStationary (nth curStationary text) bd) :offset 0 
                                                                                   :extend trans-inter :posy (round(- (car(mc->f midic)) 30)) :sizey 25)))
                        )
                      (setf transelts (x-append transelts (make-temporal-box (getTransition (nth (+ textindex 1) text) bd) :extend trdur 
                                                                             :posy (round (- (car (mc->f midic)) 30)) :sizey 25)))
                      
                      ;;ajouter un état stationnaire entre deux transitions (sa durée est égale au paramètre (variable test) "trans-inter") [AJOUT DE L'ONSET]
                      (if (> transnb 0)
                          (progn
                          (setf tmptransdurs (x-append tmptransdurs trans-inter))
                          (setf tmpdur (- tmpdur trans-inter))
                          ))

                      (setf tmptransdurs (x-append tmptransdurs trdur))
                      (setf tmponset (+ onset tmpdur))

                      ;;incréments
                      (setf trans-index (+ trans-index 1))
                      (setf textindex (+ 1 textindex))
                      (setf transnb (+ transnb 1))
                      )

                  (error (concatenate 'string "impossible to add transition symbol" (nth (+ index 1) text)))
                )))

          ;; somme cumulative des durées des elements de transition, pour obtenir la liste de leurs onsets ...
          (setf tmp 0)
          
          (loop for i in tmptransdurs do
                (setf transtimes (x-append transtimes (+ (+ onset tmpdur) tmp)))
                (setf tmp (+ tmp i))
                )
          
          (setf tmptransdurs (or tmptransdurs 0))
          (setf transInfo (x-append transInfo tmptransdurs))
       
          ;; ajouter la voyelle
          (setf objs (x-append objs (make-temporal-box  (getStationary (nth curStationary text) bd) 
                                                        :offset 0 :posy (round(- (car (mc->f midic)) 30)) 
                                                        :extend tmpdur :sizey 25)))
          ;; ajout des transitions
          (setf objs (x-append objs transelts))

          ;;ajout des temps d'onset de la voyelle
          (setf times (x-append times (list onset)))
          ;; ajout des temps d'onset des transitions
          (setf times (x-append times transtimes))
 
          (setf textindex (+ textindex 1))
          )

     ;; données de connection, obtenues par parsing de la phrase (et des durées de transition)
    (loop for dur in transInfo do
          (if (> dur 0) 
              (progn
                (setf connections (x-append connections (list (list index 0 (+ index 1) 0 nil 0) (list (+ index 2) 0 (+ index 1) 1 nil 0))))
                (setf index (+ index 1))
                )
          )
          (setf index (+ index 1))
          )

    (values times objs connections)
    )
  )

(defmethod! make-temporal-fof ((cseq chord-seq) (text string) (bd phoneme_DB) trans-dur)
  (make-temporal-fof cseq (sentence-to-simple-phonemes text) bd trans-dur))



;; appelle les deux methodes make-temporal-f0 et make-temporal-fof, concatene leurs resultats pour etre envoyés dans uen maquette,
;; et renvoie nil s'il y a erreur
(defmethod! make-temporal-fof-f0 ((cseq chord-seq) text (bd phoneme_DB) trans-dur (f0-ref ompatch) (transf0-ref ompatch))
  :numouts 3
  :icon 642
  (let* ((trans-inter 20)
         (format-list (format-trans-durs cseq text bd trans-dur trans-inter)))

  (if (not format-list)
      (list nil nil nil)
    (multiple-value-bind (fofonsets fofpatches fofconnections) (make-temporal-fof cseq text bd (first format-list))
      (multiple-value-bind (f0onsets f0patches f0connections) (make-temporal-f0 cseq (second format-list) f0-ref transf0-ref (length fofonsets))
        (values (append fofonsets f0onsets) (append fofpatches f0patches) (append fofconnections f0connections))
        )))))


;; verifie les durées de transition, par rapport au texte et aux durées des notes
;; renvoie nil s'il y a erreur, sinon une liste contenant : en première position, la liste des durées de transition formatée pour les fofs,
;; en seconde, celle formatée pour les f0
;; trans-inter : durée de l'état stable placé entre deux transitions successives
(defmethod format-trans-durs ((cseq chord-seq) (text list) (bd phoneme_DB) trans-dur (trans-inter real))
  (let* ((nextonset 0)
         (textindex 0)
         (trans-time 0)
         (trans-index 0)
         (fof-list nil)
         (f0-list nil)
         
         (nevts (length (lonset cseq))))
    
    (if (isStationary (first text) bd) ;; le premier element des symboles doit être stationnaire

        (loop for onset in (lonset cseq)
               for dur in (ldur cseq) 
               for index from 1 to nevts
               do
               (setf textindex (+ 1 textindex))
               (setf nextonset (if (< (+ index 1) nevts) (nth (+ index 1) (lonset cseq)) 0)) ;; onset de la prochaine note : vaut 0 si inexistant
               
               ;; calcul des temps de transition
               (setf trans-time 0)
               (loop while (and (nth textindex text) (not (isStationary (nth textindex text) bd))) do
                     (let ((trdur (if (listp trans-dur) (or (nth trans-index trans-dur) (last-elem trans-dur)) trans-dur)))
                       (if (= trans-time 0) ;; première transition
                           (setf trans-time trdur)
                         (progn
                           (setf trans-time (+ trans-time trdur trans-inter))
                           ;;(setf fof-list (x-append fof-list trans-inter))
                           )
                         )
                       (setf fof-list (x-append fof-list trdur))
                       (setf textindex (+ textindex 1))
                       (setf trans-index (+ trans-index 1))
                       ))
               
               ;; durée de l'élément stationnaire, avant transitions..
               (setf tmpdur (if (and (< (+ index 1) nevts) (> (+ onset (car dur)) nextonset )) (- nextonset onset) (car dur))) ;;change la durée pour eviter les overlap

               (if (< (- tmpdur trans-time) 0)
                   (progn 
                     (print "too many transitions at note index # ")
                     (print index)
                     (return nil)
                     )
                 (setf f0-list (x-append f0-list trans-time))
                 )))
    (list fof-list f0-list)
    ))

(defmethod format-trans-durs ((cseq chord-seq) (text string) (bd phoneme_DB) trans-dur (trans-inter real))
  (format-trans-durs cseq (sentence-to-simple-phonemes text) bd trans-dur trans-inter))
  

