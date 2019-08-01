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

;============================================================================
; Phonemes and Morphing
; Raphael Foulon, IRCAM 2012
;============================================================================


(in-package :om)


;;;=========================
;;; DICTIONARY UTILS FOR FOF PHONEMES
;;;=========================

(defclass! phoneme (class-array)
   ((elts :initform 1 :initarg :elts :accessor elts :documentation "number of elements (lines)")
    (transition :accessor transition :initarg :transition :initform nil))
   (:icon 150)
   (:documentation "A PHONEME is a special type of CLASS-ARRAY object defining TRANSITION PROFILES for a number of :keyword parameters.

Add/remove kewords with k/K and name them after the parameter(s) to which the transition may apply (e.g. :freq, :amp, :bw...)

<transition> dteremines the profile of the interpolation used at applying the PHONEME between two Chant events.
"))



;;; creation of a default transition with N formants
(defun default-phoneme (n)
  (om-init-instance 
   (make-instance 'phoneme :elts n :transition (om-make-bpf 'bpf '(0 1) '(0 1) 3))
   `((:freq ,(list (om-make-bpf 'bpf '(0 1) '(1 1) 3)))
      (:amp ,(list (om-make-bpf 'bpf '(0 1) '(1 1) 3)))
      (:bw ,(list (om-make-bpf 'bpf '(0 1) '(1 1) 3))))
   ))


;;trouver l'entrée correspondante à la clef 'phon' dans dico, renvoyer les trajectoires
(defmethod! find-phoneme ((phon string) (dico list))
            :indoc '("phoneme symbol or list" "dictionnary")
            :doc "Finds entries in a trajectories dictionnary.

<phon> is a string or a list
- string: defines an unique entry in the dictionnary
- list of 3 elements :
   - a string which defines the first entry in the dictionnary
   - a string which defines the second entry in the dictionnary
   - a morphing coefficient, given bewteen 0 and 1, which respectively gives the proportion of the first and the second entries in the final result.

The dictionnary is a list contaning the several entries.
Each entry is a list of two elements :
- the symbol matching the trajectories (string)
- the trajectories, ie. a list of 4 elements :
     - the list of the frequencies trajectories
     - the list of the amplitudes trajectories
     - the list of the bandwidth trajectories
     - the morphing trajectory

Each trajectory is defined by a BPF, which values will be multiplied with the formant parameters, with no constraints concerning the abscissa."
   (let ((dic (or dico (def-phonemes-dictionary))))
     (cadr (find phon dic :key 'car :test 'string-equal))))


;;idem, mais morphe deux phonemes entrés (liste : phon1 phon2 coeff-morph)
(defmethod! find-phoneme ((phon list) (dico list))
  (let* ((ctrl 50)
         (phon1 (first phon))
         (phon2 (second phon))
         (morph (third phon))
         (trajs1 (find-phoneme phon1 dico))
         (trajs2 (find-phoneme phon2 dico))
         )

    (loop for slot1 in trajs1 for slot2 in trajs2 collect
          (if (typep slot1 'bpf) ;;si slot de morphing
              (morphed-vals slot1 slot2 morph ctrl)
            (loop for s1 in slot1 for s2 in slot2 collect
                  (morphed-vals s1 s2 morph ctrl)
                  )))))


(defvar *default-phonemes* nil)

(defun def-phonemes-dictionary ()
  (or *default-phonemes*
      (setf *default-phonemes*
            (list 
             (list "b" 
                   (om-init-instance 
                    (make-instance 'phoneme :elts 5 :transition (om-make-bpf 'bpf '(0 1) '(0 1) 3))
                    `((:freq ,(list (om-make-bpf 'bpf '(0.025 0.2 0.5 0.7 1) '(1 0.5 0 0 0.3 1) 3)))
                      (:amp ,(list (om-make-bpf 'bpf '(0.02 0.15 0.85 0.92 1) '(1 0.1 0 0 0.05 1) 3)))
                      (:bw ,(list (om-make-bpf 'bpf '(0 1) '(1 1) 3))))
                    )
                   ))
            )))

      
;;; make simple vowel fofs

(defmethod! make-fof (db-entry onset dur)
  :initvals '((a a) 0 1)
  (multiple-value-bind (fofvalues freqs amps bws)
      (vowel-formants db-entry 'lin)
    (when fofvalues
      (om-init-instance 
       (make-instance 'ch-fof :elts (length fofvalues) 
                      :action-time onset 
                      :dur dur)
       `((:freq ,freqs) (:amp ,amps) (:bw ,bws)))
      )))


;;;===================
;;; FOF MORPHING
;;;===================

;; cree autant de bpfs que d'élements dans les listes y1 et y2, en réalisant les trajets y1
(defmethod! make-bpfs ((x-list list) (y1 list) (y2 list) (ctrl real))
    (loop for yy1 in y1
          for yy2 in y2 
          collect (morphed-val yy1 yy2 ctrl x-list)
          ))



;;morphing d'une ou deux valeurs selon un profil (bpf)
;; ctrl : nombre de points de sampling
;; renvoie une bpf ; on donne en entrée la liste des abscisses
(defmethod! morphed-val ((val1 bpf) (morph bpf) (ctrl real) (x-list list))
  (let* ((x-list (or x-list (third (multiple-value-list (om-sample (list 0 1) ctrl)))))
         (morph-vals (third (multiple-value-list (om-sample morph ctrl)))))
    (om-make-bpf 'bpf 
                 x-list 
                 (om* morph-vals (third (multiple-value-list (om-sample val1 ctrl)))) 
                 (decimals morph))
    ))


;;morphing d'une ou deux valeurs selon un profil (scalaire ou bpf)
(defmethod! morphed-vals (val1 val2 (morph bpf)(ctrl real))
  
  (let* ((morph-vals (third (multiple-value-list (om-sample morph ctrl))))
         (x-list (third (multiple-value-list (om-sample (list 0 1) ctrl))))
         (val1 (if (not (typep val1 'bpf)) (list val1 val1) val1))
         (val2 (if (not (typep val2 'bpf)) (list val2 val2) val2)))
    (om-make-bpf 'bpf 
                 x-list 
                 (om+ (om* (om- 1 morph-vals) (third (multiple-value-list (om-sample val1 ctrl)))) 
                      (om* morph-vals (third (multiple-value-list (om-sample val2 ctrl))))) 
                 (decimals morph))
    ))

(defmethod! morphed-vals ( val1 val2 (morph float)(ctrl real))
  (let* ((x-list (third (multiple-value-list (om-sample (list 0 1) ctrl)))))
    (om-make-bpf 'bpf 
                 x-list 
                 (om+ (om* (om- 1 morph) (third (multiple-value-list (om-sample val1 ctrl))))
                      (om* morph (third (multiple-value-list (om-sample val2 ctrl))))) 
                 3)
    ))


;; recupère les valeurs de slot d'abscisses situées entre start et end, et les ajoute à xs et ys. renvoie le résultat
(defmethod! append-vals ((slots list) (start float) (end float) (xss list) (yss list))
  (mat-trans
   
   (loop for slot in slots 
         for xs in xss 
         for ys in yss 
         collect
         
         (list 
          (loop for s in slot 
                for x in xs 
                collect (x-append x ( if (typep s 'bpf) (om+ start (x-points (bpf-extract s start end))) (list start end)))) 
          (loop for s in slot 
                for y in ys 
                collect (x-append y ( if (typep s 'bpf) (y-points (bpf-extract s  start end)) (list s s)))))
         )))
   


;; idem que pour append-vals, mais "morphe" les valeurs de deux slots (multiplication selon un profile de morphing "morph")
(defmethod! morph-vals ((slots1 list) (slots2 list) 
                        (start float) (end float) 
                        (xss list) (yss list) 
                        (morph list) (ctrl real))
  (mat-trans
   (append
    (loop for slot1 in slots1 
          for slot2 in slots2 
          for xs in xss 
          for ys in yss 
          collect (let* ((xret nil)
                         (yret nil))
                    (setf xret (loop for x in xs 
                                     collect (x-append x (third (multiple-value-list (om-sample (list start end) ctrl))))))
                    (setf yret (loop for y in ys 
                                     for s1 in slot1 
                                     for s2 in slot2 
                                     collect (x-append 
                                              y 
                                              (om+ 
                                               (om* 
                                                (om- 1 morph)
                                                (if (typep s1 'bpf) 
                                                    (third (multiple-value-list (om-sample (bpf-extract s1 start end) ctrl))) 
                                                  (third (multiple-value-list (om-sample (list s1 s1) ctrl)))))
                                               (om* 
                                                morph 
                                                (if (typep s2 'bpf) 
                                                    (third (multiple-value-list (om-sample (bpf-extract s2 start end) ctrl)))
                                                  (third (multiple-value-list (om-sample (list s2 s2) ctrl)))))))))
                    (list xret yret)
                    )
          ))))


;;;=========================
;;; MORPHING FUNCTIONS
;;;=========================

;; crée le morphing entre a et b (listes de flottants ou de bpf!)
(defmethod! inter-trans ((a list) (b list) (morph-curve bpf) (ctrl real))
  (loop for aa in a
        for bb in b collect
        (morphed-vals aa bb morph-curve ctrl)
        ))

(defmethod! gen-inter-fofs ((fof1 ch-fof) (fof2 ch-fof) (transition phoneme)
                            &optional sr onset duration)
    :icon 624
    :indoc '("first FOF event" "second FOF event" "transition (a 'phoneme' instance)" 
                               "sample rate or number of sampling points" "onset" "duration")
    :initvals '(nil nil nil nil nil nil)
    :outdoc '("transition fof event")
            :doc "Generates a FOF event, which represents the transition between the two distant FOF events (<fof1> and <fof2>). 

The transition follows individual profiles fro frequencies, amplitudes and bandwidths from the <transition> phoneme object. 

<transition> can be obtained from a transition dictionary using FIND-PHONEME.
"

        (let* ((dur (or duration (- (abs (- (action-time fof2) 
                                            (+ (action-time fof1) (event-dur fof1))))
                                    (* 2 *min-delta-frames*))))
               (ctrl (if sr (if (integerp sr) sr (round (/ dur sr))) 50))
               (action-time (or onset (+ (min (+ (action-time fof1) (event-dur fof1))
                                              (action-time fof2))
                                         *min-delta-frames*)))
               (freq-curves (or (get-field transition "freq") (list (om-make-bpf 'bpf '(0 1) '(1 1) 6))))
               (amp-curves (or (get-field transition "amp") (list (om-make-bpf 'bpf '(0 1) '(1 1) 6))))
               (bw-curves (or (get-field transition "bw") (list (om-make-bpf 'bpf '(0 1) '(1 1) 6))))
               (morph-curve (or (transition transition) (list (om-make-bpf 'bpf '(0 1) '(0 1) 6)))))
          
          (if (= dur 0)
              (om-beep-msg "Inter FOF duration = 0!!!")
            (let* ((x-list (third (multiple-value-list (om-sample (list 0 dur) ctrl)))))
              (om-init-instance 
               (make-instance 
                'ch-fof 
                :elts (max (elts fof1) (elts fof2))
                :action-time action-time 
                :dur dur) 
               `((:freq ,(make-bpfs x-list freq-curves (inter-trans (get-field fof1 "freq") (get-field fof2 "freq") morph-curve ctrl) ctrl))
                 (:amp ,(make-bpfs x-list amp-curves (inter-trans (get-field fof1 "amp") (get-field fof2 "amp") morph-curve ctrl) ctrl))
                 (:bw ,(make-bpfs x-list bw-curves (inter-trans (get-field fof1 "bw") (get-field fof2 "bw") morph-curve ctrl) ctrl))
                 ))
              ))
          ))




;; prend une liste de fof (ordonnée) et renvoie la liste des fof morphées (sans overlap)
;; 3D : (morph-profile list)
(defmethod! fof-morph ((fofs list) (morph-prof list) &optional sr)
  :indoc '("list of FOF events to morph" "list of morphing profiles (one or two BPFs)")
  :icon 624
  :doc "Generates a FOF event, which represents the morphing between the two overlapping FOF events given as an input. The morphing follows the morphing profiles.

The morphing profile is represented by a list which contains two BPF object. 

Depending of the configuration of the input events, two kinds of morphing can be applied : 2D morphing and 3D morphing.
If only two events are overlapping, 2D morphing will be applied to them, which profile is defined by the first element of the morphing profiles list.
If three events are overlapping, 2D morphing will be applied to the first one and the second ones. Then, the result of this first morphing will be morphed with the third FOF event, with the profile defined by the second element of the morphing profiles list.

The morphing profiles are represented by BPFs.
The y-values of each BPF define the 'morphing coefficient'.
The BPFs will be rescaled, thus there are no constraints concerning the values, which will be normalized."

  (let* ((dur (- (abs (- (action-time (car (last fofs))) 
                         (+ (action-time (car fofs)) (event-dur (car fofs)))))
                 (* 2 *min-delta-frames*)))
         (ctrl (if sr (if (integerp sr) sr (round (/ dur sr))) 50)) ;; contrainte : nombre pair ! (division par deux ci-dessous)
         (bpf1 (bpf-scale (first morph-prof) :y1 0 :y2 1))
         (bpf2 (bpf-scale (second morph-prof) :y1 0 :y2 1))
         (morph2D (third (multiple-value-list (om-sample bpf1 ctrl))))
         (morph2D2 (x-append (first-n morph2D (/ ctrl 2)) (last-n (om- 1 morph2D) (/ ctrl 2))))
         (morph3D (third (multiple-value-list (om-sample bpf2 ctrl))))
         (morph3D2 (x-append (first-n morph3D (/ ctrl 2)) (last-n (om- 1 morph3D) (/ ctrl 2))))
         (morph morph2D)
         (morph2 morph2D2)
         (fof1 (first fofs))
         (fofsbis (cdr fofs))
         (beg1 (get-absolute-time fof1))
         (end1 (+ beg1 (dur fof1)))
         (end0 nil)
         (slots1 (list (get-field fof1 "freq")
                       (get-field fof1 "amp")
                       (get-field fof1 "bw")))
         (xslots (list nil nil nil))
         (yslots (list nil nil nil))
         (no-overlap nil)
         (tmp nil)
         (epsilon 0.00002) ;; evite que deux points soient superposés dans les bpf
         (notfirstloop nil))
       
    (append
     
     (loop for fof2 in fofsbis                                        
           when (>= (get-absolute-time fof2) end1) collect fof1 ;; no overlap => return pred
           do
           (let* ((beg2 (get-absolute-time fof2))
                  (end2 (+ beg2 (dur fof2)))
                  (slots2 (list (get-field fof2 "freq")
                                (get-field fof2 "amp")
                                (get-field fof2 "bw")))
                  (nfmts (max (length (first slots1)) (length (first slots2))))) ;; nombre de composants
                      
             (if (< beg2 end1)
                           
                 (progn

                   (setf no-overlap nil)
                             
                   ;; take 1st part
                   (setf tmp (append-vals slots1 0.0 (float (- (- beg2 beg1) epsilon)) 
                                          (create-list 3 (create-list nfmts nil)) 
                                          (create-list 3 (create-list nfmts nil))))
                   (setf xslots (first tmp))
                   (setf yslots (second tmp))

                   ;; morph                            
                   (if (and notfirstloop (and (< beg2 end1) (< beg2 end0)))
                       (progn (setf morph morph3D) (setf morph2 morph3D2))
                     (progn (setf morph morph2D) (setf morph2 morph2D2)))
                             
                   (if (< end1 end2)
                       (setf tmp (morph-vals slots1 slots2 (float (- beg2 beg1)) (float (- end1 beg1)) xslots yslots morph ctrl))
                     ;; change la 2nde partie de la courbe de morphing
                     (setf tmp (morph-vals slots1 slots2 (float (- beg2 beg1)) (float (- end2 beg1)) xslots yslots morph2 ctrl)))

                   (setf xslots (first tmp))
                   (setf yslots (second tmp))
                   
                   ;; take 2nd part
                   (if (< end1 end2)  
                       (setf tmp (append-vals slots2 (float (+ epsilon (- end1 beg1))) (float (- end2 beg1)) xslots yslots))
                     (setf tmp (append-vals slots1 (float (+ epsilon (- end2 beg1))) (float (- end1 beg1)) xslots yslots)))
                   
                   (setf xslots (first tmp))
                   (setf yslots (second tmp))
                             
                   (setf fof1 (om-init-instance 
                               (make-instance 'ch-fof
                                              :elts (length (first xslots))
                                              :action-time beg1 
                                              :dur (- (max end1 end2) beg1))
                               `((:freq ,(loop for x in (first xslots) for y in (first yslots) 
                                               collect (om-make-bpf 'bpf x y 5)))
                                 (:amp ,(loop for x in (second xslots) for y in (second yslots) 
                                             collect (om-make-bpf 'bpf x y 5)))
                                 (:bw ,(loop for x in (third xslots) for y in (third yslots) 
                                               collect (om-make-bpf 'bpf x y 5))))
                               ))
                   )
                         
               (progn
                 (setf no-overlap t)
                 (setf fof1 fof2))
               )
                        
             ;;update
             (setf end0 end1)
             (setf notfirstloop t)
             (setf beg1 (get-absolute-time fof1))
             (setf end1 (+ beg1 (dur fof1)))                     
             (setf slots1 (list (get-field fof1 "freq")
                                (get-field fof1 "amp")
                                (get-field fof1 "bw")))

             ))

     (if no-overlap
         (last fofs) ;; no overlap => return pred
       (list fof1)))
    ))


(defmethod! fof-morph ((fofs list) (morph-prof bpf) &optional sr)
  (fof-morph fofs (list morph-prof morph-prof)))

