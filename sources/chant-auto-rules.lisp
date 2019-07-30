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

;;;==============================================================
;;; CHANT automatic parameter correction rules 
;;; J. Bresson, IRCAM 2010
;;; Transcribed from Max/MSP objects source code by F. Iovino and G. Eckel, 1994 IRCAM
;;; Original CHANT rules by X. Rodet and Y. Potard, IRCAM 1985
;;;==============================================================


(in-package :om)


;;;==============================================================
;;; Automatic calculation of the formants' bandwidths.
;;; 'ATB' flag in CHANT
;;; From chant_autobw.c (F. Iovino, G. Eckel, 1994 IRCAM)
;;; Bandwidth are calculated depending on the center frequencies of the formants and 
;;; using a global parabolic function determined by 3 reference (freq bandwidth) pairs.
;;;==============================================================

;;; Default reference curve is '((200 75) (500 75) (4000 150))
;;; In the CHANT manual (1985) gives the following values : '((200 50) (500 40) (4000 100))

;;; f is a 3x3 array 
;;; b is a list of length 3.
(defun rsolve (f b)
  (let* ((v (- (aref f 0 2) (aref f 1 2)))
         (u (- (aref f 0 1) (aref f 2 1)))
         (w (- (nth 0 b) (nth 1 b)))
         (x (- (aref f 0 1) (aref f 1 1)))
         (y (- (aref f 1 1) (aref f 2 1)))
         (z (/ x y))
         (d (- v (* (- (aref f 1 2) (aref f 2 2)) z)))
         c1 c2 c3)
    (unless (or (zerop x) (zerop u) (zerop d))
      (setq c3 (/ (- w (* (- (nth 1 b) (nth 2 b)) z)) d))
      (setq c2 (/ (- w (* c3 v)) x))
      (setq c1 (- (nth 0 b) (* c2 (aref f 0 1)) (* c3 (aref f 0 2))))
      (list c1 c2 c3))
    ))
         

(defun GetPolCoefs (fref bref)
  (let ((matrix (make-array '(3 3))))
    (loop for i from 0 to 2 do
          (setf (aref matrix i 0) 1.0)
          (setf (aref matrix i 1) (log (nth i fref)))
          (setf (aref matrix i 2) (expt (aref matrix i 1) 2.0)))
    (or (rsolve matrix bref)
        (om-beep-msg "Illegal reference values in AUTOBW"))))


(defmethod! autobw (freqlist &optional (ref-curve '((200 75) (500 75) (4000 150))))
   :icon 600
   :initvals '((609. 1000. 2450. 2700. 3240.)
               ((200 75) (500 75) (4000 150)))
   :indoc '("formants' frequecy list" "3 points of the reference function bw(f)")
    :doc "AUTOBW calculates the bandwidth of the formants depending on the frequencies (<freqlist>), using a global parabolic function determined by 3 reference (freq bandwidth) pairs (<ref-curve>).

The result is a list of calculated formant bandwidths.

<freqlist> can also be a list of lists of a list of equally-sampled BPFs containing the evolution of every formant frequencies.
The result will be a list of list of bandwidth.

This function is adapted from the original CHANT rules as implemented in CHANT by X. Rodet and Y. Potard (1984) ('atb' flag) and in the chant_autobw object for Max by F. Iovino and, G. Eckel (1994).
Documentation adapted from the CHANT manual, P.-F. Baisnée and the Chant group, 1985.
"
    
    (cond ((listp (car freqlist))

           (let* ((states (mat-trans freqlist)))
             (mat-trans (loop for st in states collect (autobw st ref-curve)))))

          ((bpf-p (car freqlist)) ;;; we assume all other elements are BPFs!
           (let ((xpts (x-points (car freqlist))))
             (loop for bwlist in (autobw (mapcar #'y-points freqlist) ref-curve)
                   collect (om-make-bpf 'bpf xpts bwlist (decimals (car freqlist))))
             ))
          
          (t 
           (let* ((ref-params (mat-trans ref-curve))
                  (cpol (GetPolCoefs (car ref-params) (cadr ref-params))))
             (when cpol
               (loop for f in freqlist collect
                     (let ((LogFreq (log f)))
                       (+ (nth 0 cpol) 
                          (* (nth 1 cpol) LogFreq)
                          (* (nth 2 cpol) LogFreq LogFreq))))
               ))
           )))




;;;==============================================================
;;; Automatic calculation of the amplitude of the formants
;;; From chant_autoamp.c (F. Iovino, G. Eckel, 1994 IRCAM)
;;; 'ATA' flag
;;; Calculates the amplitude of the formants depending on the frequencies
;;; (takes into account the interactions between formants)
;;; also scales the formant amplitudes with a function that varies as 1/f
;;; (correct in approximating natural voice spectra)
;;;==============================================================

;;;-------------------------------
;;; also used in FCOMP
;;;
(defun InitChp ()
  (mapcar #'(lambda (n) (expt 10 (/ n 20.0)))
          '(0 1 2.5 5 9 14.5 21.5 30 41 51 55 55 55 55 55)))
;ms_1111
; extend table to 7500 Hz, repeat last val, to avoid nil when indexing it
;          '(0 1 2.5 5 9 14.5 21.5 30 41 51 55 55)))


(defun GetIndex (CentFreq)
  (let ((i (floor (/ CentFreq 500.0)))
        (LIM 11))
;ms_1111
;        (LIM 11))
    (if (>= i LIM) LIM
      (if (<= i 0) 0 i))))

(defun ContFactor (CentFreq freqs bandwidths)
  (let ((result 1)
        (epsilon 0.00000001))
    (loop for f in freqs
          for bw in bandwidths do
          (let* ((mbw (max bw epsilon))
                 (mbw2 (/ (* mbw mbw) 4.0))
                 (Numer (+ (* f f) mbw2))
                 (Denom (* (expt (+ (* (- f CentFreq) (- f CentFreq)) mbw2) 0.5)
                           (expt (+ (* (+ f CentFreq) (+ f CentFreq)) mbw2) 0.5))))
            (setq result (* result (/ Numer Denom)))))
    result))

;(setf aaa (INitChp))
;(GetIndex 8400.)
;(nth 12 aaa)
;;;
;;;-------------------------------

(defmethod! autoamp (freqlist bwlist &optional (scaler 1.0))
   :icon 600
   :initvals '((609. 1000. 2450. 2700. 3240.) 
               (77.64382 88.43109 122.9401 127.8438 137.6589) 1.0)
   :indoc '("list of formants' frequencies" "list of formants' bandwidths" "amplitude scaler factor")
   :doc "AUTOAMP calculates the amplitude of the formants depending on the frequencies (<freqlist>) and bandwidths (<bwlist>), taking into account the interactions between formants (reinforcement of the amplitudes of formants approaching one another).

It also scales the formant amplitudes with a function that varies as 1/f (approximation of a natural voice spectrum).

The result is a list of calculated formant amplitudes.

<freqlist> and/or <bwlist> can also be lists of lists or a list of equally-samples BPFs containing the evolution of every formant frequencies and bandwidths.
The result will be a list of lists of amplitudes or of BPFs.

This function is adapted from the original CHANT rules as implemented in CHANT by X. Rodet and Y. Potard (1984) ('ata' flag) and in the chant_autoamp object for Max by F. Iovino and, G. Eckel (1994).
Documentation adapted from the CHANT manual, P.-F. Baisnée and the Chant group, 1985.
"
   (cond 
    ((and (bpf-p (car freqlist)) (bpf-p (car bwlist)))
     (let ((xpts (x-points (car freqlist))))
       (loop for amplist in (autoamp (mapcar #'y-points freqlist) 
                                     (mapcar #'y-points bwlist)
                                     scaler)
             collect (om-make-bpf 'bpf xpts amplist 6))
       ))
    
    ((bpf-p (car freqlist))
     (let ((xpts (x-points (car freqlist))))
       (loop for amplist in (autoamp (mapcar #'y-points freqlist) bwlist scaler)
             collect (om-make-bpf 'bpf xpts amplist 6))
       ))
    
    ((bpf-p (car bwlist))
     (let ((xpts (x-points (car bwlist))))
       (loop for amplist in (autoamp freqlist (mapcar #'y-points bwlist) scaler)
             collect (om-make-bpf 'bpf xpts amplist 6))
       ))
    
    ((and (listp (car freqlist)) (listp (car bwlist)))
     (mat-trans (loop for lfr in (mat-trans freqlist)
                      for lbw in (mat-trans bwlist)
                      collect (autoamp lfr lbw scaler))))
    
    ((listp (car freqlist))
     (mat-trans (loop for lfr in (mat-trans freqlist)
                      collect (autoamp lfr bwlist scaler))))
    
    ((listp (car bwlist))
     (mat-trans (loop for lbw in (mat-trans bwlist)
                      collect (autoamp freqlist lbw scaler))))
    
    (t 
          (let ((chp (InitChp)))
            (loop for f in freqlist collect
                  (let* ((Ind (GetIndex f))
                         (ChpCont (+ (nth Ind chp) 
                                     (* (- (nth (1+ Ind) chp) (nth Ind chp)) 
                                        (/ (- f (* Ind 500.0)) 500.0)))))
                    (* scaler (ContFactor f freqlist bwlist) (/ ChpCont f)))
                  )))
         ))



;;;==============================================================
;;; Automatic calculation of the complement
;;; From fcomp.c (F. Iovino, G. Eckel, 1994 IRCAM)
;;; ('ATC' flag)
;;; The 'complement' formant reinforces the first formant, the fundamental partial and the low register.
;;; This formant is by default at 80 Hz with bandwidth = 200 Hz
;;; NOTE (fi 10/05/94) FCOMP IS TO BE USED ONLY IF ATA HAS BEEN USED
;;;==============================================================
;;; Note : its just the same formula as in 'autoamp' with frcomp and bwcomp
                          

;(defmethod! fcomp (freqlist bwlist &optional (frcomp 80.0) (bwcomp 200.0))
;   :icon 600
;   :initvals '((609. 1000. 2450. 2700. 3240.) 
;               (77.64382 88.43109 122.9401 127.8438 137.6589) 80.0 200.0)

; If <merge-output> it T (default), the returned values are those of the complement formant merged with the rest of the formants. Else, FCOMP returns only the complement formant.

;ms_1111
(defmethod! fcomp (freqlist bwlist &optional (ampscaler 1.0) (frcomp 80.0) (bwcomp 200.0))
   :icon 600
   :initvals '((609. 1000. 2450. 2700. 3240.) 
               (77.64382 88.43109 122.9401 127.8438 137.6589) 80.0 200.0)
   :outdoc '("formant values" "frequency" "amplitude" "bandwidth")
   :numouts 4
   :indoc '("list of formants' frequencies" "list of formants' bandwidths" "amplitude scaler" "complement formant frequency" "complement formant bandwidth")
   :doc "FCOMP calculates the amplitude of a complement formant to be added to the spectrum. 
The 'complement' formant can be added to reinforce the first formant of a spectrum, the fundamental partial and the low register.

The result is a list with (<frcomp> calculated-amplitude <bwcomp>)

Note: the amplitude is computed following the rules in AUTOAMP.

This function is adapted from the original CHANT rules as implemented in CHANT by X. Rodet and Y. Potard (1984) ('atc' flag) and in the chant_fcomp object for Max by F. Iovino and, G. Eckel (1994).
Documentation adapted from the CHANT manual, P.-F. Baisnée and the Chant group, 1985.
"
   (let* ((Ind (GetIndex frcomp))
          (chp (InitChp))
          (ChpCont (+ (nth Ind chp) 
                      (* (- (nth (1+ Ind) chp) (nth Ind chp)) 
                         (/ (- frcomp (* Ind 500.0)) 500.0))))
;ms_111
          (AMP (* ampscaler (ContFactor frcomp freqlist bwlist) (/ ChpCont frcomp))))
;          (AMP (* (ContFactor frcomp freqlist bwlist) (/ ChpCont frcomp)))
     (values (list frcomp AMP bwcomp) frcomp AMP bwcomp)))


; (fcomp '(609. 1000. 2450. 2700. 3240.) '(77.64382 88.43109 122.9401 127.8438 137.6589))


;ms_1111, ms_1205 (replaced optional with key), added lin
(defmethod! comp-formants (lformants &key (frcomp 80.0) (amp 'lin) (ampscaler 1.0) (bwcomp 200.0))
            :icon 600
            :initvals '(((800 1.0 80) (1150 0.63095737 90) (2800 0.1 120) (3500 0.015848933 130) (4950 0.001 140)))
            :numouts 4
            :indoc '("List of formants or single formant" "Amplitude scaler" "Complement formant frequency" "Complement formant bandwidth")
            :doc "COMP_FORMANTS is the same as FCOMP, but requires a complete list of formants, as well as the optional parameters of FCOMP.
It returns a list of formants, containing the complement, as well as the separate freqs, amps, bws"

   (let* ((trans (mat-trans (if (listp (car lformants)) lformants (list lformants))))
          (freqs (first trans))
          (amps (second trans))
          (bws (third trans))
          (comp (fcomp freqs bws ampscaler frcomp bwcomp))
          (lfreqs (x-append (first comp) freqs))
          (lamps (x-append (second comp) amps))
          (lamps (if (equal amp 'lin) (db->lin lamps) lamps))
          (lbws (x-append (third comp) bws)))
      (values (mat-trans (list lfreqs lamps lbws)) lfreqs lamps lbws)))

; (comp-formants '((800 0 80) (1150 -4 90) (2800 -20 120) (3500 -36 130) (4950 -60 140)) :frcomp 200)
; (comp-formants '((800 0 80) (1150 -4 90) (2800 -20 120) (3500 -36 130) (4950 -60 140)) :frcomp 200 :amp 'db)


;;;==============================================================
;;; Adjusting the amplitude of the formants
;;; From cslope.c (F. Iovino, G. Eckel, 1994 IRCAM)
;;; Takes into account the dynamic amplitude, the type of voice, and the fundamental freq.
;;;==============================================================

;;; Note in the CHANT Manual, ajus1 = 1

(defmethod! cslope (freqlist amplist slope voice &optional (fund 100.0) (f0moyen 200.0) (coefamp 1.0) (maxamp 1.0) (ajus '(0.0 5.7 0.0)))
   :icon 600
   :initvals '((609. 1000. 2450. 2700. 3240.)
               (0.027797 0.013660 0.006961 0.007831 0.001811)
               -1 'f
               100.0 200.0 1.0 1.0 (0.0 5.7 0.0))
   :indoc '("list of formants' frequencies (Hz)" 
            "list of initial amplitudes"
            "slope of the spectrum" "type of voice (male or female)"
            "fundamental frequency" "middle frequency of the range of the voice"
            "dynamic amplitude (modifies the spectrum)" "max amplitude (scaler)"
            "CHANT adjustment parameters")
   :menuins '((3 (("Female" 'f) ("Male" 'm))))
   :doc "CSLOPE allows to control the spectrum (<freqlist>, <amplist>) in relation to the dynamic amplitude of the sound (<coefamp>), the type of voice which is being used (<voice>), and the fundamental frequency (<fund>).

The result is a list of adjusted formant amplitudes.

The following formula is used to maintain humanly possible ratios between amplitude and spectrum :
- adjusted-formant-amplitude = given-amplitude * <coefamp> * f0-linked-scaler.

f0-linked-scaler varies with the fundamental frequency and has a negative slope (decreases as fundamental increases).
It is given by the following formulae :
 3 + 1.1 * (400 - <fund>) / 300 for male voices (<voice> = 'm)
 0.8 + 1.05 * (1000 - <fund>) / 1250 for female (or castrati) voices (<voice> = 'f)

If <slope> = -1 (or negative) the previous formulas are used (and the actual value of <slope> is ignored).

If <slope> is positive (>= 0) it is used as a scaler on the amplitudes following the formula :
 <slope> * exp(a1 * atan(a2 * log(<fund> / <f0moyen>)))

In both case, another adjustment is computed with an additional scaler = <coefamp> * (<fund> - <f0moyen>)^a3

Notes: 
1) only the formant whose center frequency is higher that that of the first formant are adjusted.
2) a1 a2 and a3 are the element of parameter <ajus> corresponding to standard CHANT parmeters.

This function is adapted from the original CHANT rules as implemented in CHANT by X. Rodet and Y. Potard (1984) and in the chant_cslope object for Max by F. Iovino and, G. Eckel (1994).
Documentation adapted from the CHANT manual, P.-F. Baisnée and the Chant group, 1985.
"
   
   (let* ((epsilon 0.00000001)
          (cslope (if (zerop slope) epsilon slope))
          (tino (* coefamp (expt (/ fund f0moyen) (nth 2 ajus))))
          (rslope (* cslope (exp (* (nth 0 ajus) 
                                    (atan (* (nth 1 ajus) 
                                             (log (/ fund f0moyen)))))))))
     (loop for f in freqlist
           for a in amplist collect
           (let ((ampi (* a tino)))
             (if (> f (car freqlist))
               (if (< cslope 0)
                   (case voice  
                     ('m (* ampi maxamp coefamp 
                            (+ (+ 3.0 (/ (- 400.0 fund) 300.0))
                               (* 0.1 (/ (- 400 fund) 300.0)))))
                     ('f (* ampi maxamp coefamp 
                            (+ (+ 0.8 (/ (- 1000.0 fund) 1250.0))
                               (+ 0.05 (* 0.1 (/ (- 1000.0 fund) 1250.0))))))
                     (otherwise nil))
                 (* ampi rslope))
               ampi))
           )))
     

;;;==============================================================
;;; Adjustment of the 1st and 2nd formant frequencies (bending)
;;; From autobend.c (F. Iovino, G. Eckel, 1994 IRCAM)
;;;==============================================================
;ms_1205, made more general, accepts/returns lfreqs or entire formants
(defmethod! autobend (lfreqs fund)
   :icon 600
   :initvals '((1000. 1500. 2450.) 200.0)
   :indoc '("list of formants' frequencies (Hz) or complete formants" "fundamental frequency (Hz)")
   :doc "AUTOBEND adjusts the frequencies of the formants depending on the fundamental frequency (<freqlist>).

It is base on a study by J. Sundberg describing showing that :
- The first formant remains fixed until the fundamental reaches its frequency, and then tracks the fundamental at the same frequency.
- The second formant moves downward in pitch as the fundamental moves > 200 Hz, but when the interval between the fundamental and its frequency has decreased to an octavve + env. 30Hz, this interval is maintained and the formant moves in parallel.

The result is a list of calculated formant frequencies.
Note : only the first and second frequencies are modified.

This function is adapted from the original CHANT rules as implemented in CHANT by X. Rodet and Y. Potard (1984) and in the chant_autobend object for Max by F. Iovino and, G. Eckel (1994).
Documentation adapted from the CHANT manual, P.-F. Baisnée and the Chant group, 1985.
"
   (let* ((freqlist (if (listp (car lfreqs)) (extract-freqlist lfreqs) lfreqs))
          (bendfreqs (copy-list freqlist)))
     (if (<= (nth 0 freqlist) fund)
         (setf (nth 0 bendfreqs) fund))
     (if (and (>= (nth 1 freqlist) 1300.0) (>= fund 200.0))
         (setf (nth 1 bendfreqs) (- (nth 1 bendfreqs) (* (/ (* (- fund 200) 2.) 3.0)
                                                         (/ (- (nth 1 freqlist) 1300) 700.0)))))
     (if (<= (nth 1 bendfreqs) (+ (* fund 2) 30))
         (setf (nth 1 bendfreqs)  (* 2 (+ fund 30))))
     (if (listp (car lfreqs))
         (mat-trans (list bendfreqs (cadr (mat-trans lfreqs)) (caddr (mat-trans lfreqs)) (cadddr (mat-trans lfreqs))))
       bendfreqs)))

;(autobend '(800 1000 3000) 1000)
;(autobend (vowel-forms 'si) 1000)

;ms_1205
(defun extract-freqlist (lfq)
  (car (mat-trans lfq)))
;(extract-freqlist (vowel-forms 'sa))

;;;==============================================================
;;; FOF band width and skirt width to tex transformation
;;; From chant_autotex.c (G. Eckel, 1994 IRCAM)
;;;==============================================================

(defmethod! autotex (bw sw &optional (level 40.0))
   :icon 600
   :initvals '(nil nil 40.0)
   :indoc '("formants' bandwidth" "formants' skirt width" "level")
   :doc "AUTOTEX calculates the FOF 'tex' parameter (in seconds) from the bandwidth and skirt width of a formant.

This function is adapted from the chant_autotex object for Max by G. Eckel (1994).
"
   (let* ((r (expt 10 (/ level 10)))
          (r2 (* r r))
          (bw2 (* bw bw))
          (bw4 (* bw2 bw2))
          (sw2 (* sw sw))
          (sw4 (* sw2 sw2))
          (v1 (sqrt (* (+ bw2 (* 4 sw2)) (+ bw2 sw2))))
          (rep nil))
     (when (>= v1 0)
       (let ((v2 (- (* bw2 r2) (* 4 sw2))))
         (setq v2 (/ v2 (- (* 4 bw sw r v1) (+ (* 16 sw4) (* bw4 r2)))))
         (when (>= v2 0)
           (setf rep (sqrt v2)))))
     (or rep (om-beep-msg (format nil "Wrong BW or SW: ~D ~D" bw sw)))
     ))
 
(defmethod! autotex ((bw list) (sw list) &optional (level 40.0))
     (mapcar #'(lambda (b s) (autotex b s level)) bw sw))

;;;==============================================================
;;; Correction of the spectrum
;;; From spcor.c (F. Iovino, G. Eckel, 1994 IRCAM)
;;;==============================================================

(defmethod! spcor (amplist bwlist texlist)
   :icon 600
   :initvals '((1.0 1.0 1.0)
               (36.97 49.02 69.69)
               (0.002 0.002 0.002))
   :indoc '("list of initial formants' amplitudes" 
            "list of bandwidths" "list of attack times (sec.)")
   :doc "SPCOR adjusts the formants amplitudes <amplist> according to the duration of the attacks of the FOFs (<texlist>) and the bandwidths of the respective formants (<bwlist>).

When the formants have different bandwidths and skirt-widths (skirt-width is directly related to the attack time -- see AUTOTEX function) their respective corresponding energy in the spectrum is modified due to superposition effects.


This function is adapted from the original CHANT rules as implemented in CHANT by X. Rodet and Y. Potard (1984) and in the chant_spcor object for Max by F. Iovino and, G. Eckel (1994).
Documentation adapted from the CHANT manual, P.-F. Baisnée and the Chant group, 1985.
"
   
     (loop for amp in amplist 
           for bw in bwlist 
           for tex in texlist collect
           (let* ((beta2 (/ 1 (float (* tex tex))))
                  (alphbw (* bw tex pi))
                  (num (* amp 0.01 pi bw (+ (* bw bw) beta2)))
                  (denom (* (+ (/ 1 (+ 1 (* alphbw (+ 1 (* alphbw 0.5))))) 1.0)
                            beta2)))
             (/ num denom))))
     
#|
#define PI 3.141592
#define NFORM 3

float amp[NFORM]  = { 1., 1., 1. };
float bw[NFORM]   = { 36.97, 49.02, 69.69 };
float tex[NFORM]  = { .002, .002, .002};  
float result[NFORM];

void chant_spcor (float *amp, float *bw, float *tex, int n, float *result)
{
	float fac, num, denom, beta2, alphbw;
	int i;	
	for (i=0; i<n; i++) {
		beta2 = 1. / (tex[i]*tex[i]);
		alphbw = bw[i] * tex[i] * PI; 
		num = amp[i] * .01 * PI * bw[i] * (bw[i] * bw[i] + beta2);
		denom = (1. / (1. + alphbw*(1.+ alphbw *.5))  +  1.)* beta2;
		result[i] = num/denom;
		printf("%d  %f\n", i, result[i]);
	}
}

main(){
	chant_spcor (amp, bw, tex, NFORM, result);
}
|#




