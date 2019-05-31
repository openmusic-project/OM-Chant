;;;===================================================
;;; OM-CHANT
;;; Control CHANT synthesis from OpenMusic
;;;
;;; Formant filter data 
;;; Jean Bresson, IRCAM 2010
;;;===================================================

(in-package :om)

;;;============================
;;; FORMANT FORMAT IN OM-CHANT:
;;; ((f1 a1 bw1) (f2 a2 bw2) ...)
;;; amplitudes are assumed to be in DB if <= 0 or LINEAR if not
;;;
;;; FORMANT DB FORMAT = list of formants OR (symbol FORMANT)
;;; e.g.:
;;; ((E1 ((f1 a1 bw1) (f2 a2 bw2) ...)) (E2 ((f1 a1 bw1) (f2 a2 bw2) ...)) ...)
;;; The 'symbol' can also be a list, e.g. '(s a) '(t o), etc.
;;;============================

;;; mini-utils
(defun f-freq (formant) (car formant))
(defun f-amp (formant) (cadr formant))
(defun f-bw (formant) (caddr formant))

(defmethod amp-format ((val number))
  (if (<= val 0) 'db 'lin))

(defmethod amp-format ((val cons))
  (if (<= (f-amp val) 0) 'db 'lin))

(defmethod amp-format ((val t)) nil)

;;;============================
;;; USELESS UTILITY: READ FORMANTS IN A TEXT FILE
;;;============================
(defun get-line-data (line)
  (cddr (data-from-line line)))

(defun get-filter-name (line)
  (let ((data (cddr (data-from-line line))))
    (list (car data) (cadr data))))

(defun get-formants (file)
  (let ((rep nil)
        name fr am bw)
    (with-open-file (f file)
      (loop while (not (stream-eofp f)) do
            (setf name (get-filter-name (read-line f nil nil)))
            (when name
              (read-line f)
              (setf fr (get-line-data (read-line f nil nil)))
              (setf am (get-line-data (read-line f nil nil)))
              (setf bw (get-line-data (read-line f nil nil)))
              (read-line f nil nil)
              (push (list name (mat-trans (list fr am bw))) rep))
            ))
    (reverse rep)))

;; (get-formants (om-make-pathname :directory (lib-resources-folder (find-library "OM-Chant")) :name "formants" :type "txt"))

;;;============================
;;; DEFAULTS OM FORMANTS
;;;============================

(defvar *default-formants* nil)

(defun get-default-formants ()
  (or *default-formants*
      (setf *default-formants*
            '(
              ((s a) ((800 0 80) (1150 -6 90) (2900 -32 120) (3900 -20 130) (4950 -50 140)))
              ((s e) ((350 0 60) (2000 -20 100) (2800 -15 120) (3600 -40 150) (4950 -56 200)))
              ((s i) ((270 0 60) (2140 -12 90) (2950 -26 100) (3900 -26 120) (4950 -44 120)))
              ((s o) ((450 0 40) (800 -11 80) (2830 -22 100) (3800 -22 120) (4950 -50 120)))
              ((s u) ((325 0 50) (700 -16 60) (2700 -35 170) (3800 -40 180) (4950 -60 200)))

              ((a a) ((800 0 80) (1150 -4 90) (2800 -20 120) (3500 -36 130) (4950 -60 140)))
              ((a e) ((400 0 60) (1600 -24 80) (2700 -30 120) (3300 -35 150) (4950 -60 200)))
              ((a i) ((350 0 50) (1700 -20 100) (2700 -30 120) (3700 -36 150) (4950 -60 200)))
              ((a o) ((450 0 70) (800 -9 80) (2830 -16 100) (3500 -28 130) (4950 -55 135)))
              ((a u) ((325 0 50) (700 -12 60) (2530 -30 170) (3500 -40 180) (4950 -64 200)))

              ((c a) ((660 0 80) (1120 -6 90) (2750 -23 120) (3000 -24 130) (3350 -38 140)))
              ((c e) ((440 0 70) (1800 -14 80) (2700 -18 100) (3000 -20 120) (3300 -20 120)))
              ((c i) ((270 0 40) (1850 -24 90) (2900 -24 100) (3350 -36 120) (3590 -36 120)))
              ((c o) ((430 0 40) (820 -10 80) (2700 -26 100) (3000 -22 120) (3300 -34 120)))
              ((c u) ((370 0 40) (630 -20 60) (2750 -23 100) (3000 -30 120) (3400 -34 120)))

              ((t a) ((650 0 80) (1080 -6 90) (2650 -7 120) (2900 -8 130) (3250 -22 140)))
              ((t e) ((400 0 70) (1700 -14 80) (2600 -12 100) (3200 -14 120) (3580 -20 120)))
              ((t i) ((290 0 40) (1870 -15 90) (2800 -18 100) (3250 -20 120) (3540 -30 120)))
              ((t o) ((400 0 70) (800 -10 80) (2600 -12 100) (2800 -12 130) (3000 -26 135)))
              ((t u) ((350 0 40) (600 -20 60) (2700 -17 100) (2900 -14 120) (3300 -26 120)))

              ((b a) ((600 0 60) (1040 -7 70) (2250 -9 110) (2450 -9 120) (2750 -20 130)))
              ((b e) ((400 0 40) (1620 -12 80) (2400 -9 100) (2800 -12 120) (3100 -18 120)))
              ((b i) ((250 0 60) (1750 -30 90) (2600 -16 100) (3050 -22 120) (3340 -28 120)))
              ((b o) ((400 0 40) (750 -11 80) (2400 -21 100) (2600 -20 120) (2900 -40 120)))
              ((b u) ((350 0 40) (600 -20 80) (2400 -32 100) (2675 -28 120) (2950 -36 120)))))
      ))


;;; compat marco's patches
(defmethod! vowel-forms ((v list) &key (amp 'lin))
   :numouts 4
   (let (all-list f-list a-list b-list)
     (mapcar #'(lambda (vo) (multiple-value-bind (all f a b)
                                (vowel-forms vo :amp amp)
                              (push all all-list)
                              (push f f-list)
                              (push a a-list)
                              (push b b-list)))
             v)
     (values (reverse all-list) (reverse f-list) (reverse a-list) (reverse b-list))))
             
(defmethod! vowel-forms (v &key (amp 'lin))
    (vowel-formants (list (interne (elt (string v) 0)) (interne (elt (string v) 1))) amp))



;;; intelligent format selection (from one of the three data bases)
(defmethod! all-vowel-forms (vowel &key (amp 'lin) (high-formants nil) database)
    :icon 530
    :initvals '((a a) lin nil nil)
    :numouts 4
    :indoc '("a sung vowel" "amplitude scale: 'lin or 'db" "high-formants arguments" "formant database")
    :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")
    :doc "Look either in the given data base or in the three available databases for a given formant.
High-formants must be a list with the arguments to pass to add-formants (n delta-fq delta-amp delta-bw).
If it is not nil, add-formants will be called and a larger amount of formants will be returned.
"
    (let* ((all-db (if database database (x-append (get-default-formants) (get-chant-formants) (get-extended-formants))))
           (formant (database-formants vowel all-db amp))
           (formantrans (mat-trans formant)))

      (when high-formants 
        (setf formant (add-formants formant (first high-formants) (second high-formants) (third high-formants) (fourth high-formants)))
        (setf formantrans (mat-trans formant)))
      
      (values (mat-trans formantrans) (car formantrans) (cadr formantrans) (caddr formantrans) (cadddr formantrans))))


(defmethod! vowel-formants (vowel &optional (amp 'lin))
    :icon 530
    :initvals '((a a) lin)
    :numouts 4
    :indoc '("a sung vowel" "amplitude scale: 'lin or 'db")
    :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")
    :menuins '((0 (("Alto A" '(a a))
                   ("Alto E" '(a e))
                   ("Alto I" '(a i))
                   ("Alto O" '(a o))
                   ("Alto U" '(a u))
                   ("Bass A" '(b a))
                   ("Bass E" '(b e))
                   ("Bass I" '(b i))
                   ("Bass O" '(b o))
                   ("Bass U" '(b u))
                   ("Countertenor A" '(c a))
                   ("Countertenor E" '(c e))
                   ("Countertenor I" '(c i))
                   ("Countertenor O" '(c o))
                   ("Countertenor U" '(c u))
                   ("Soprano A" '(s a))
                   ("Soprano E" '(s e))
                   ("Soprano I" '(s i))
                   ("Soprano O" '(s o))
                   ("Soprano U" '(s u))
                   ("Tenor A" '(t a))
                   ("Tenor E" '(t e))
                   ("Tenor I" '(t i))
                   ("Tenor O" '(t o))
                   ("Tenor U" '(t u))))
               (1 (("dB" 'db) ("linear" 'lin))))
    :doc "Returns the values of the main formants in the spectral envelope of <vowel> (click to select from the input menu).

If <vowel> is a number, returns the nth vowel in the list. If the formant is not found in the database, nil is returned.

Values are formatted as '((frequency1 amplitude1 bandwidth1) (frequency2 amplitude2 bandwidth2) ...) in the first output.
2nd output is the list of frequencies in Hz.
3rd output is the lis of amplitudes in dB or linear (depending on <amp>).
4th output is the list of bandwidths in Hz.
"
    (let ((exist-formant (if (integerp vowel) (nth vowel (get-default-formants))
                           (find vowel (get-default-formants) :key 'car :test 'equal))))
      (when exist-formant
        (let* ((formants (copy-list (cond ((integerp vowel) 
                                           (cadr (nth vowel (get-default-formants))))
                                          ((listp vowel) 
                                           (cadr (find vowel (get-default-formants) :key 'car :test 'equal)))
                                          (t vowel))))
               (vals (mat-trans formants)))
          (when (equal amp 'lin)
            (setf (nth 1 vals) (db->lin (nth 1 vals))))
          (values (mat-trans vals) (first vals) (second vals) (third vals))))))

;;;============================================
;;; GET FORMANTS FROM USER'S DATABASE
;;;============================================

(defun guess-amp-format (list)
  (let ((formats (loop for elt in list collect (amp-format elt))))
    (if (and (find 'lin formats) (find 'db formats))
        (om-beep-msg "Problem in database: amplitudes seem to be LIN and DB...")
      (car formats))))


(defmethod! get-database-ids (database)
    :icon 530
    :indoc '("A formant database")
    :doc "Return all the keys associated to a given formant database.

If no database id provided the default OM formant database will be used instead."
    (let ((database (if (symbolp database) (eval database) 
                      (or database (get-default-formants)))))
      (loop for vowel in database
            collect (car vowel))))

(defmethod! random-vowel (database)
    :icon 530
    :indoc '("A formant database")
    :numouts 2
    :outdoc '("Formantic values" "Database ID")
    :doc "Return the formantic data associated to a random vowel from <database>.

If no database id provided the default OM formant database will be used instead."
    (let ((vowel (nth-random (if (symbolp database) (eval database) database))))
      (values (cadr vowel) (car vowel))))


(defmethod! database-formants (vowel-id &optional database (amp 'lin))
    :icon 530
    :initvals '(0 nil lin)
    :numouts 4
    :indoc '("vowel or database element identifier" "formant database" "amplitude scale: 'lin or 'db")
    :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")
    :menuins '((1 (("dB" 'db) ("linear" 'lin) ("initial format" nil))))
    :doc "Returns formant values selected in a database by <vowel-id>.

If <vowel-id> is a number, returns the nth element in the database.
if <vowel-id> is a symbol or a list, return the element associated to this <id>.

Values are formatted as '((frequency1 amplitude1 bandwidth1) (frequency2 amplitude2 bandwidth2) ...) in the first output.
2nd output is the list of frequencies in Hz.
3rd output is the lis of amplitudes in dB or linear (depending on <amp>).
4th output is the list of bandwidths in Hz.

If no database is provided OM default formants database will be used.
"
      (let* ((db (or database (get-default-formants)))
             (formants (clone (cond ((integerp vowel-id) 
                                     (cadr (nth vowel-id db)))
                                    (t (cadr (find vowel-id db :key 'car :test 'equal))))))
             (vals (mat-trans formants))
             (amp-format (amp-format (nth 1 vals))))
      
        (when (and amp-format (not (equal amp-format amp))) 
          (setf (nth 1 vals) (if (equal amp 'lin) 
                                 (db->lin (nth 1 vals)) 
                               (lin->db (nth 1 vals)))))

        (values (mat-trans vals) (first vals) (second vals) (third vals))))


;;;============================================
;;; PROCESS FORMANT LIST (UTILITIES)
;;;============================================

; corrected error in internal variable (ms, 1603)
(defmethod! add-formants (formant-list n delta-fq delta-amp delta-bw)
  :icon 530
    :initvals '(((800 1.0 80) (1150 0.63095737 90) (2800 0.1 120) (3500 0.015848933 130) (4950 0.001 140)) 5 'f 0.0 0.0)
    :numouts 4
    :indoc '("formant list" "number of added formants" "fq-steps" "amp-steps" "bw-steps")
    :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")

  :doc "Adds <n> higher-frequency formants to complete the initial set of formants in <formant-list>.

<formant-list> is given as a list of formant values '((f1 a1 bw1)(f2 a2 bw2)...)

Formants are added respecting a regular increase from the last formant, with a step in frequency, amplitude and bandwidth given by <delta-fq> <delta-amp> <delta-bw>.
If <delta-freq> is a symbol (f or m) the increase in frequency is set automatically respecting male/female voice rules (+1000 Hz for males / +1200 Hz for females).
The frequencies of the new formants are computed by adding to the freq of the last formant delta-fq*i (i=1-n).
The amplitudes of the new formants are computed by adding to the amp of the last formant delta-amp*i (i=1-n). A positive value will increase the amp. Negative values may lead to negative amps.
   If delta-amp is a list, use the values in the list (if too few, repeat the last one).
The bandwidths of the new formants are computed by adding to the bw of the last formant delta-amp*i (i=1-n). A positive value will increase the bw Negative values may lead to negative bws.
   If delta-amp is a list, use the values in the list (if too few, repeat the last one).
"
  (let* ((higher-formant (last-elem formant-list))
         (dfq (if (numberp delta-fq) delta-fq (if (equal delta-fq 'm) 1000.0 1200.0)))
         (newfqs (if (listp delta-fq) (get-vals n delta-fq) (loop for i from 1 to n collect (+ (f-freq higher-formant) (* i dfq)))))
         (newamps (if (listp delta-amp) (get-vals n delta-amp) (loop for i from 1 to n collect (+ (f-amp higher-formant) (* i delta-amp)))))
         (newbws (if (listp delta-bw) (get-vals n delta-bw) (loop for i from 1 to n collect (+ (f-bw higher-formant) (* i delta-bw)))))
         (result (x-append formant-list (mat-trans (list newfqs newamps newbws))))
         (vals (mat-trans result)))
        (values result (first vals) (second vals) (third vals))))

(defun get-vals (nn list)
  (let ((ll (length list)))
    (cond
     ((> ll nn) (first-n list nn))
     ((< ll nn)
      (x-append list (repeat-n (car (last list)) (- nn ll))))
     (t list))))


;(defmethod! add-formants (formant-list n delta-fq delta-amp delta-bw)
;  :icon 530
;  :doc "Adds <n> higher-frequency formants to complete the initial set of formants in <formant-list>.

;<formant-list> is given as a list of formant values '((f1 a1 bw1)(f2 a2 bw2)...)

;"
;  (let* ((higher-formant (last-elem formant-list))
;         (dfq (if (numberp delta-fq) delta-fq (if (equal delta-fq 'm) 1000.0 1200.0)))
;         (newfqs (loop for i from 1 to n collect (+ (f-freq higher-formant) (* i dfq))))
;         (newamps (loop for i from 1 to n collect (+ (f-amp higher-formant) (* i delta-amp))))
;         (newbws (loop for i from 1 to n collect (+ (f-bw higher-formant) (* i delta-bw)))))
;    (x-append formant-list (mat-trans (list newfqs newamps newbws)))))


; added (ms, 1603)
(defmethod! complete-formants (formant-list n delta-fq delta-amp delta-bw)
  :icon 530
  :initvals '(((800 1.0 80) (1150 0.63095737 90) (2800 0.1 120) (3500 0.015848933 130) (4950 0.001 140)) 5 'm 0.0 0.0)
  :numouts 4
  :indoc '("formant list or data base" "total number of needed formants" "fq-steps" "amp-steps" "bw-steps")
  :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")
  :doc "Return a formant or a list of formants of <n> values (freq amp bw). Useful when the same amount of values are needed for interpolations.

<formant-list> can be a single group of formant '((f1 a1 bw1)(f2 a2 bw2)...), a list of groups of formants or a formantic data-base (that is, a list of formants preceded by a symbol).

If needed, formants are added by calling add-formants (see the documentation of that function).
"
  (cond ((numberp (caar formant-list)) ; it is only one formant
         (if (<= n (length formant-list))
             (values-list (cons (firstn formant-list n) (mat-trans (firstn formant-list n))))
           (add-formants formant-list (- n (length formant-list)) delta-fq delta-amp delta-bw)))
         ((listp (caar formant-list)) ; it is a list of formants or the key is a list
          (if (numberp (caaar formant-list)) ; it is a list of formants
              (loop for form in formant-list do
                    collect (if (<= n (length form))
                                (firstn form n)
                              (add-formants form (- n (length form)) delta-fq delta-amp delta-bw)))
            ; the key is a list
            (complete-formants (mapcar 'second formant-list) n delta-fq delta-amp delta-bw)))
         ((symbolp (caar formant-list)) ; it is a data base of formants andthe key is a symbol 
          (complete-formants (mapcar 'second formant-list) n delta-fq delta-amp delta-bw))
         (t (print "Not a valid list of formants or data base"))))


(defmethod! main-formants (formant-list n-trsh)
    :icon 530
    :indoc '("formants list" "threshold or number of formants")
    :initvals '(nil -50)
    :numouts 5
    :outdoc '("formants" "frequencies" "amplitudes" "bandwidths" "rest")
    :doc "Return the main formants from whose <formant-list>.

If <n-trsh> is a positive integer, returns tge <n-trsh> loudest formants.
Else, return the formants with amplitude value > <n-trsh>."
    (let ((result
           (if (and (integerp n-trsh) (>= n-trsh 0))
               (first-n (sort-list formant-list :test '> :key 'cadr) n-trsh)
             (remove n-trsh vals :test '> :key 'cadr))))
      (let ((matres (mat-trans result)))
        (values result (first matres) (second matres) (third matres) (cdddr matres)))))

;;;============================================
;;; EXAMPLE ALTERNATIVE DATABASES
;;;============================================
;;; data-base from the historical 1985 Chant Manual
(defvar *chant-vowels*
; m = male, f = female 
  `((MA ; "Male-A"
     ((609.0 ,(db->lin 0.0) 78.0)
      (1000.0 ,(db->lin -6.1) 88.0)
      (2450.0 ,(db->lin -12.0) 123.0)
      (2700.0 ,(db->lin -11.0) 128.0)
      (3240.0 ,(db->lin -23.8) 138.0)))

    (ME ; "Male-E"
     ((400.0 ,(db->lin 0.0) 64.0)
      (1700.0 ,(db->lin -9.0) 81.0)
      (2300.0 ,(db->lin -8.0) 101.0)
      (2900.0 ,(db->lin -11.0) 119.0)
      (3400.0 ,(db->lin -19.0) 134.0)))

    (MI ; "Male-I"
     ((238.0 ,(db->lin 0.0) 73.0)
      (1741.0 ,(db->lin -19.6) 108.0)
      (2450.0 ,(db->lin -16.5) 123.0)
      (2900.0 ,(db->lin -19.6) 132.0)
      (4000.0 ,(db->lin -31.7) 150.0)))

    (MO ; "Male-O"
     ((325.0 ,(db->lin 0.0) 73.0)
      (700.0 ,(db->lin -11.8) 80.0)
      (2550.0 ,(db->lin -26.0) 125.0)
      (2850.0 ,(db->lin -21.9) 131.0)
      (3100.0 ,(db->lin -27.9) 135.0)))

    (MU ; "Male-U"
     ((360.0 ,(db->lin 0.0) 51.0)
      (750.0 ,(db->lin -11.8) 61.0)
      (2400.0 ,(db->lin -29.4) 168.0)
      (2675.0 ,(db->lin -26.4) 184.0)
      (2950.0 ,(db->lin -35.4) 198.0)))

    (ME1 ; "Male-french-E"
     ((415.0 ,(db->lin 0.0) 45.0)
      (1400.0 ,(db->lin -12.3) 64.0)
      (2200.0 ,(db->lin -15.6) 93.0)
      (2800.0 ,(db->lin -18.4) 114.0)
      (3300.0 ,(db->lin -27.0) 129.0)))

    (MY ; "Male-french-U"
     ((300.0 ,(db->lin 0.0) 66.0)
      (1600.0 ,(db->lin -13.6) 93.0)
      (2150.0 ,(db->lin -12.3) 108.0)
      (2700.0 ,(db->lin -14.9) 122.0)
      (3100.0 ,(db->lin -23.4) 131.0)))

    (ME2 ; "Male-stg"
     ((400.0 ,(db->lin 0.0) 73.0)
      (1050.0 ,(db->lin -12.3) 90.0)
      (2200.0 ,(db->lin -19.1) 118.0)
      (2650.0 ,(db->lin -19.6) 127.0)
      (3100.0 ,(db->lin -28.7) 135.0)))

    (FA ; "Female-A"
     ((650.0 ,(db->lin 0.0) 69.0)
      (1100.0 ,(db->lin -8.3) 95.0)
      (2860.0 ,(db->lin -12.8) 95.0)
      (3300.0 ,(db->lin -11.7) 102.0)
      (4500.0 ,(db->lin -19.2) 120.0)))

    (FE ; "Female-E"
     ((500.0 ,(db->lin 0.0) 75.0)
      (1750.0 ,(db->lin -8.9) 104.0)
      (2450.0 ,(db->lin -10.4) 123.0)
      (3350.0 ,(db->lin -14.5) 140.0)
      (5000.0 ,(db->lin -22.6) 165.0)))

    (FI ; "Female-I"
     ((330.0 ,(db->lin 0.0) 89.0)
      (2000.0 ,(db->lin -14.3) 114.0)
      (2800.0 ,(db->lin -10.9) 132.0)
      (3650.0 ,(db->lin -9.8) 145.0)
      (5000.0 ,(db->lin -18.8) 162.0)))

    (FO ; "Female-O"
     ((400.0 ,(db->lin 0.0) 86.0)
      (840.0 ,(db->lin -12.3) 109.0)
      (2800.0 ,(db->lin -25.7) 130.0)
      (3250.0 ,(db->lin -24.0) 138.0)
      (4500.0 ,(db->lin -30.8) 157.0)))

    (FU ; "Female-U"
     ((280.0 ,(db->lin 0.0) 70.0)
      (650.0 ,(db->lin -18.2) 132.0)
      (2200.0 ,(db->lin -47.7) 156.0)
      (3450.0 ,(db->lin -50.2) 224.0)
      (4500.0 ,(db->lin -51.7) 272.0)))
    ))

(defun get-chant-formants () *chant-vowels*)

;;; extended data-base (different sizes for the formant)
(defvar *long-vowels-lin* 
  '((AA ; "A-circonflex"
    ((0.0 0.100799054 430.461)
     (553.001 1.0 42.96)
     (911.2 0.54817003 97.65)
     (1203.125 0.11403164 171.87)
     (2665.36 0.26844886 160.156)
     (2919.51 0.4975889 66.406)
     (3522.6 0.041091886 214.843)
     (4865.79 0.0260448 246.093)
     (5000.0 0.025104069 406.25)
     (5521.83 0.026753698 175.781)
     (6425.55 0.019172196 292.968)
     (6992.19 0.006031818 257.812)))
   (E ; "E-ai_french"
    ((537.753 1.0 62.5)
     (1405.3363 0.8465936 74.218)
     (2262.9395 0.36730808 132.812)
     (2822.2168 0.3285864 199.219)
     (3072.9876 0.24335467 175.7812)
     (4839.144 0.021821509 285.156)
     (5663.1333 0.0467747 292.969)
     (5843.75 0.04068297 218.75)
     (6662.1655 0.024738085 125.0)
     (7223.4 0.0067808815 160.156)))
   (AN ; "AN nasal"
    ((0.0 0.16094759 499.007)
     (537.45 1.0 58.594)
     (919.5937 0.7931833 46.875)
     (2594.908 0.29951158 82.0312)
     (2963.486 0.27512273 363.281)
     (3132.813 0.25318608 226.56)
     (3523.438 0.093511045 156.25)
     (4644.062 0.01648023 300.781)
     (5286.11 0.02281098 191.406)
     (6442.568 0.013303429 222.6563)
     (6881.676 0.014159813 121.0937)))
   (A ; "A normal"
    ((0.0 0.36945137 145.0034)
     (515.156 0.811402 74.2187)
     (1062.853 1.0 58.5937)
     (2440.085 0.13766971 121.094)
     (2929.377 0.20217513 160.156)
     (3223.246 0.22063592 82.0312)
     (4026.29 0.025668955 218.4231)
     (4912.48 0.046491273 214.8437)
     (5568.642 0.027213287 332.0312)
     (5890.625 0.013881338 406.25)
     (6573.919 0.011455042 214.848)
     (7116.529 0.0048448323 183.594)))
   (EH ; "E sharp_accent"
    ((318.87 1.0 140.625)
     (1561.321 0.55094785 179.687)
     (2226.377 0.3909499 223.211)
     (2919.2097 0.80801195 144.5312)
     (4140.87 0.026290589 202.709)
     (4855.211 0.045640298 148.4375)
     (5733.538 0.048323915 250.0)
     (6538.461 0.047178727 121.0937)
     (6859.375 0.014938439 375.0)
     (7367.187 0.0072577195 289.063)))
   (E1 ; "E closed"
    ((356.9431 1.0 19.53125)
     (1323.1342 0.37429592 19.53125)
     (2127.681 0.29077023 19.53125)
     (2653.059 0.13258514 54.6875)
     (2931.8426 0.036746338 121.09375)
     (3691.658 0.0043028253 269.53)
     (4706.635 0.008415382 128.906)
     (5457.19 0.0047433097 315.172)
     (6268.7 0.014479208 195.312)
     (6723.073 0.0070199473 277.344)
     (7085.937 0.003154038 187.5)))
   (E2 ; "E open"
    ((0.0 0.1029614 384.3206)
     (449.882 1.0 23.4375)
     (1170.537 0.538721 46.875)
     (2184.438 0.27055734 50.7812)
     (2878.4693 0.28619808 105.4687)
     (3601.562 0.012192422 250.0)
     (4734.529 0.012014254 117.1875)
     (5414.68 0.012153524 191.40625)
     (5718.75 0.007926992 406.25)
     (6204.42 0.014224627 175.781)
     (6523.44 0.0036049832 242.1875)
     (7136.2 0.0010409923 230.469)))
   (IN ; "nasal vowel"
    ((0.0 0.22937262 789.0767)
     (583.6507 0.81651575 160.156)
     (1295.6068 1.0 89.8437)
     (2321.163 0.3259579 191.4062)
     (2987.041 0.55778945 152.3437)
     (3492.187 0.03286316 289.0625)
     (5018.52 0.020163158 324.2187)
     (5620.48 0.01715958 621.0937)
     (6070.31 0.009640001 406.25)
     (6602.47 0.0123221055 144.5312)
     (7296.87 0.0016126316 406.25)))
   (I ; "I normal"
    ((243.2858 1.0 46.875)
     (1741.2586 0.27120638 121.094)
     (2582.08 0.44467682 167.97)
     (2740.79 0.47287497 160.16)
     (4562.05 0.01787259 510.73)
     (5556.81 0.03949129 125.0)
     (6366.68 0.03087104 82.031)
     (7101.0 0.004308969 183.594)))
   (ON ; "nasal vowel"
    ((481.886 1.0 234.375)
     (734.375 0.41299802 156.25)
     (2545.191 0.23905036 89.844)
     (2904.558 0.29989865 209.92)
     (3031.106 0.31971496 136.72)
     (3445.31 0.07953065 164.06)
     (4156.3 0.008964389 406.25)
     (5102.29 0.058926184 74.219)
     (5386.42 0.040532563 97.6563)
     (6545.9 0.021659857 183.594)
     (6840.4 0.02371548 191.406)))
   (O1 ; "O closed"
    ((408.12 1.0 82.0312)
     (2594.048 0.32326368 50.7813)
     (2856.6353 0.2967093 113.281)
     (3171.88 0.09264487 296.88)
     (4589.7 0.012743323 156.3)
     (5203.5 0.038904678 74.2)
     (5412.01 0.021848506 121.1)
     (6210.3 0.010017724 101.6)
     (6875.8 0.005194596 222.7)))
   (O2 ; "O open"
    ((487.1717 1.0 70.313)
     (2415.848 0.22475514 58.594)
     (2905.0479 0.1958489 128.91)
     (3507.81 0.013516841 226.563)
     (4967.95 0.028484175 74.219)
     (5206.85 0.020301814 136.719)
     (6036.34 0.006234313 300.78)
     (6855.25 0.0042864783 292.97)))
   (U ; "French OU"
    ((346.375 1.0 70.3125)
     (1037.53 0.10692199 113.281)
     (2567.78 0.266296 82.031)
     (2776.41 0.29332668 121.6802)
     (3074.9 0.35513178 58.5938)
     (3579.28 0.02998779 121.094)
     (5083.2 0.01906091 144.531)
     (5322.98 0.015995929 230.469)
     (6284.2 0.016316614 183.594)
     (6429.69 0.011017563 171.88)
     (7349.7 0.0016675553 222.656)))
   (UN ; "nasal"
    ((0.0 0.27730182 490.704)
     (493.9153 0.92755086 109.375)
     (1088.4008 1.0 113.281)
     (2271.256 0.43676433 121.094)
     (2940.733 0.47967026 246.094)
     (5118.78 0.038360305 289.063)
     (5735.42 0.015589258 339.844)
     (6566.1 0.014020207 187.5)
     (7133.4 0.00354378 199.219)))
   (Y ; "French" 
    ((301.1012 0.812549 62.5)
     (1584.9513 0.5885238 62.5)
     (2088.95 1.0 35.1563)
     (2652.92 0.73988784 39.063)
     (2898.95 0.13698116 105.469)
     (3718.8 0.006024752 406.25)
     (4668.0 0.02792035 70.3125)
     (5458.8 0.112943806 39.0625)
     (6321.3 0.045334492 375.0)
     (6539.1 0.026599689 242.19)
     (7070.3 0.008375232 218.8)))))

(defun get-extended-formants () *long-vowels-lin*)


;;;============================================
;;; FORMANT MORPHING (2D - 3D)
;;;============================================

(defmethod! 2d-morph (fmt1 fmt2 scaler)
  :icon 530
    :initvals '(((800 1.0 80) (1150 0.63095737 90) (2800 0.1 120) (3500 0.015848933 130) (4950 0.001 140))
                ((270 1.0 60) (2140 0.25118864 90) (2950 0.05011873 100) (3900 0.05011873 120) (4950 0.0063095726 120))
                0.5)
    :numouts 4
    :indoc '("formant1" "formant2" "scaler [0-1, clipped]")
    :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")

  :doc "Compute an intermediate formant between two given ones, according to the value of the scaler (0=1st formant, 1=2nd formant).
If the formants do not have the same amount ofr data, the morphing will apply only to the common data.
"
  (let ((scaler (cond ((> scaler 1.0) 1.0) ((< scaler 0.0) 0.0) (t scaler)))
        (result))
    (setf result
          (mapcar #'(lambda (f1 f2)
                      (let ((fq1 (first f1))
                            (fq2 (first f2))
                            (a1 (second f1))
                            (a2 (second f2))
                            (b1 (third f1))
                            (b2 (third f2))
                            (f3) (a3) (b3))
                        (setf f3 (+ (* fq1 (- 1.0 scaler)) (* fq2 scaler)))
                        (setf a3 (+ (* a1 (- 1.0 scaler)) (* a2 scaler)))
                        (setf b3 (+ (* b1 (- 1.0 scaler)) (* b2 scaler)))
                        (list f3 a3 b3)))
                  fmt1 fmt2))
    (values result (first (mat-trans result)) (second (mat-trans result)) (third (mat-trans result)))))


(defmethod! 3d-morph (fmt1 fmt2 fmt3 scaler1 scaler2)
  :icon 530
    :initvals '(((800 1.0 80) (1150 0.63095737 90) (2800 0.1 120) (3500 0.015848933 130) (4950 0.001 140))
                ((270 1.0 60) (2140 0.25118864 90) (2950 0.05011873 100) (3900 0.05011873 120) (4950 0.0063095726 120))
                ((450 1.0 40) (800 0.2818383 80) (2830 0.07943282 100) (3800 0.07943282 120) (4950 0.0031622777 120))
                0.5
                0.5)
    :numouts 4
    :indoc '("formant1" "formant2" "formant3" "scaler1 [0-1, clipped]" "scaler2 [0-1, clipped]")
    :outdoc '("formants values" "frequencies" "amplitudes" "bandwidths")

  :doc "Compute an intermediate formant between three given ones, according to the value of the scalers
First, a 2D-morphing will be computed between the first and second formant, then another 2D-morphing will be applied between the result
of the first morphing and the third formant.
Hence:
0.0 0.0 = first formant (only)
1.0 0.0 = second formant (only)
1.0 1.0 = thrid formant (only)
If the formants do not have the same amount ofr data, the morphing will apply only to the common data.
"
  (let ((scaler1 (cond ((> scaler1 1.0) 1.0) ((< scaler1 0.0) 0.0) (t scaler1)))
        (scaler2 (cond ((> scaler2 1.0) 1.0) ((< scaler2 0.0) 0.0) (t scaler2)))
        (result1) (result2))

    (setf result1 (2d-morph fmt1 fmt2 scaler1))
    (setf result2 (2d-morph result1 fmt3 scaler2))
    (values result2 (first (mat-trans result2)) (second (mat-trans result2)) (third (mat-trans result2)))))

(setf result (2d-morph '((800 1.0 80) (1150 0.63095737 90) (2800 0.1 120) (3500 0.015848933 130) (4950 0.001 140))
                       '((270 1.0 60) (2140 0.25118864 90) (2950 0.05011873 100) (3900 0.05011873 120) (4950 0.0063095726 120))
                       0.0))
