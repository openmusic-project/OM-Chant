;;; OMChroma
;;; Pulse train generation 
;;; M. Stroppa 2010


(in-package :om)


(defmethod! pulse-train ((freq t) (durtrain t) (durpulse t) (maxamp t) (minamp t) (atk t) (dec t) &optional (decimals 6))
   :initvals '(10.0 1.0 0.5 1.0 0.0 0.1 0.1 6)
   :indoc '("Frequency of the train [Hz or bpf]" "Total duration of the pulse train [sec or bpf]" "Duration of pulse [% of period, 0-1 or bpf]"
                                       "Maximum amplitude [flt or bpf]" "Minimum amplitude [% of maxamp, 0-1 or bpf]"
                                       "Attack [% of pulse dur, 0-1 or bpf]" "Decay [% of pulse dur, 0-1 or bpf]"
                                       "(decimals of bpf [6])")
   :outdoc '("bpf" "x-points" "y-points")
   :icon 633
   :numouts 3
   :doc
"Generate a dynamic pulse train, all the inputs can vary over time (bpf) or be fixed. If incoherent values are given, try to find an acceptable solution.
See single-pulse for the structure of each pulse.
If durtrain is nil and frea is a bpf, take this bpf as absolute time values, otherwise scale it according to durtrain.
All the other bpf's, if given, will be scaled according to durtrain.
"
;MS, 100819

   (when (and (null durtrain) (not (bpf-p freq)))
     (error "You must give a bpf for the frequencies if the duration is nil, Sir!"))

;Test and convert all inputs into bpf's ready for interpolation
   (let* ((durtrain (if (null durtrain) (last-x freq) durtrain))
          (ltimes (if (bpf-p freq) (train freq durtrain) (compute-points freq durtrain)))
          (dltimes (x->dx ltimes))
          (durpulse (compute-input durpulse durtrain))
          (maxamp (compute-input maxamp durtrain))
          (minamp (compute-input minamp durtrain))
          (atk (compute-input atk durtrain))
          (dec (compute-input dec durtrain)))
     (let ((first (let* ((i (pop ltimes)) ; minamp of first pulse must start from 0.0
                         (j (pop dltimes))
                         (curr-atk (x-transfer atk i)))
                    (if (= curr-atk 0.0) ; ONLY for the first pulse, if attack=0, start directly with maxamp
                        (single-pulse-with-time i j (* (x-transfer durpulse i) j) (x-transfer maxamp i) (x-transfer maxamp i)
                                                (x-transfer minamp i) (x-transfer atk i) (x-transfer dec i) (/ 1.0 (expt 10 (1- decimals))))
                      (single-pulse-with-time i j (* (x-transfer durpulse i) j) 0.0 (x-transfer maxamp i)
                                              (x-transfer minamp i) (x-transfer atk i) (x-transfer dec i) (/ 1.0 (expt 10 (1- decimals)))))))
           (bulk ; main loop, except first loop
              (loop for i in ltimes
                    for j in dltimes collect
                    (single-pulse-with-time i j (* (x-transfer durpulse i) j) (x-transfer minamp i) (x-transfer maxamp i)
                                            (x-transfer minamp i) (x-transfer atk i) (x-transfer dec i) (/ 1.0 (expt 10 (1- decimals)))))))

;Prepare the result for a bpf
       (let* ((result (mat-trans (append (list first) bulk)))
              (xvals (prepare4bpf (first result))) ; eliminate common points between end and beg of adjacent pulses
              (yvals (prepare4bpf (second result))))
         (progn (let ((butlast (- (length yvals) 2))
                      (last (- (length yvals) 1)))
                  (setf (nth butlast yvals) 0.0)
                  (setf (nth last yvals) 0.0))
 ; write the last two y-vals at 0.0
         (values (simple-bpf-from-list xvals yvals 'bpf decimals) xvals yvals))))))

;(pulse-train '(10.0 2.0 0.5 1.0 0.1 0.1 0.1))

(defun prepare4bpf (l)
"Eliminate the last point of each list in l, except the last list, and flatten everything, so as to be ready for a bpf"
  (let ((first (butlast l))
        (last (last l)))
    (flat (append (mapcar #'butlast first) last))))


(defun compute-input (val dur)
"If val is a bpf, scale x-vals according to dur, otherwise make a bpf of two constant points"
  (if (bpf-p val)
      (scale-x-bpf val 0.0 dur)
    (simple-bpf-from-list (list 0.0 dur) (list val val) 'bpf 8)))

(defun compute-points (freq dur)
"Return the list of x (time) points of period 1/freq for the duration of dur"
  (let* ((period (/ 1.0 (float freq)))
         (end (+ dur period))) ;make sure you do not end BEFORE dur
    (arithm-ser 0.0 end period)))

(defun last-x (bpf)
"Return the last x-point of a bpf"
  (last-elem (x-points bpf)))

(defun scale-x-bpf (bpf min max)
"Scale the x-points of a bpf between min and max; leave the y-points untouched"
  (simple-bpf-from-list (om-scale (x-points bpf) min max) (y-points bpf)  'bpf (decimals bpf)))


(defmethod* single-pulse ((durtot number) (durpulse number) (a1 number) (a2 number) (a3 number) (atk number) (dec number) &optional (epsilon 0.0001))  
   :initvals '(0.1 0.05 0.0 1.0 0.0 0.3 0.3 0.0001)
   :indoc '("Duration of period [sec]" "Duration of pulse [sec]"
                                       "Initial amplitude [flt]" "Highest amplitude [flt]" "Final amplitude [flt]"
                                       "Attack [0-1]" "Decay [0-1]" "(mininim atk or dec allowed, 0.0001)")
   :icon 314
   :doc
"Generate a single pulse.
Structure: Time=0, Ampl=a1
           Time=atk, Ampl=a2
           Time=durpulse-dec, Ampl=a2
           Time=durpulse, Ampl=a3
           Time=durtot, Ampl=a3
Return the list of times and values.
"
   (let* ((durpulse (if (>= durpulse durtot) (- durtot epsilon) durpulse)) ; correction in case of too long dur
     (atk+dec (compute-atk atk dec durpulse epsilon))
     (atk (first atk+dec))
     (dec (second atk+dec)))
     (if (= atk dec (* durpulse 0.5)) ; if both points coincide, make only a triangle
         (list
          (list 0.0 atk durpulse durtot)
          (list a1 a2 a3 a3))
       (list ; otherwise make a trapezoid
        (list 0.0 atk (- durpulse dec) durpulse durtot)
        (list a1 a2 a2 a3 a3)))))


(defmethod* single-pulse-with-time ((begtime number) (durtot number) (durpulse number) (a1 number) (a2 number) (a3 number) (atk number) (dec number) &optional (epsilon 0.0001))
   :initvals '(0.0 0.1 0.05 0.0 1.0 0.0 0.3 0.3 0.0001)
   :indoc '("Starting Time [sec]" "Duration of period [sec]" "Duration of pulse [sec]"
                                  "Initial amplitude [flt]" "Highest amplitude [flt]" "Final amplitude [flt]"
                                  "Attack [0-1]" "Decay [0-1]"  "(mininim atk or dec allowed, 0.0001)")
   :icon 314
   :doc
"Generate a single pulse.
Structure: Time=begtime, Ampl=a1
           Time=(begtime+atk), Ampl=a2
           Time=(begtime+durpulse-dec), Ampl=a2
           Time=(begtime+durpulse), Ampl=a3
           Time=(begtime+durtot), Ampl=a3
Return the list of times and values.
"

   (let ((pls (single-pulse durtot durpulse a1 a2 a3 atk dec epsilon)))
     (list (om+ begtime (first pls))
           (second pls))))


(defun compute-atk (atk dec durpulse &optional (epsilon 0.0001))
; correct atk and dec if they are bigger than 1.0
; if the value is < epsilon, set it to epsilon to avoid having the same x value (if below the resolution of the bpf)
  (let ((sum (+ atk dec))
        ;; (dureps (- durpulse epsilon))
        )
    (cond
; atk+dec beyond the duration of the pulse, set them to the maximum possible duration respecting the proportions
      ((> sum 1.0) (list (* atk (/ 1.0 sum) durpulse) (* dec (/ 1.0 sum) durpulse)))
; atk and dec too short, set them to epsilon
      ((and (< atk epsilon) (< dec epsilon))
       (list epsilon epsilon))
; atk too short, correct atk
      ((< atk epsilon) (list epsilon (* dec durpulse)))
; dec too short, correct
      ((< dec epsilon) (list (* atk durpulse) epsilon))
; normal case
      (t (list (* atk durpulse) (* dec durpulse))))))


(defmethod* clip-bpf ((bpf bpf) (dur number))
   :initvals '(nil 1.0)
   :indoc '("Absolute bpf [bpf]" "Duration to clip [sec]")
   :icon 314
   :doc
"Generate a new bpf whose duration is clipped to dur."
   (let* ((x (x-points bpf))
          (y (y-points bpf))
          (first-x (first x))
          (second-x (second x))
          (last-x (last-elem x))
          (first-y (first y))
          ;; (second-y (second y))
          )


     (cond
      ((<= dur first-x)
       (error "This bpf starts after or at the end of the given duration. Can do nothing with it, Sir!"))
      ((<= dur second-x)
         (simple-bpf-from-list (list first-x dur) (list first-y (x-transfer bpf dur)) 'bpf (decimals bpf)))
      ((>= dur last-x)
         (simple-bpf-from-list (append x (list dur)) (append y (last y)) 'bpf (decimals bpf)))
      (t
       (let* ((result 
               (loop for xi in x
                     for yi in y
                     while (< xi dur)
                     collect (list xi yi)))
              (xy (mat-trans result))
              (xvals (first xy))
              (yvals (second xy)))
         (simple-bpf-from-list (append xvals (list dur)) (append yvals (list (x-transfer bpf dur))) 'bpf (decimals bpf)))))))






