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
; Jean Bresson, IRCAM 2010
;===================================================


(in-package :om)

;;;============================================================
;;; COMPAT: OM6 FUNCTIONS IN OM7
;;;============================================================
#+om-sharp
(defun write-sdif-types (file str)
  (sdif-write-types-string file str))

#+om-sharp
(defun write-1nvt-table (thefile namelist vallist)
  (sdif-write-nvt thefile (mapcar #'list namelist vallist)))

#+om-sharp
(defmethod save-sdif ((self sdifframe) fileptr)
  (sdif-write self fileptr))


;;;============================================================
;;; Compatibility function to generate SDIF matrices in OM6/OM7
;;;============================================================
;;; Data is a list of SDIF components 
;;; OM7 requires transposing the list
;;; OM6 requires flattening the list

#+om-sharp
(defun om-make-sdifmatrix (type data)
  (om-init-instance 
   (make-instance 'sdifmatrix :matrixtype type 
                  ;; :elts (length data)
                  :data (mat-trans data))))

#-om-sharp
(defun om-make-sdifmatrix (type data)
  (make-instance 'raw-sdifmatrix  
                 :signature type 
                 :num-elts (length data) 
                 :num-fields (length (car data))
                 :data (flat data)))

;;;============================================================
;;;============================================================
;;;============================================================

(defmethod! chant-patch (modules)
  :initvals '(nil)
  :indoc '("CHANT modules")
  :menuins '((0 (("0  - FOF" 0)
                 ("1  - FOF + NOISE => FILTER" 1)
                 ("2  - NOISE + SOUND => FILTER" 2)
                 ("3  - FOF + NOISE + SOUND => FILTER" 3)
                 ("4  - NOISE => FILTER" 4)
                 ("5  - SOUND => FILTER" 5)
                 ("6  - FOF => FILTER" 6)
                 ("7  - FOF + SOUND => FILTER" 7)
                 ("8  - FOF + [NOISE + SOUND => FILTER]" 8)
                 ("9  - FOF + [NOISE => FILTER]" 9)
                 ("10 - FOF + [SOUND => FILTER]" 10)
                 )))
  :doc "Outputs CHANT patch number depending on the selected module configuration."
  :icon 600
  modules)

(defun numfromsymb (symb)
  (cond 
   ((string-equal symb "FOB") 0)
   ((string-equal symb "REB") 1)
   ((string-equal symb "NOI") 2)))

(defun chant-write-one-IDS (file patch type size beg end)
  (sdif-write-IDS file (numfromsymb type) "Chant" 
                  (format nil "Patch~D/1/~D/1/~D/~F/~F" patch type size beg end)))

(defun chant-write-sdif-NVT (file dur &optional (channels 1) (sr 44100))
  (write-1nvt-table 
   file
   (list "BufferSize" "NumberOfChannels" "EndTime" "SdifTypesVersion" "ChantLibraryVersion" "Author" "SamplingRate")
   (list "512" (format nil "~D" channels) (format nil "~D" dur) "1.01.ppc" "1.01.ppc" "OM" (format nil "~F" sr)))
  )


(defun chant-write-sdif-IDS (file patch &key (fob nil) (reb nil) (noi nil) (snd nil))
            (let ((sizefob (or (car fob) 0))
                  (sizereb (or (car reb) 0))
                  (sizenoi 0)
                  (sndpath (if (car snd) (namestring (car snd)) ""))
                  (bf (or (cadr fob) 0.0)) (ef (or (caddr fob) 0.0))
                  (br (or (cadr reb) 0.0)) (er (or (caddr reb) 0.0))
                  (bn (or (car noi) 0.0)) (en (or (cadr noi) 0.0))
                  (bs (or (cadr snd) 0.0)) (es (or (caddr snd) 0.0)))                 
              (cond
               ((= patch 0) 
                (chant-write-one-IDS file patch "FOB" sizefob bf ef))
               ((or (= patch 2) (= patch 4)) 
                (chant-write-one-IDS file patch "REB" sizereb br er)
                (chant-write-one-IDS file patch "NOI" sizenoi bn en))
               ((or (= patch 6) (= patch 7) (= patch 10)) 
                (chant-write-one-IDS file patch "FOB" sizefob bf ef)
                (chant-write-one-IDS file patch "REB" sizereb br er))
               ((or (= patch 1) (= patch 3) (= patch 8) (= patch 9)) 
                (chant-write-one-IDS file patch "FOB" sizefob bf ef)
                (chant-write-one-IDS file patch "REB" sizereb br er)
                (chant-write-one-IDS file patch "NOI" sizenoi bn en))
               ((= patch 5) 
                (chant-write-one-IDS file patch "REB" sizereb br er)))
              (when (or (= patch 2) (= patch 3) (= patch 5) (= patch 7) (= patch 8) (= patch 10))
                (sdif-write-IDS file 3 "Chant" (format nil "Patch~D/3/SND/1/0/~F/~F/~S"
                                                       patch bs es sndpath)))
              ))

(defun chant-write-sdif-typedefs (file ch-num)
   (when (> ch-num 2)
     (let ((typedef-str "{ 1MTD 1CHA {"))
       (loop for c = 3 then (+ c 1) while (<= c ch-num) do
             (setf typedef-str (string+ typedef-str " Channel" (number-to-string c) ",")))
       (setf typedef-str (string+ typedef-str " } }"))
       (write-sdif-types file typedef-str))))



(defmethod! write-chant-sdif-file ((control-data list) pathname patch dur 
                                   &key (fob nil) (reb nil) (noi nil) (snd nil)
                                   (channels 1) (sr 44100))
   :icon 600
   :indoc '("an SDIF-Buffer" "output file pathname" "CHANT patch number" "duration (sec.)"
                            "FOF properties" "Filter properties" "Noise properties" "Sound properties"
                            "number of channels" "sample rate (Hz)")
   :initvals '(nil nil 0 1.0 nil nil nil nil 1 44100)
   :doc "Generates an SDIF file formatted for the CHANT synthesizer.

The data is written from the SDIFFrame objects in <self>.
Depending on <patch> these SDIFframes must be frames of type 1FOB, 1REB and/or 1NOI.

<pathname> is the output file to write. If <pathname> = NIL, a file chooser dialog allows to determine this file name and location.

<patch> is the CHANT patch to use (use CHANT-PATCH to make the choice).

The SDIF NVT and stream IDS are filled according to the different synthesis parameters.

<dur> determines the total synthesis duration.

<channels> is the number of channels.

<sr> is the sample rate (use default OM sample rate if NIL).

<fob> defines the properties of the FOF generator (Chant patches 0 1 3 6 7 8 9 10)
- these properties are a list (number-of-FOFs begin-time end-time)

<reb> defines the properties of the Filter (Chant patches 1 2 3 4 5 8 9)
- these properties are a list (number-of-formants begin-time end-time)

<noi> defines the properties of the Noise generator (Chant patches 1 2 3 4 8 9)
- these properties are a list (begin-time end-time)

<snd> defines the properties of the sound generator (Chant patches 2 3 5 7 8 10)
- these properties are a list (sound-pathname begin-time end-time)

The output is the written file parthname (connect to an SDIFFILE object or to CHANT-SYNTH).
"
   (let ((outfile (cond ((null pathname) (om-choose-new-file-dialog))
                        ((stringp pathname) (outfile pathname))
                        ((pathnamep pathname) pathname)
                        (t nil))
                        ))
     (when outfile
       (let ((thefile (sdif::sdif-open-file outfile #+om-sharp sdif::eWriteFile #-om-sharp :eWriteFile)))
         (sdif::SdifFWriteGeneralHeader thefile)
         (chant-write-sdif-NVT thefile dur channels sr)
         (chant-write-sdif-typedefs thefile channels)
         (chant-write-sdif-ids thefile patch 
                               :fob fob :reb reb :noi noi 
                               :snd snd)
         (sdif::SdifFWriteAllASCIIChunks thefile)
                               
         (loop for item in control-data do
               (save-sdif item thefile))
         
         (sdif::SDIFFClose thefile))
     outfile)))


; Frequency, Amplitude, Bandwidth, Tex, DebAtt, Atten, FofPhase
(defun complete-fof-data (list)
  (let ((model '(609.0 1.0 77.0 0.003 0.02 0.007 0)))
    (loop for i from 0 to (1- (length model)) collect (or (nth i list) (nth i model)))))

; Frequency, Amplitude, Bandwidth, Saliance, Correction
(defun complete-reb-data (list)
  (let ((model '(609.0 1.0 77.0 1 0)))
    (loop for i from 0 to (1- (length model)) collect (or (nth i list) (nth i model)))))


(defmethod! gen-chant-sdif-frames (f0 fof filt noise &optional (nch 1))
   :icon 600
   :indoc '("F0 data" "FOF data" "Filter data" "Noise data" "number of channels")
   :initvals '(nil nil nil nil)
   :doc "Generates a list of SDIF frames with the CHANT parameters.

<f0> is a list of (time frequency).

<fof> is a list of (time fof-params) where fof-params = ((freq-1 amp-1 bw-1) ... (freq-n amp-n bw-n)) for the n FOF generators (formants).

<filt> is a list of (time filter-params) where filter-params = ((freq-1 amp-1 bw-1) ... (freq-n amp-n bw-n)) for the n filter formants.

<noise> is a list of (time noise-distribution)

The returned value is a list of SDIFFrames suitable to connect to WRITE-CHANT-SDIF-FILE input.
"
   (let ((FOB (loop for item in f0 collect
                    (make-instance 'sdifframe :signature "1FOB" :ftime (car item) :streamid 0 
                                   :lmatrix (list (om-make-sdifmatrix 
                                                   "1FQ0" 
                                                   (list (list (cadr item))))))))
         
         (REB (loop for item in filt collect
                    (make-instance 'sdifframe :signature "1REB" :ftime (car item) :streamid 1 
                                   :lmatrix (list 
                                             (om-make-sdifmatrix 
                                              "1RES" 
                                              (loop for elt in (cadr item) 
                                                    collect (complete-reb-data elt)))
                                             (om-make-sdifmatrix 
                                              "1CHA" 
                                              (make-list (length (cadr item))
                                                         :initial-element (make-list nch :initial-element 1))))
                                   )))
         
         (NOI (loop for item in noise collect 
                    (make-instance 'sdifframe :signature "1NOI" :ftime (car item) :streamid 2 
                                   :lmatrix (list (om-make-sdifmatrix 
                                                   "1DIS" 
                                                   (list (list 0 (cadr item))))))))
         )

     (loop for item in fof do
           (let ((frame-at-t (find (car item) FOB :test '= :key 'ftime))
                 (new-matrix (om-make-sdifmatrix 
                              "1FOF" 
                              (loop for elt in (cadr item) 
                                    collect (complete-fof-data elt))))
                 (chan-matrix (om-make-sdifmatrix 
                               "1CHA" 
                               (make-list (length (cadr item))
                                          :initial-element (make-list nch :initial-element 1)))))
             
             (if frame-at-t
                 (setf (lmatrix frame-at-t) (append (lmatrix frame-at-t) (list new-matrix chan-matrix)))
               (push (make-instance 'sdifframe :signature "1FOB" :ftime (car item) :streamid 0 
                                    :lmatrix (list new-matrix chan-matrix)) FOB))))

     (sort (remove nil (append FOB REB NOI)) '< :key 'ftime)))



;;; Compatibility.
(defmethod! gen-chant-sdif-buffer (f0 fof filt noise &optional (nch 1))
   :icon 600
   :indoc '("F0 data" "FOF data" "Filter data" "Noise data" "number of channels")
   :doc "Generates SDIF frames with CHANT parameters.

DEPRECATED - Use GEN-CHANT-SDIF-FRAMES instead."
   (gen-chant-sdif-frames f0 fof filt noise nch))
   

