;;;===================================================
;;; OM-CHANT
;;; Control CHANT synthesis from OpenMusic
;;;
;;; CHANT Synthesis / Normalization
;;; Jean Bresson, IRCAM 2010
;;;===================================================


(in-package :om)


(defmethod! chant-synth ((self t) &key outfile resolution normalize-level out-format)
    (om-beep-msg "ERROR: Invalid input to CHANT-SYNTH !"))


(defmethod! chant-synth ((self sdiffile) &key outfile resolution normalize-level out-format)
   (chant-synth (filepathname self) :outfile outfile :resolution resolution :normalize-level normalize-level))

(defmethod! chant-synth ((self string) &key outfile resolution normalize-level out-format)
   (if (probe-file (pathname self))
       (chant-synth (pathname self) :outfile outfile :resolution resolution :normalize-level normalize-level)
     (om-beep-msg (string+ "File " self " not found!"))))


(defmethod! chant-synth ((self pathname) &key outfile resolution normalize-level out-format)
  :icon '(410)
  :indoc '("SDIFfile object or file pathname" "output file pathname" "output sample resolution")
  :initvals '(nil nil nil nil nil)
  :doc "Calls CHANT to synthesize a sound from <self>.

<self> is and SDIF file formatted for the control of CHANT (see MAKE-CHANT-SDIF-FILE).

Note : the CHANT path must be specified in the OM preferences.

<outfile> is an output file pathname. If NIL, the name  of <self> will be used with a sound file extension and the output will be written in the OM output files folder.

CHANT output raw data files, which are normalized internally by CHANT-SYNTH.
Some normalization modules may be required (check messages in the OM Listener!)

<resolution> sets the resolution for the raw data -> sound file conversion.

<normalize-level> (if specified) enable normalization of the output 

<out-format> :aiff or :wav.
"

  (let* ((path-sdif self)
         (CHANT-PATH (get-chant-exec-path))
         (format (or out-format *def-snd-format*))
         (outpath (handle-new-file-exists 
                   (corrige-sound-filename (if outfile outfile (string+ (pathname-name self) "." (string-downcase format))) *om-outfiles-folder*)))
         ;(tmppath1 (handle-new-file-exists
         ;           (make-pathname :device (pathname-device outpath) :directory (pathname-directory outpath)
         ;                          :name (pathname-name outpath) :type "tmp1")))
         ;(tmppath2 (handle-new-file-exists
         ;           (make-pathname :device (pathname-device outpath) :directory (pathname-directory outpath)
         ;                         :name (pathname-name outpath) :type "sf")))
         (res (or resolution *audio-res*)))
  (if (not (probe-file CHANT-PATH))
      (om-beep-msg "CHANT not found !!")
    (when (chant-forum-protec CHANT-PATH (find-library "OM-Chant"))
      (if (not (probe-file path-sdif))
          (om-beep-msg "ERROR : bad INPUT file pathname for CHANT-SYNTH !!")
        (let ((nch (find-in-nvtlist (getnvtlist path-sdif) "NumberOfChannels"))
              (sr (find-in-nvtlist (getnvtlist path-sdif) "SamplingRate")))
          (om-print "==========================" "OM-Chant ::")
          (om-print "CHANT SYNTHESIS" "OM-Chant ::")
          (om-print "==========================" "OM-Chant ::")
          (om-print (format nil "Running CHANT with ~D" (namestring path-sdif)) "OM-Chant ::")
          (setf nch (or (and nch (read-from-string nch)) 1)
                sr (or (and sr (read-from-string sr)) *audio-sr*))
          (om-print (format nil "Number of channels= ~D" nch) "OM-Chant ::")
          (om-cmd-line  
           ;(string+ ;(format nil "PATH=$PATH:~s; " (namestring (make-pathname :directory (pathname-directory CHANT-PATH))))
            (format nil "~s -i~s -o~s ~A ~A ~A" 
                    ;#+macosx (string+ "./" (pathname-name CHANT-PATH))
                    ;#+win32 (string+ (pathname-name CHANT-PATH) "." (pathname-type CHANT-PATH))
                    (namestring CHANT-PATH)
                    (namestring path-sdif) 
                    (namestring outpath)
                    (if (or (equal format :aiff) (equal format :wav))
                        (format nil "-s~A~D" (if (equal format :aiff) "a" "w") res)
                      "")
                    (if (and (or (equal format :aiff) (equal format :wav)) normalize-level) 
                        (format nil "-n~D" (- (if (plusp normalize-level) (lin->db normalize-level) normalize-level)))
                      "")
                    (if *sys-console* "-v" "")
                    )
            ;)
           
           *sys-console* 
           T
           ;(om-make-pathname :directory CHANT-PATH)
           )
           
          (if (not (probe-file outpath))
              (om-beep-msg "!! ERROR in CHANT synthesis !!")
            outpath)
          
          ))))))





;=================================================



  









