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
;;;===================================================
;;; LIBRARY MAIN FILE
;;; Jean Bresson, IRCAM 2010
;;;===================================================


(in-package :om)

(defparameter *chant-files* '("om6-chant-prefs"
                              "chant-sdif" 
                              "chant-synth"
                              "formants"
                              "chant-auto-rules"
			      "impulse"
			      "chant-evts-om6"
			      "transitions"
			      "transition-viewer"
			      "morphing"
                              "chant-maquette"
                              ))

(mapc #'(lambda (file) (compile&load (om-relative-path '("sources") file))) *chant-files*)



(fill-library '(("Synthesize" nil nil (chant-synth synthesize) nil)
                ("SDIF" nil nil (chant-patch write-chant-sdif-file gen-chant-sdif-frames) nil)
                ("Formants" nil nil (vowel-formants database-formants get-database-ids random-vowel add-formants complete-formants main-formants))
                ("Chant Control Rules" nil nil (autoamp fcomp autobw cslope autobend autotex spcor) nil)
                ("Chant Events" 
                 (("Maquette" nil nil (maq-fof maq-fof-transition maq-fof-morph maq-f0 maq-f0-vib maq-f0-jit maq-f0-bpf maq-f0-transition) nil))
                 (ch-f0 ch-noise ch-snd ch-fof ch-flt ch-channels phoneme) 
                 (ch-transitions gen-chant-channels gen-inter-event gen-inter-fofs fof-morph))
                ))


(doc-library "OM-CHANT is a library for the control of CHANT synthesis from OpenMusic.
<br><br>
The CHANT synthesizer (X. Rodet, Y. Potard, IRCAM - 1984) is set up according to one of 11 available patches, corresponding to different configurations of a FOF generator, a NOISE generator, a SOUND file player and a resonant FILTER.
<br><br>
Each of these modules is controlled by a continuous sequence of states. The synthesizer automatically interpolates between two successive states specified for each modules.
The CHANT synthesizer can be controlled by a file in the SDIF format. 
<br><br>
OM-CHANT provides tools for creating this control file and for calling the CHANT synthesizer in OM visual programs.
Some automatic calculation andc orrection rules adapted from previous Chant implementations are also available.
<br><br>
OM-CHANT also provides higher-level control structures under the form of matrices, representing 'CHANT events'.
<br><br>
See the <a href=\"../OM-Chant-2.0-UserManual.pdf\">OM-Chant User Manual</a>
" 
             (find-library "OM-Chant"))

; (gen-lib-reference (find-library "OM-Chant"))


(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(om::set-lib-release 3.0)


(om::om-print "
=======================================
 OM-CHANT 3.0 (c) IRCAM 2010-2019
 Control of CHANT in OpenMusic
=======================================
")


;;;======================
;;; RELEASE NOTES
;;;======================
;;; 1.1
;;; - formant-filters returned as symbols ex. '(soprano o) instead of strings
;;; - chant-synth reformatted with keywords
;;; - chant-synth new input : normalize-level
;;;
;;; 1.2
;;; - New CHANT release compatibility (fixes SDIF dependencies and fromsf calls)
;;;
;;; 2.0 
;;; - Merge with the CHANT-EVENTS architecture
;;; - Morphing and transition tools (R. Foulon)
;;; - New Chant standalone (removed dependency to SuperVP and tosf)
;;; 
;;; 2.2 
;;; - fix in complete-formants function
;;; 2.3 
;;; - fix audio format issues
;;; 3.0
;;; - Compatibility OM7
;;; - Chant synth included
