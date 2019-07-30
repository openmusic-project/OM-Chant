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
; Jean Bresson, IRCAM 2010
;============================================================================

(in-package :om)

;;;===================================
;;; exported functions :
;;; - ch-transitions
;;; - inter-events
;;;===================================


(defclass! transition-info () 
 ((full-list :accessor full-list :initarg :full-list :initform nil)
  (evt1 :accessor evt1 :initarg :evt1 :initform nil) 
  (evt2 :accessor evt2 :initarg :evt2 :initform nil)
  (i :accessor i :initarg :i :initform nil)
  (beg1 :accessor beg1 :initarg :beg1 :initform nil)
  (end1 :accessor end1 :initarg :end1 :initform nil)
  (dur1 :accessor dur1 :initarg :dur1 :initform nil)
  (beg2 :accessor beg2 :initarg :beg2 :initform nil)
  (end2 :accessor end2 :initarg :end2 :initform nil)
  (dur2 :accessor dur2 :initarg :dur2 :initform nil)
  (dur-inter :accessor dur-inter :initarg :dur-inter :initform nil))
 (:icon 668))

#-om7
(defmethod spec-obj-icon-size ((self transition-info)) '(300 36))

#+om7
(defun arglist (fun) (function-arg-list fun))


(defun apply-transition (fun ev1 ev2 i all-events)
  (if fun
      (cond ((= 1 (length (arglist fun)))
             (let* ((tr-info (make-instance 'transition-info)))
               (setf (full-list tr-info) all-events
                     (i tr-info) i
                     (evt1 tr-info) ev1
                     (evt2 tr-info) ev2
                     (beg1 tr-info) (action-time ev1)
                     (end1 tr-info) (+ (action-time ev1) (event-dur ev1))
                     (beg2 tr-info) (action-time ev2)
                     (end2 tr-info) (+ (action-time ev2) (event-dur ev2))
                     (dur1 tr-info) (- (min (beg2 tr-info) (end1 tr-info)) (beg1 tr-info))
                     (dur2 tr-info) (- (end2 tr-info) (max (beg2 tr-info) (end1 tr-info)))
                     (dur-inter tr-info)  (- (end1 tr-info) (beg2 tr-info)))   ;(max 0 (- (end1 tr-info) (beg2 tr-info))))
               (apply fun (list tr-info))))
            ((= 2 (length (arglist fun)))
             (apply fun (list ev1 ev2)))
            (t (om-beep-msg "ERROR IN TRANSITION FUNCTION ARGUMENTS")))
    (list ev1 ev2)
    ))




(defmethod! ch-transitions ((ch-events list) trans-function &optional visualization-params)
  :icon 624
  :indoc '("list of CHANT event instances (same type)" "function or function list")
  :doc "Generates transitions in a list of CHANT events.

CHANT EVENTS in <ch-events> must be of the same type and will be processes 2 by 2 in the order given by the list.

The the single, free input of the function provided in <trans-function> corresponds to a virtual TRANSITION-INFO box containing information on 2 successive events and relative timing.

It must return either a single, or a list of CHANT event instances which will replace the 2 initial ones in the list. The last one of this list will be used for the next transition.

<trans-function> can also accept a list of functions which will be applied to the successive pairs of events.

<visualization-params> allows to open a transition controler utility displaying the steps of the transition process.
If non-NIL, supply the name of the slot to visualize in ch-event objects and its display range, e.g. '(f0 200 1000), '(amp 0 1), ...
"
  (let ((tranformed-list (list (car ch-events))))
    
    (loop for curr-evt in (cdr ch-events)
          for i = 0 then (+ i 1)
          do (let ((fun (if (listp trans-function) 
                            (nth i trans-function) trans-function)))
               (let* ((prec-evt (last-elem tranformed-list))
                      (newmatrices (apply-transition fun prec-evt curr-evt i ch-events)))
                 (setf tranformed-list
                       (append (butlast tranformed-list)
                               (list! newmatrices))))))
    
    (when visualization-params
      (open-param-visualizer visualization-params ch-events trans-function))
                             
    tranformed-list))






#|
;;; OLD CODE FOR HANDLING TRANSITIONS IN THE MAQUETTE
;;; => See GEN-INTER-EVENTS in Chant-Evts.lisp
;;; (from PhD examples)
(defmethod! transitions ((tmpobjlist list) mkev mkinter modifbeg modifend)
  (let ((sortedlist (sort tmpobjlist '< :key 'offset)))
    (flat (loop for curr-ev in sortedlist
                for prev-ev in (x-append (car sortedlist) sortedlist)
                for next-ev in (x-append (cdr sortedlist) (last-elem sortedlist))
                collect 
                (if 
                          (and (< (offset prev-ev) (offset curr-ev)) ; secu begin
                               (> (+ (offset prev-ev) (* (extend prev-ev) (strech-fact prev-ev))) (offset curr-ev)))
                          ;;; p2 > e1
                          (if (and (< (offset curr-ev) (offset next-ev)) ; secu end
                                   (> (+ (offset curr-ev) (* (extend curr-ev) (strech-fact curr-ev))) (offset next-ev)))
                            ;;; e2 > n1
                            ;;;'double-switch
                            (let ((evt (funcall mkev curr-ev)))
                              (funcall modifend 
                                       (funcall modifbeg evt (+ (offset prev-ev) (* (extend prev-ev) (strech-fact prev-ev))))
                                       (offset next-ev))
                              )
                            (if (and (< (offset curr-ev) (offset next-ev)) ; secu end
                                     (< (+ (offset curr-ev) (* (extend curr-ev) (strech-fact curr-ev))) (offset next-ev)))
                              ;;; e2 < n1
                              ;;;'begin-switch+silence
                              (let ((evt (funcall mkev curr-ev))
                                    (nex (funcall mkev next-ev)))
                                (list 
                                 (funcall modifbeg evt (+ (offset prev-ev) (* (extend prev-ev) (strech-fact prev-ev)))) 
                                 (funcall mkinter evt nex)))
                              ;;;'begin-switch
                              (let ((evt (funcall mkev curr-ev)))
                                (funcall modifbeg evt (+ (offset prev-ev) (* (extend prev-ev) (strech-fact prev-ev))))
                                )
                              )
                            )
                          (if (and (< (offset curr-ev) (offset next-ev)) ; secu end
                                   (> (+ (offset curr-ev) (* (extend curr-ev) (strech-fact curr-ev))) (offset next-ev)))
                            ;;; e2 > n1
                            ;;;'end-switch
                            (let ((evt (funcall mkev curr-ev)))
                              (funcall modifend evt (offset next-ev))
                              )
                            (if (and (< (offset curr-ev) (offset next-ev)) ; secu end
                                     (< (+ (offset curr-ev) (* (extend curr-ev) (strech-fact curr-ev))) (offset next-ev)))
                              ;;; e2 < n1
                              ;;; 'evt+silence
                              (let ((evt (funcall mkev curr-ev))
                                    (nex (funcall mkev next-ev)))
                                (list evt (funcall mkinter evt nex))
                                )
                              ;;; 'evt
                              (let ((evt (funcall mkev curr-ev)))
                                evt)
                              )
                            )
                          )
                ))))

|#


