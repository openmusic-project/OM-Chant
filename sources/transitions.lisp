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
    
    #-om7
    (open-param-visualizer visualization-params ch-events trans-function)
                             
    tranformed-list))



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
                       (print (format nil "RULE(S) for ~A:" (string slot)))
                       (loop for r in slotrules do (print r))
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
                                                                  
                                         (simple-bpf-from-list (list 0 dur) (list (car item) (cadr item)) 'bpf 5))
                                       )
                               slotvals))))))
    (setf inter-evt (cons-array inter-evt (list nil (numcols ev1) start dur nil nil) args))
    (set-data inter-evt)
    inter-evt))
                      






#|
;;; OLD CODE FOR HANDLING TRANSITIONS IN THE MAQUETTE
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


