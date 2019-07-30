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

;;;==================================
;;; OM6 ONLY (at the moment)
;;; visualization / simulation of transition control
;;;==================================

(in-package :om)

(defun open-param-visualizer (visualization-params ch-events trans-function)
     (when visualization-params
      (if (and (listp visualization-params)
               (symbolp (car visualization-params))
               (slot-boundp (car ch-events) (car visualization-params))
               (numberp (second visualization-params))
               (numberp (third visualization-params)))
          (let ((tc (make-instance 'transition-controller
                                   :trans-patch trans-function
                                   :event-list ch-events
                                   :slot-display (car visualization-params)
                                   :range (cdr visualization-params))))
            (make-editor-window (get-editor-class tc) tc
                                "transition-controller"
                                nil)))
      (om-beep-msg "WRONG VISUALIZATION PARAMS!")
      ))

(defclass! transition-controller () 
  ((trans-patch :accessor trans-patch :initarg :trans-patch :initform nil)
   (event-list :accessor event-list :initarg :event-list :initform nil)
   (slot-display :accessor slot-display :initarg :slot-display :initform nil)
   (range :accessor range :initarg :range :initform nil)))


(defclass transitioneditor (editorview) 
  ((transpanel :accessor transpanel :initform nil)
   (statelist :accessor statelist :initform nil)
   (trans-positions :accessor trans-positions :initform nil)
   (built-list :accessor built-list :initform nil)
   (rest-list :accessor rest-list :initform nil)
   (curr-state :accessor curr-state :initform nil)
   (tmax :accessor tmax :initarg :tmax :initform 0)
   (line-delta :accessor line-delta :initarg :tmax :initform 0)))

(defmethod class-has-editor-p ((self transition-controller)) t)
(defmethod get-editor-class ((self transition-controller)) 'transitioneditor)

(defmethod get-win-ed-size ((self transition-controller)) 
  (om-make-point 400 500))

(defclass transition-panel (om-scroller) ())

(defclass state-panel (om-view) 
  ((state :initarg :state :initform nil :accessor state)))

(defmethod editor ((self transition-panel)) 
  (om-view-container self))
(defmethod editor ((self state-panel)) 
  (editor (om-view-container self)))

(defmethod initialize-instance :after ((self transitioneditor) &rest l)
  (declare (ignore l))
  (init-panels self)
  self)

(defmethod update-editor-after-eval ((self transitioneditor) val)
  (setf (object self) val)
  (apply 'om-remove-subviews (cons self (om-subviews self)))
  (init-panels self)
  (update-subviews self))

(defmethod init-panels ((self transitioneditor))
  (let ((mainpanelcolor *om-gray-color*)
        ;(statepanelcolor (om-make-color 0.19 0.25 0.22))
        (statepanelcolor (om-make-color 0.1 0.12 0.11))
        )
    
    (setf (statelist self) (list (event-list (object self)))
          (trans-positions self) (list '(0 1)))

    (setf (built-list self) (list (car (event-list (object self))))
          (rest-list self) (cdr (event-list (object self)))
          (curr-state self) 0)
    
    (setf (tmax self) (+ (or (list-max (mapcar #'end-time (event-list (object self)))) 0) 0.5))
    
    (om-add-subviews self (setf (transpanel self) 
                                (om-make-view 'transition-panel 
                                              :position (om-make-point 0 0)
                                              :size (om-make-point 100 30)
                                              :scrollbars :v
                                              :bg-color mainpanelcolor)))
    
    (om-add-subviews (transpanel self) (om-make-view 'state-panel
                                                     :bg-color statepanelcolor
                                                     :state 0))))
  
(defmethod new-state ((self transitioneditor))
  (let* ((ev1 (last-elem (built-list self)))
         (ev2 (pop (rest-list self)))
         (newmatrices nil))
    (when (and ev1 ev2)
      (setf newmatrices (apply-transition (trans-patch (object self)) ev1 ev2 (curr-state self) (event-list (object self))))
      (setf (built-list self)
            (append (butlast (built-list self))
                    (list! newmatrices)))
      (setf (curr-state self) (1+ (curr-state self)))
    
      (setf (statelist self) (append (statelist self)
                                     (list (append (built-list self) (rest-list self)))
                                     ))
    
      (setf (trans-positions self) (append (trans-positions self)
                                           (list (list (1- (length (built-list self))) (length (built-list self))))))
    
      (om-add-subviews (transpanel self) (om-make-view 'state-panel
                                                       :bg-color (om-make-color 0.1 0.12 0.11)
                                                       :state (curr-state self)))

      (update-subviews self)
      )))

(defmethod handle-key-event ((self transitioneditor) key)
  (case key
    (:om-key-tab (new-state self))
    (:om-key-up (setf (line-delta self) (- (line-delta self) 10))
     (update-panel self))
    (:om-key-down (setf (line-delta self) (+ (line-delta self) 10))
     (update-panel self))
    (otherwise nil)
    ))


(defmethod update-subviews ((self transitioneditor))
  (call-next-method)
  (let ((space 0))
    (om-set-view-size (transpanel self)
                      (om-make-point (- (w self) space)
                                     ; (h (transpanel self))
                                     (- (h self) space)
                                     )) 
    (update-subviews (transpanel self))
    ))

(defmethod update-subviews ((self transition-panel))
  (when (om-subviews self)
    (let* ((space 4)
           (n (length (om-subviews self)))
           ; (vh (round (- (h self) (* (1+ n) space)) n))
           (vh 120)
           (vw (- (w self) (* space 2)))
           (vx space))
      (om-set-field-size self (om-make-point (w self)
                                            (+ (* n (+ vh space)) space)))
      (loop for sv in (om-subviews self) 
            for i = 0 then (+ i 1) do
            (om-set-view-size sv (om-make-point vw vh))
            (om-set-view-position sv (om-make-point vx (+ (* i (+ vh space)) space)))
            ))
      ))
  
(defmethod om-draw-contents ((self state-panel)) 
  (let ((ev-list (nth (state self) (statelist (editor self)))))
    (om-with-focused-view self
      (om-with-fg-color self *om-light-gray-color*
        (om-with-font *om-default-font1*
                      (om-draw-string 4 12 (format nil "STATE ~D" (state self))))

        (let* ((p1 20) (p2 (+ p1 (line-delta (editor self)))) 
               (h 50) (mt (tmax (editor self)))
               (xratio (/ (w self) mt)))
          (om-with-fg-color self *om-gray-color*
            (loop for i = 1 then (+ i 1) while (< i mt) do
                  (om-draw-line (* i xratio) (h self) (* i xratio) (- ( h self) 4))
                  (om-draw-string (- (* i xratio) 3) (- ( h self) 8) (number-to-string i))
                  ))
          
          (loop for ev in ev-list for i = 0 then (+ i 1) do
                (let ((y (if (oddp i) p1 p2))
                      (x1 (* (action-time ev) xratio))
                      (w (* (event-dur ev) xratio)))
                  
                  (om-with-fg-color self (if (find i (nth (state self) (trans-positions (editor self))))
                                             *om-red2-color* *om-gray-color*)
                    (om-draw-rect x1 y w h))
                  (let ((val (slot-value ev (slot-display (object (editor self)))))
                        (range (range (object (editor self)))))
                    (when val
                      (if (listp val) (setf val (car val)))
                      (if (bpf-p val)
                          (let ((dec-fact (expt 10 (decimals val))))
                            (draw-bpf-in-rect val (* (action-time ev) xratio) (* (end-time ev) xratio) 
                                              y (+ y h) 
                                              (om* (append (list 0 (event-dur ev)) range) dec-fact) ;(print (give-bpf-range val))
                                              ))
                        (let* ((v (if (listp val) (car val) val))
                               (yp (- (+ y h) (* h (/ (- v (car range)) (- (cadr range) (car range)))))))
                          (om-draw-line x1 yp (+ x1 4) yp)
                          (om-draw-line (+ x1 w) yp (+ x1 w -4) yp)
                        
                          (om-draw-string (+ (* (action-time ev) xratio) 10)
                                          (+ y 40)
                                          (format nil "~A" v))
                          )
                        )))
                  )))
        ))))
