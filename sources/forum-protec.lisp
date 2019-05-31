;=========================================================
; FORUM PROTECT: TEST FORUM PROTECT WITH IRCAM SOFTWARE
;=========================================================

(in-package :om)

(defvar *check-chant-forum-protec* nil)

(defun chant-forum-protec (exe-path &optional lib)
  (if *check-chant-forum-protec*
      (if (probe-file exe-path) 
          (let ((wait-time 10)
                (authorized :no))
            (let ((protec-process (mp:process-run-function "TEST FORUM PROTEC" '(:priority 10) 
                                                           #'(lambda (path) 
                                                               (print "Checking Forum protection...")
                                                               (setf authorized (om-cmd-line (format nil "~s" (namestring path)) nil t))
                                                               (print (format nil "=> ~A" authorized))) exe-path))
                  (count 0) (abort nil))
              (when (equal authorized :no)
                (loop while (and (equal authorized :no) (not abort)) do
                      (sleep 0.2)
                      (setf count (+ count 1))
                      (when (> count (/ wait-time 0.2))
                        (om-message-dialog (format nil "The external ~A does not respond or it is not authorized.~%~%In order to authorize IRCAM Forum tools, run the \"ForumProtec\" application available on the ForumNet website."
                                                   (string-upcase (pathname-name exe-path))
                                                   ;(if lib (namestring (om-make-pathname :directory (pathname-directory (lib-pathname lib)))) "library")
                                                   ))
                        (om-kill-process protec-process)
                        #+win32(om-cmd-line (string+ "taskkill /F /IM " (pathname-name exe-path) "." (pathname-type exe-path)) nil t)
                        (setf abort t)
                        )
                      )
                )
              (setf *check-chant-forum-protec*  ;;; this is also the returned value
                    (not (equal authorized :no)))))
        (progn (om-message-dialog (format nil "The external ~A was not found. Please check the installation path in the OM Preferences ('Libraries' tab)."
                                          (namestring exe-path)))
          nil))
    t))


            


