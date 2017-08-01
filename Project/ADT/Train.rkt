#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Train ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-train)

(define (make-train id)
  (define max-speed 20)
  (define schedule '())
	(define last-detection-track '())

  (define (set-schedule! schedule-list)
    (set! schedule schedule-list))

	(define (set-last-detection-track! dt-id)
		(set! last-detection-track dt-id))

  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-id) id)
      ((eq? msg 'get-max-speed) max-speed)
      ((eq? msg 'get-schedule) schedule)
			((eq? msg 'get-last-dt) last-detection-track)
      ; Setters
      ((eq? msg 'set-schedule!) set-schedule!)
			((eq? msg 'set-last-dt!) set-last-detection-track!)
      (else
       (error "Message not understood" msg))))
  dispatch)
