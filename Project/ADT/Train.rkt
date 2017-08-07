#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Train ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Simulator/interface.rkt")


(provide make-train)

(define (make-train id)

  (define max-speed 20)
  (define schedule '())
  (define direction 1)
	(define next-detection-track '())

  (define (set-schedule! schedule-list)
    ;(if (> (length schedule-list) 2)
    ;    (set-next-detection-track! (car schedule-list) (cadr schedule-list))
    ;    (set! next-detection-track '()))
    (set! schedule schedule-list)
    (displayln schedule))

	(define (set-next-detection-track! node1 node2)

		(set! next-detection-track (cons node1 node2))
    (display "set-next-detection-track to: ") (displayln next-detection-track))

  (define (set-direction! dir)
    (set! direction dir))

  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-id) id)
      ((eq? msg 'get-direction) direction)
      ((eq? msg 'get-max-speed) max-speed)
      ((eq? msg 'get-schedule) schedule)
			((eq? msg 'get-next-dt) next-detection-track)
      ; Setters
      ((eq? msg 'set-schedule!) set-schedule!)
			((eq? msg 'set-next-dt!) set-next-detection-track!)
      ((eq? msg 'set-direction!) set-direction!)
      (else
       (error "Message not understood" msg))))
  dispatch)
