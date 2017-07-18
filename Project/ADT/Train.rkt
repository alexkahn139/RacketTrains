#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Train ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-train)

(define (make-train id)
  (define max-speed 20)
  (define schedule '())

  (define (set-schedule! schedule-list)
    (set! schedule schedule-list))
  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-id) id)
      ((eq? msg 'get-max-speed) max-speed)
      ((eq? msg 'get-schedule) schedule)

      ; Setters
      ((eq? msg 'set-schedule!) set-schedule!)
      (else
       (error "Message not understood"))))
  dispatch)
