#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Switch ADT                ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "track.rkt")

(provide make-dt)

(define (make-dt id node1 node2 [max-speed 10])
  (define type 'detection-track)
  (define occupied #f)

  (define (occupy! train-id)
    (set! occupied train-id)) ; If not #f, then it's true

  (define (free!)
    (set! occupied #f))
  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-type) type)
      ((eq? msg 'get-node1) node1)
      ((eq? msg 'get-node2) node2)
      ((eq? msg 'occupied?) occupied)
      ((eq? msg 'get-max-speed) max-speed)
      ; setters
      ((eq? msg 'free!) free!)
      ((eq? msg 'occupy!) occupy!)
      (else
       (error "Message not understood"))))
  dispatch)
