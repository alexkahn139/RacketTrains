#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Track ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-track)

(define (make-track node1 node2 [max-speed 10])

  (define type 'track)

  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-type) type)
      ((eq? msg 'get-node1) node1)
      ((eq? msg 'get-node2) node2)
      ((eq? msg 'get-max-speed) max-speed)
      (else
       (error "Message not understood" msg))))
  dispatch)
