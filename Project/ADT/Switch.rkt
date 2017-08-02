#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Switch ADT                ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-switch)

(define (make-switch id node1 node2 node3 [max-speed 10])

  (define type 'switch)

  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-type) type)
      ((eq? msg 'get-id) id)
      ((eq? msg 'get-node1) node1)
      ((eq? msg 'get-node2) node2)
      ((eq? msg 'get-node3) node3)
      ((eq? msg 'get-max-speed) max-speed)
      (else
       (error "Message not understood" msg))))
  dispatch)
