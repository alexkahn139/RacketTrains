#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Switch ADT                ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide make-dt)

(define (make-dt id node1 node2 [max-speed 10])

  (define type 'detection-track)

  (define (dispatch msg)
    (cond
      ; getters
      ((eq? msg 'get-type) type)
      ((eq? msg 'get-node1) node1)
      ((eq? msg 'get-node2) node2)
      ((eq? msg 'get-max-speed) max-speed)
      ((eq? msg 'get-id) id)
      ; setters
      (else
       (error "Message not understood - DT " msg))))
  dispatch)
