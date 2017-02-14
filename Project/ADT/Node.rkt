#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Switch ADT                ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-node)

(define (make-node id x y)
    (define (dispatch msg)
        (cond
            ; getters
            ((eq? msg 'get-id) id)
            ((eq? msg 'get-x) x)
            ((eq? msg 'get-y) y)

            (else
                (error "Message not understood"))))
    dispatch)
