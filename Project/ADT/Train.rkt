#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Train ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-train)

(define (make-train id)
    (define max-speed 20)
    
    (define (dispatch msg)
        (cond
            ; getters
            ((eq? msg 'get-id) id)
            ((eq? msg 'get-max-speed) max-speed)

            (else
                (error "Message not understood"))))
    dispatch)
