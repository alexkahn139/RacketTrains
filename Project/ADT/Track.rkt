#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Track ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-track)

(define (make-track [max-speed 10] node1 node2) ; Zo kan er ook een detecion track aangemaakt worden
	 (define type 'track)
    (define occupied #f)

    (define (occupy! train-id)
        (set! occupied train-id)) ;Alles wat niet #f is, is true

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
