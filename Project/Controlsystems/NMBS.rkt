#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             NMBS ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Simulator/interface.rkt")
(require "../Abstractions.rkt")

(provide make-nmbs)

(define (make-nmbs infrabel)

  (define (print-status)
    (hash-for-each (rwm-dt rwm)
                   (lambda (detection-track)
                     (display "Detection track: ")
                     (display (detection-track 'get-id))
                     (display ": Is free ")
                     (display (detection-track 'free))))
    (for-each
     (lambda (track)
       (display "Track: ")
       (display (track 'get-node1))
       (display " ")
       (display (track 'get-node2))
       (display ": Is free ")
       (display (track 'free?))
       (newline))
     (rwm-ts)))

  (define (schedule-destination! train-id destination) ; Need the ID of the train and the destination
    (define train (find-train train-id))
    (define location (infrabel 'get-locomotive-location train-id))
    (define path 'a) ; Met de grafalgorithmen hier de korste weg berekenen
    ((train 'set-schedule!) path))


  (define (dispatch msg)
    (cond
      ((eq? msg 'print-status) print-status)
      (else (error "Unkown Message"))))
  dispatch)
