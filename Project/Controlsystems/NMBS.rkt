#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             NMBS ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../ADT/RwmToGraph.rkt")
(require "../Simulator/interface.rkt")
(require "../Abstractions.rkt")


(provide make-nmbs)

(define (make-nmbs infrabel)

  (define planner (make-rwm-to-graph))

  (define (print-status occupied-list) ; Not really needed, the GUI shows it all
    (hash-for-each (rwm-dt railwaymodel)
                   (lambda (id detection-track)
                     (display "Detection track: ")
                     (display (detection-track 'get-id))
                     (display ": Is free ")
                     (display (list-ref occupied-list (- id 1)))
                     (newline)))
    (for-each
     (lambda (track)
       (display "Track: ")
       (display (track 'get-node1))
       (display " ")
       (display (track 'get-node2))
       (display ": Is free ")
       (display (track 'free?))
       (newline))
     (rwm-ts railwaymodel)))

  (define (schedule-destination! train-id destination) ; Need the ID of the train and the destination
    (define train (find-train train-id))
    (define location ((infrabel 'get-locomotive-location) train-id))
    (define path ((planner 'calculate-path) location destination)) ; Met de grafalgorithmen hier de korste weg berekenen
    ((train 'set-schedule!) path)
		(displayln path))


  (define (dispatch msg)
    (cond
      ((eq? msg 'print-status) print-status)
      ((eq? msg 'schedule-destination!) schedule-destination!)
      (else (error "Unkown Message"))))
  dispatch)
