#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             NMBS ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/GraphRWM.rkt")
;(require "../ADT/RwmToGraph.rkt")
;(require "../Simulator/interface.rkt")
(require "../Abstractions.rkt")
(require "../Controlsystems/NMBSnet.rkt")




(provide make-nmbs)

(define (make-nmbs)

  (define (schedule-destination! train-id destination) ; Need the ID of the train and the destination
    (define train (find-train train-id))
    (define location (location-in-list train-id (get-all 'get-all-loco)))
    (define path (calculate-path location destination))
    (if (eq? location destination)
        (error "Train already on destination")
        (send-path train-id path)))


  (define (dispatch msg)
    (cond
      ((eq? msg 'schedule-destination!) schedule-destination!)
      (else (error "Unkown Message"))))
  dispatch)
