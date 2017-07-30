#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Main                  ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Project/Controlsystems/Infrabel.rkt")
(require "../Project/Controlsystems/NMBS.rkt")
(require "../Project/GUI/GUI.rkt")
(require "../Project/ADT/RailwayModel.rkt")


  (define infrabel (make-infrabel))
  (define NMBS (make-nmbs infrabel))
  (define loop #t)

  (define (main)
    ((infrabel 'update))
    (draw-all infrabel)
    (sleep 0.02)
    (when loop
      (main)))

(when loop
  (thread main))
;((NMBS 'schedule-destination!) 1 2)
;Nm
;Mx
;A2