#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Main                  ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Project/Controlsystems/Infrabel.rkt")
(require "../Project/Controlsystems/NMBS.rkt")
(require "../Project/GUI/GUI.rkt")
(require "../Project/ADT/RailwayModel.rkt")
(require "../Project/Controlsystems/Infranet.rkt")
(require "../Project/Controlsystems/NMBSnet.rkt")


(define infrabel (make-infrabel))
(define NMBS (make-nmbs))
(define loop #t)
;(define port (random 2000 65535)) (display port)
(define port 29486)

(set-up-server infrabel port)
(set-up-listener port)

(define (main)
  ((infrabel 'update))
  (sleep 0.02)
  (draw-all NMBS)
  (when loop
    (main)))

(when loop
  (thread main))