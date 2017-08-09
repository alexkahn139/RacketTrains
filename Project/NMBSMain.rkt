#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Main                  ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "../Project/Controlsystems/NMBS.rkt")
(require "../Project/GUI/GUI.rkt")
(require "../Project/Controlsystems/NMBSnet.rkt")



(define NMBS (make-nmbs))
(define loop #t)
;(define port (random 2000 65535)) (display port)
(define port 29486)
;(define host "192.168.0.160")
(define host "localhost")

(set-up-client port host)

(define (nmbs-loop)
  (sleep 0.02)
  (draw-all NMBS)
  (when loop
    (nmbs-loop)))

(when loop
  (thread nmbs-loop))