#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Main                  ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "../Project/Controlsystems/NMBS.rkt")
(require "../Project/GUI/GUI.rkt")
(require "../Project/Controlsystems/NMBSnet.rkt")
(require "../Project/Tests.rkt")


(define NMBS (make-nmbs))
(define loop #t)
(define port 29486) ; Is a port that is almost always free
(define host "localhost")
;(define host "volumio.local") ; Name of my Raspberry Pi, which is also an volumio server

(set-up-client port host)

(define (nmbs-loop) ; The program-loop
  (sleep 0.02)
  (draw-all NMBS)
  (when loop
    (nmbs-loop)))

(when loop
  (thread nmbs-loop))

(define test (make-test NMBS)) ; Initializes the test suite
