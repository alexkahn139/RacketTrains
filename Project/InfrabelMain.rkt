#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Infrabel-main             ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Project/Controlsystems/Infrabel.rkt")
(require "../Project/Controlsystems/Infranet.rkt")

(define infrabel (make-infrabel))
(define loop #t)
(define port 29486)

(set-up-server infrabel port)

(define (infra-loop)
  (sleep 0.02)
  ((infrabel 'update))
  (infra-loop))

(when loop
  (infra-loop))
