#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Infrabel-main             ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Project/Controlsystems/Infrabel.rkt")
(require "../Project/Controlsystems/Infranet.rkt")
(require racket/cmdline)

(define sim #f)

(command-line ; Program can only be started from the command-line
	#:once-each
	[("--simulated" "-s" ) "Use the simulator"
		(set! sim #t)])

(define infrabel (make-infrabel sim))
(define loop #t)
(define port 29486)

(set-up-server infrabel port)

(define (infra-loop) ; The program-loop
  (sleep 0.02)
  ((infrabel 'update))
  (infra-loop))

(when loop
  (infra-loop))
