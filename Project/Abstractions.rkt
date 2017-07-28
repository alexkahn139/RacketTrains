#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Infrabel ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
	current-node
	next-node
	rest-nodes
	x-nodelist
	y-nodelist
	id-nodelist
	x1
	x2
	y1
	y2
	id-dt
	)

(define current-node car)

(define next-node cadr)

(define rest-nodes cdr)

(define x-nodelist car)

(define y-nodelist cadr)

(define id-nodelist caddr)

(define x1 car)

(define x2 caddr)

(define y1 cadr)

(define y2 cadddr)

(define (id-dt dt)
	(cadddr (cdr dt)))
