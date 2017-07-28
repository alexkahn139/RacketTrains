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
	x1t
	x2t
	y1t
	y2t
	x3t
	y3t
	id-dt
	xID
	yID
	)

(define current-node car)

(define next-node cadr)

(define rest-nodes cdr)

(define x-nodelist car)

(define y-nodelist cadr)

(define id-nodelist caddr)

(define x1t car)

(define x2t caddr)

(define (x3t switch)
	(cadddr (cdr switch)))
(define (xID switch)
	(cadddr (cdddr switch)))

(define y1t cadr)

(define y2t cadddr)
(define (yID switch)
	(cadddr (cddddr switch)))
(define (y3t switch)
	(cadddr (cddr switch)))

(define (id-dt dt)
	(cadddr (cdr dt)))
