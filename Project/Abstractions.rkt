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
	)

(define current-node car)

(define next-node cadr)

(define rest-nodes cdr)

(define x-nodelist car)

(define y-nodelist cadr)

(define id-nodelist caddr)
