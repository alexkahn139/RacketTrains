#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Infrabel ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
	current-node
	next-node
	rest-nodes
	)

(define current-node car)

(define next-node cadr)

(define rest-nodes cdr)
