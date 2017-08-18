#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Main                  ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These tests work on NMBSMain
; They can be used to test if the system is working correctly
; The user can observe in the GUI but should't click on any button during the test!!!

(require "../Project/GUI/GUI.rkt")

(provide make-test)

(define (make-test NMBS)

	(define (wait-loop train-id nxt-train-id destination next-destination)
		(if (eq? destination (get-loco-location train-id))
				((NMBS 'schedule-destination!) nxt-train-id next-destination)
				(wait-loop train-id nxt-train-id destination next-destination)))

	(define (one-train) ; This is a first test where one train should be driving, this one is just to test that the paths are calculated correctly and the switches are set accordingly
		((NMBS 'schedule-destination!) 1 1)
		(wait-loop 1 1 1 11)
		(wait-loop 1 1 11 8)
		(wait-loop 1 1 8 12)
		(wait-loop 1 1 12 5))

	(define (two-trains) ; This is a test to show that two trains will not collide
		((NMBS 'schedule-destination!) 1 2)
		(wait-loop 1 2 2 5)
		((NMBS 'schedule-destination!) 1 4))


	(define (dispatch msg)
		(cond
			((eq? msg 'one-train) one-train)
			((eq? msg 'two-trains) two-trains)
			(else error "Message could not be understood - test " msg)))

	dispatch)
