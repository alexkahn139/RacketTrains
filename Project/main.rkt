#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Main                  ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Project/Controlsystems/Infrabel.rkt")
(require "../Project/Controlsystems/NMBS.rkt")
(require "../Project/GUI/GUI.rkt")

(define (make-main)
  (define infrabel (make-infrabel))
  (define NMBS (make-nmbs infrabel))
  (define loop #t)

  (define (prog-loop)
    ((infrabel 'update))
    (draw-all infrabel)
    (sleep 0.1)
    (when loop
      (prog-loop)))

  (define (dispatch msg)
    (cond
      ((eq? msg 'start) prog-loop)
      ((eq? msg 'stop) (set! loop #f))
      ))
  dispatch)
(define main (make-main))
((main 'start))
