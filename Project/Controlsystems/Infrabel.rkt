#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Infrabel ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Simulator/interface.rkt")

(define (make-infrabel)
  (define railwaymodel (load-rwm "../ADT/be_simple.txt"))
  ;; Speed of the train
  (define (get-locomotive-speed id)
      (get-loco-speed id))
  ;; Location of the train, if it is on a detection-block
  (define (get-locomotive-location id)
    (get-loco-detection-block id))
  (define (get-switch-state id)
    (get-switch-position id))
  (define (set-switch-state! id new-pos)
    (set-switch-position! id new-pos))
  (define (get-light track-id)
    (define light 'green)
    (define (look-up trains)
      (for-each (lambda (train)
                  (define id (train 'get-id))
                  (if (eq? (get-locomotive-location id) track-id)
                      (set! light 'red)
                      'ok))
                trains))
      (look-up (rwm-ls railwaymodel))
      light)
  (define (dispatch msg)
    (cond
      ; Getters
      ((eq? msg 'get-locomotive-speed) get-locomotive-speed)
      ((eq? msg 'get-locomotive-location) get-locomotive-location)
      ((eq? msg 'get-switch-state) get-switch-state)
      ((eq? msg 'get-light) get-light)
      ; Setters
      ((eq? msg 'set-switch-state!) set-switch-state!)

      (else (error "Unknown message"))
      ))
  (start-simulator)
  dispatch)
