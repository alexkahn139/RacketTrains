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

  (define (get-next-detection-track train)
    (define first-node '())
    (define second-node '())
    (define detection-track '())
    (define schedule ((train 'get-schedule))) ;; Schedule exists of the Required Nodes
    (if (not (null? schedule))
        (begin
          (set! first-node (car schedule))
          (set! second-node (cadr schedule))
          (set! detection-track (get-dt first-node second-node))
          (if (detection-track)
              (begin ((detection-track 'get-id)) ((train 'set-schedule) (cddr schedule)))
              (begin ((train 'set-schedule) cdr schedule) (get-next-detection-track train)
                     ))
          detection-track)
        #f))
  (define (calculate-switch switch nA nB) ;Enkel switchen mee geven indien nodig verplaatsen
    (define id (switch 'get-id))
    (if (eq? (switch 'get-node1) nA)
        (if (eq? (switch 'get-node2) nB)
            (set-switch-state! id 1)
            (set-switch-state! id 2))
        (if (eq? (switch 'get-node2) nA)
            (set-switch-state! id 1)
            (set-switch-state! id 2))))
  (define (calculate-direction node1 node2)
    (define track (get-track node1 node2))
    (define direction 0)
    (if (eq? ((track 'get-node1)) node1)
        (set! direction +1)
        (set! direction -1)))

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
