#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Infrabel ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Simulator/interface.rkt")
(require "../Abstractions.rkt")

(provide make-infrabel)

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
  (define (get-light track)
    (define track-id (track 'get-id))
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
        (set! direction -1))
    direction)

  (define (calculate-train-movement train) ;Makes the train move, and the switches be set correctly
    (define train-id (train 'get-id))
    (define schedule (train 'get-schedule))
    (define location (get-loco-detection-block train-id))
    (define current (current-node schedule))
    (define next (next-node schedule))
    (define nextnext (next-node (next-node schedule)))
    (define max-speed ((find-railwaypiece current next) 'get-max-speed))
    (define track (find-railwaypiece current next))

    (define (move)
      (define (switch-setter) ; Switchen moeten op tijd klaar gezet worden
        (when (eq? 'switch (track 'get-type))
          (calculate-switch track next nextnext)))

      (define (next-max-speed schedule)  ; The speed has to be calculated
        (define next-track (find-railwaypiece (next-node schedule) (next-node (cdr schedule))))
        (cond
          ((null? schedule) max-speed) ; Train can stop here
          ((eq? 'detection-track (next-track 'get-type)) (set! max-speed (min max-speed (next-track 'get-max-speed)))) ; If on dt, it should not go further until you know the lights of the next
          (next-track (set! max-speed (min max-speed (next-track 'get-max-speed))) (next-max-speed (cdr schedule))) ; If on normal track, it can go further
          ))
      (switch-setter)
      (next-max-speed))
    (cond
      ((>= 2 (length schedule)) 0) ; train should come to a stop
      ((eq? 'red (get-next-detection-track train)) 0)
      (else (* (calculate-direction current next) (min (train 'get-max-speed) (move))))))


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
(define infrabel (make-infrabel))
