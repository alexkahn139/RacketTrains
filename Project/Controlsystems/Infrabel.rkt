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

  (define (clear-schedule train)
    ((train 'set-schedule!) '()))

  (define (update)
    (hash-for-each (rwm-ls railwaymodel) (lambda (id train)
                                           (drive-train train)
                                           (prepare-tracks train))))

  (define (get-light track)
    (define track-id (track 'get-id))
    (define light #f)
    (hash-for-each
     (rwm-ls railwaymodel)
     (lambda (id loco)
       (define id (loco 'get-id))
       (if (eq? (get-locomotive-location id) track-id)
           (set! light #t)
           'ok))
     )
    light)

  (define (drive-train train)
    ;(display "Train ") (display (train 'get-id)) (displayln "get's driven")
    (define schedule (train 'get-schedule))
    (define (arrived)
      (set-loco-speed! (train 'get-id) 0)
      (clear-schedule train))
    (cond ((null? schedule) (set-loco-speed! (train 'get-id) 0))
          ((eq? ((find-railwaypiece (car (reverse schedule)) (cadr (reverse schedule))) 'get-id) (get-locomotive-location (train 'get-id))) (arrived))
          ;(((get-next-detection-track train) 'occupied?) (set-loco-speed! (train 'get-id) 0))
          (else (set-loco-speed! (train 'get-id) (calculate-train-movement train)))
          ;(else (display (calculate-train-movement train)))
          )
    )

  (define (prepare-tracks train)
    (define last-dt (train 'get-last-dt))
    (define (set-next-parts)	; Altijd als we op een dt komen kunnen we het pad tot daar deleten + de volgende zetten
      (define schedule (train 'get-schedule))
      (define (set-loop rest-of-schedule)
        (when (< 2 (length rest-of-schedule))
          (define testtrack (find-railwaypiece (car schedule) (cdr schedule)))
          (cond ((and testtrack (eq? 'detection-track (testtrack 'type)))
                 (begin
                   ((train 'set-schedule!) rest-of-schedule)
                   ((train 'set-next-dt!) (testtrack 'id)))
                 (set-loop (cdr rest-of-schedule)))
                ((and testtrack (eq? 'switch (testtrack 'type)))
                 (begin
                   (calculate-switch testtrack (car rest-of-schedule) (cdr rest-of-schedule))
                   (set-loop (cdr rest-of-schedule))))                 
                )))
      (set-loop schedule))
    ;(when (eq? (last-dt 'iq) (get-locomotive-location (train 'get-id)))
    (set-next-parts))


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
    (display node1)(display node2)
    (define track (find-railwaypiece node1 node2))
    (display track)
    (define direction 0)
    (if (eq? (track 'get-node1) node1)
        (set! direction +1)
        (set! direction -1))
    direction)

  (define (calculate-train-movement train)
    (define schedule (train 'get-schedule))
    (define loco-speed (train 'get-max-speed))
    (define (loop max-speed schedule)
      (let*
          ((current (current-node schedule))
           ;(display schedule)
           (next (next-node schedule))
           (track (find-railwaypiece current next)))
        (set! max-speed (min max-speed loco-speed (track 'get-max-speed)))
        (cond
          ((< (length (cdr schedule)) 2) max-speed)
          (else (loop max-speed (cdr schedule))))))
    (loop loco-speed schedule))

  (define (dispatch msg)
    (cond
      ((eq? msg 'update) update)
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
