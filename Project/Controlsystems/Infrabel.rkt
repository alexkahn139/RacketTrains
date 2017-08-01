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
                                           ;(prepare-tracks train)
                                           (define next-dt (train 'get-next-dt))
                                           (when (eq? next-dt (get-locomotive-location (train 'get-id)))
                                             (define schedule (train 'get-schedule))
                                             (fix-schedule train next-dt schedule)
                                             (fix-switches schedule)
                                             (find-next-dt train schedule)))))

  (define (get-light track) ; Returns true, when the light is red
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
    (define schedule (train 'get-schedule))
    (define (arrived)
      (set-loco-speed! (train 'get-id) 0)
      (clear-schedule train))
    (when (not (null? schedule))
      (begin
        (let*
            ((next-dt (train 'get-next-dt))
             (det (hash-ref (rwm-dt railwaymodel) next-dt)))
          (cond ((eq? ((find-railwaypiece (car (reverse schedule)) (cadr (reverse schedule))) 'get-id) (get-locomotive-location (train 'get-id))) (arrived))
                ((and (not (eq? next-dt (get-locomotive-location (train 'get-id)))) (get-light det)) (set-loco-speed! (train 'get-id) 0))
                (else (set-loco-speed! (train 'get-id) (calculate-train-movement train)))
                ;(else (display (calculate-train-movement train)))
                )
          ))))

  (define (fix-schedule train next-dt rest-schedule)
    (when (> (length rest-schedule) 2)
      (define testtrack (find-railwaypiece (car rest-schedule) (cdr rest-schedule)))
      (if (and testtrack (eq? 'detectiontrack (testtrack 'get-type)) (eq? next-dt (testtrack 'get-id)))
          ((train 'set-schedule!) (cdr (cdr rest-schedule)))
          (fix-schedule train next-dt (cdr rest-schedule)))))

  (define (find-next-dt train schedule)
    (define (find-loop rst-sched)
      (when (> (length rst-sched) 2)
        (define track (find-railwaypiece (car rst-sched) (cdr rst-sched)))
        (if (and track (eq? 'detectiontrack (track 'get-type)))
            ((train 'set-next-dt!) track)
            (find-loop (cdr rst-sched)))))
    (find-loop schedule))


  (define (fix-switches schedule)
    ;(displayln "fixing switches")
    (define (set-loop rst-sched)
      (when (> (length rst-sched) 2)
        (define track (find-railwaypiece (car rst-sched) (cdr rst-sched)))
        (when (and track (eq? 'switch (track 'get-type)))
          (calculate-switch track (car rst-sched) (cdr rst-sched)))
        (set-loop (cdr rst-sched))))
    (set-loop schedule))

  (define (calculate-switch switch nA nB) ;Enkel switchen mee geven indien nodig verplaatsen
    (displayln "setting switches")
    (define id (switch 'get-id))
    (if (eq? (switch 'get-node1) nA)
        (if (eq? (switch 'get-node2) nB)
            (set-switch-state! id 1)
            (set-switch-state! id 2))
        (if (eq? (switch 'get-node2) nA)
            (set-switch-state! id 1)
            (set-switch-state! id 2))))

  (define (calculate-direction train nodeA nodeB)
    (define track (find-railwaypiece nodeA nodeB))
    (if (eq? (track 'get-node1) nodeA)
        ((train 'set-direction!) -1)
        ((train 'set-direction!) +1))
    )

  (define (calculate-train-movement train)
    (define schedule (train 'get-schedule))
    (define loco-speed (train 'get-max-speed))
    (define (loop max-speed schedule)
      (let*
          ((current (current-node schedule))
           (next (next-node schedule))
           (track (find-railwaypiece current next)))
        (calculate-direction train current next)
        (set! max-speed (min max-speed loco-speed (track 'get-max-speed)))
              (if (< (length (cdr schedule)) 2)
            (* (train 'get-direction) max-speed)
            (loop max-speed (cdr schedule)))))
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
