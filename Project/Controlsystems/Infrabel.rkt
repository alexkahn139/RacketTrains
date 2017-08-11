#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Infrabel ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/GraphRWM.rkt")
(require "../Railways/Railway.rkt")
(require "../Abstractions.rkt")

(provide make-infrabel)

(define (make-infrabel sim)

  (define railway (make-railway sim))

  (define locations (make-hash))

  ;; Location of the train, if it is on a detection-block
  (define (get-locomotive-location id) ; Otherwise the location is asked to oftenly
    (hash-set! locations id ((railway 'get-loco-detection-block) id)))

  (define (set-locomotive-speed! id speed)
    ((railway 'set-loco-speed!) id speed))

  (define (get-switch-state id)
    ((railway 'get-switch-position) id))

  (define (set-switch-state! id new-pos)
    ((railway 'set-switch-position!) id new-pos))

  (define (clear-schedule train)
    ((train 'set-schedule!) '()))

  (define (set-new-destination! train-id path)
    (define train (find-train train-id))
    ((train 'set-schedule!) path)
    ((train 'set-next-dt!) (car path) (cadr path)))

  (define (update) ; Update function. Moves the train if needed, and sets the switches correctly
    (hash-for-each (rwm-ls railwaymodel) (lambda (id train)
                                           (define next-dt (train 'get-next-dt))
                                           ; (when (pair? next-dt)
                                           ;   (define track  (find-railwaypiece (car next-dt) (cdr next-dt)))
                                           (get-locomotive-location (train 'get-id))
                                           (when (hash-ref locations (train 'get-id))
                                             (define schedule (train 'get-schedule))
                                             (fix-schedule train next-dt schedule)
                                             (fix-switches train schedule)
                                             ;(find-next-dt train schedule)
                                             )
                                           (drive-train train))))

    (define (get-light track) ; Returns true, when the light is red
      (define track-id (track 'get-id))
      (define light #f)
      (hash-for-each
       (rwm-ls railwaymodel)
       (lambda (id loco)
         (define id (loco 'get-id))
         (if (eq? (hash-ref locations (loco 'get-id)) track-id)
             (set! light #t)
             'ok)))
      light)

    (define (drive-train train) ; Makes the train move if needed, by calculating the max allowed speed
      (define schedule (train 'get-schedule))
      (define (arrived)
        (set-locomotive-speed! (train 'get-id) 0)
        (clear-schedule train))
      (if (> 2 (length schedule))
          (set-locomotive-speed! (train 'get-id) 0)
          (begin
            (let*
                ((next-dt (train 'get-next-dt)) ; next-dt should be a list with the two nodes
                 (det (find-railwaypiece (car next-dt) (cdr next-dt)))
                 (last-dt (find-railwaypiece (car (reverse schedule)) (cadr (reverse schedule)))))
              (cond ((eq? (last-dt 'get-id) (hash-ref locations  (train 'get-id))) (arrived)) ; If the train is on the final block it should come to a stop and the schedule should be deleted
                    ((eq? (hash-ref locations  (train 'get-id)) (det 'get-id)) (find-next-dt train (cdr schedule)))
                    ((get-light det) (set-locomotive-speed! (train 'get-id) 0)); If there is another train, the train stops
                    (else (set-locomotive-speed! (train 'get-id) (calculate-train-movement train))))))))

    (define (fix-schedule train next-dt rest-schedule) ; Function to delete te part of the schedule that's already driven
      (when (> (length rest-schedule) 2)
        (define testtrack (find-railwaypiece (car rest-schedule) (cadr rest-schedule)))
        (if (and testtrack (eq? 'detection-track (testtrack 'get-type)) )
            (begin ((train 'set-schedule!) (cdr (cdr rest-schedule)))
                   (find-next-dt train (cdr (cdr rest-schedule))))
            (fix-schedule train next-dt (cdr rest-schedule)))))

    (define (find-next-dt train schedule)
      (define (find-loop rst-sched)
        (when (>= (length rst-sched) 2)
          (define node1 (car rst-sched))
          (define node2 (cadr rst-sched))
          (define track (find-railwaypiece (car rst-sched) (cadr rst-sched)))
          (if (and track (eq? 'detection-track (track 'get-type)))
              ((train 'set-next-dt!) node1 node2)
              (find-loop (cdr rst-sched)))))
      (find-loop schedule))


    (define (fix-switches train schedule) ; Set's the switches correctly
      (define (set-loop rst-sched)
        (when (and (> (length rst-sched) 2)
                   (not (and (eq? (car(train 'get-next-dt)) (car rst-sched))
                             (eq? (cdr (train 'get-next-dt)) (cadr rst-sched)))))
          (define track (find-railwaypiece (car rst-sched) (cadr rst-sched)))
          (define pos-track (find-railwaypiece (cadr rst-sched) (caddr rst-sched)))
          (when (and track (eq? 'switch (track 'get-type)))
            (calculate-switch track (car rst-sched) (cadr rst-sched)))
          ;(when (and track pos-track (eq? 'switch (track 'get-type)) (eq? 'switch (pos-track 'get-type))) ; Needed for when there is a double switch, otherwise the ID never gets found
          ;  (let* ((nodeA (track 'get-id))
          ;         (nodeB (pos-track 'get-id))
          ;         (new-track (find-railwaypiece nodeA nodeB)))
          ;    ;(when new-track
          ;    (calculate-switch new-track nodeA nodeB)))
          ;;)
          (set-loop (cdr rst-sched))))
      (set-loop schedule))

    (define (calculate-switch switch nA nB)
      (display "Calculating switch: ") (displayln (switch 'get-id))
      (define id (switch 'get-id))
      (define n1 (switch 'get-node1))
      (define n2 (switch 'get-node2))
      (define n3 (switch 'get-node3))
      (cond ((and (eqv? n1 nA) (eqv? n2 nB))
             (set-switch-state! id 1)) ; Then nB is equal to n3
            ((and (eqv? n1 nA) (eqv? n3 nB))
             (set-switch-state! id 2))
            ((and (eqv? n1 nB) (eqv? n2 nA))
             (set-switch-state! id 1))
            ((and (eqv? n1 nB) (eqv? n3 nA))
             (set-switch-state! id 2))))

    (define (calculate-direction train nodeA nodeB)
      (define track (find-railwaypiece (car (train 'get-next-dt)) (cdr (train 'get-next-dt))))
      (if (eq? (track 'get-node1) (car (train 'get-next-dt)))
          ((train 'set-direction!) +1)
          ((train 'set-direction!) -1)))

    (define (calculate-train-movement train)
      (define schedule (train 'get-schedule))
      (define loco-speed (train 'get-max-speed))
      (calculate-direction train (car schedule) (cadr schedule))
      (define (loop max-speed schedule)
        (let*
            ((current (current-node schedule))
             (next (next-node schedule))
             (track (find-railwaypiece current next)))
          (set! max-speed (min max-speed loco-speed))
          (when track
            (track 'get-max-speed))
          (if (< (length (cdr schedule)) 2)
              (* (train 'get-direction) max-speed)
              (loop max-speed (cdr schedule)))))
      (loop loco-speed schedule))

    (define (get-all-dt) ; On the n-1 location in the list you get the occupancy
      (define occupied '())
      (for-each
       (lambda (dt)
         (set! occupied (cons (cons (dt 'get-id) (get-light dt)) occupied))) (all-dt))
      (reverse occupied))

    (define (get-all-loco) ; On the n-1 location in the list you find the location or #f
      (define loco-list '())
      (hash-for-each (rwm-ls railwaymodel)
                     (lambda (id ls)
                       (set! loco-list (cons (cons id (hash-ref locations  (ls 'get-id))) loco-list))))
      (reverse loco-list))

    (define (dispatch msg)
      (cond
        ((eq? msg 'update) update)
        ; Getters
        ((eq? msg 'get-locomotive-location) get-locomotive-location)
        ((eq? msg 'get-switch-state) get-switch-state)
        ((eq? msg 'get-light) get-light)
        ((eq? msg 'get-all-dt) get-all-dt)
        ((eq? msg 'get-all-loco) get-all-loco)
        ; Setters
        ((eq? msg 'set-switch-state!) set-switch-state!)
        ((eq? msg 'set-new-destination!) set-new-destination!)
        (else (error "Unknown message"))
        ))

    dispatch)
  