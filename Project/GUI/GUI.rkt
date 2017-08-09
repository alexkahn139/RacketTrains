#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/GraphRWM.rkt")
(require "../Abstractions.rkt")
(require "../Controlsystems/NMBSnet.rkt")
;(require "../Simulator/interface.rkt")


(require racket/gui/base)

(provide
 draw-all)

(define nmbs '())
(define train-list '())



(define size 800)
; Load all the bitmaps
(define locomotive  (read-bitmap "GUI/loco.jpeg"))

; Needed for the choice menu's
(define train-choice 1)
(define dt-choice 1)

(define (search-in-list id list)
  (cond ((null? list) (error "Not in list"))
        ((eq? id (caar list)) (cdar list))
        (else (search-in-list id (cdr list)))))

(define (set-color! color)
  (send dc set-pen (send the-pen-list find-or-create-pen color 4 'solid 'round)))

; Make a frame by instantiating the frame% class
(define train-frame (new frame%
                         [label "Racket Trains"]
                         [style '(no-resize-border)]
                         [width size]
                         [height size]))
(define (scale coordinate)
  (/ (* coordinate size) 1000))

; Get all the things that should be drawn (Locomotives, nodes, tracks switches) and put them in a list
(define (get-nodes)
  (define node-list '())
  (hash-for-each
   (rwm-ns railwaymodel)
   (lambda (id node)
     (let*
         ((x (node 'get-x))
          (y (node 'get-y))
          (id (node 'get-id)))
       (set! node-list (cons (list x y (symbol->string id)) node-list)))))
  node-list) ; return the nodes

(define (get-tracks)
  (define track-list '())
  (for-each
   (lambda (track)
     (let*
         ((node1 (hash-ref (rwm-ns railwaymodel) (track 'get-node1)))
          (node2 (hash-ref (rwm-ns railwaymodel) (track 'get-node2)))
          (x1 (node1 'get-x))
          (y1 (node1 'get-y))
          (x2 (node2 'get-x))
          (y2 (node2 'get-y)))
       (set! track-list (cons (list x1 y1 x2 y2) track-list))))
   (all-tracks))
  track-list)

(define (get-dt occupied-list)
  (define dt-list '())
  (for-each
   (lambda (dt)
     (let*
         ((node1 (hash-ref (rwm-ns railwaymodel) (dt 'get-node1)))
          (node2 (hash-ref (rwm-ns railwaymodel) (dt 'get-node2)))
          (x1 (node1 'get-x))
          (y1 (node1 'get-y))
          (x2 (node2 'get-x))
          (y2 (node2 'get-y))
          (id (dt 'get-id))
          (occupied? (search-in-list id occupied-list)))
       (set! dt-list (cons (list x1 y1 x2 y2 (number->string id) occupied?) dt-list))))
   (all-dt))
  dt-list)

(define (get-switches)
  (define switch-list '())
  (for-each
   (lambda (switch)
     (let*
         ((node1 (hash-ref (rwm-ns railwaymodel) (switch 'get-node1)))
          (node2 (hash-ref (rwm-ns railwaymodel) (switch 'get-node2)))
          (node3 (hash-ref (rwm-ns railwaymodel) (switch 'get-node3)))
          (id (hash-ref (rwm-ns railwaymodel) (switch 'get-id)))
          (x1 (node1 'get-x))
          (y1 (node1 'get-y))
          (x2 (node2 'get-x))
          (y2 (node2 'get-y))
          (x3 (node3 'get-x))
          (y3 (node3 'get-y))
          (idx (id 'get-x))
          (idy (id 'get-y)))
       (set! switch-list (cons (list x1 y1 x2 y2 x3 y3 idx idy (switch 'get-id)) switch-list))))
   (all-switches))
  switch-list)

(define (get-locomotives loc-list)
  (define loco-list '())
  (hash-for-each
   (rwm-ls railwaymodel)
   (lambda (id loco)
     (let*
         ((idl (loco 'get-id))
          (det-id (search-in-list id loc-list))
          (dt #f)
          )
       (when det-id
         (set! dt (find-dt det-id)))
       (set! loco-list (cons (list id dt) loco-list)))))
  loco-list)

; Draw functions, they use the list to correctly model the railwaymodel
(define (draw-nodes)
  (define nodes (get-nodes))
  (for-each (lambda (node)
              (define x (scale (x-nodelist node)))
              (define y (scale (y-nodelist node)))
              (define id (id-nodelist node))
              (send dc set-text-foreground "orange")
              (send dc draw-text id (+ 4 x) (+ y 4))
              (set-color! "orange")
              (send dc draw-ellipse x y 4 4)); Elke node moet hier getekend worden
            nodes))

(define (draw-tracks)
  (define tracks (get-tracks))
  (for-each (lambda (track)
              (define x1 (scale (x1t track)))
              (define x2 (scale (x2t track)))
              (define y1 (scale (y1t track)))
              (define y2 (scale (y2t track)))
              (set-color! "black")
              (send dc draw-line x1 y1 x2 y2))
            tracks))

(define (draw-dt occupied-list)
  (define dts (get-dt occupied-list))
  (for-each (lambda (dt)
              (define x1 (scale (x1t dt)))
              (define x2 (scale (x2t dt)))
              (define y1 (scale (y1t dt)))
              (define y2 (scale (y2t dt)))
              (define id (id-dt dt))
              (send dc set-text-foreground "black")
              (send dc draw-text id (+ 4 (/ (+ x1 x2) 2)) (+ (/ (+ y1 y2) 2) 4))
              (if (cadddr (cddr dt))
                  (set-color! "red")
                  (set-color! "green"))
              (send dc draw-line  x1 y1 x2 y2))
            dts))

(define (draw-switches)
  (define switches (get-switches))
  (for-each (lambda (switch)
              (define x1 (scale (x1t switch)))
              (define x2 (scale (x2t switch)))
              (define x3 (scale (x3t switch)))
              (define y1 (scale (y1t switch)))
              (define y2 (scale (y2t switch)))
              (define y3 (scale (y3t switch)))
              (define xid (scale (xID switch)))
              (define yid (scale (yID switch)))
              (define sid (sID switch))
              (send dc set-text-foreground "black")
              ;(send dc draw-text sid (+ 4 xid) (+ yid 4)) ; Is zelfde als de node
              (set-color! "black")
              (send dc draw-line x1 y1 xid yid)
              (send dc draw-line x2 y2 xid yid)
              (send dc draw-line x3 y3 xid yid)
              (send dc set-text-foreground "black")
              ;(send dc draw-text (number->string (get-switch-position sid)) (+ 4 (/ (+ x1 x2) 2)) (+ (/ (+ y1 y2) 2) 4))
              )
            switches))

(define (draw-locos loco-list)
  (define locos (get-locomotives loco-list))
  (for-each (lambda (loco)
              (define id (car loco))
              (when (cadr loco)
                (define node1 (hash-ref (rwm-ns railwaymodel) ((caadr loco) 'get-node1)))
                (define node2 (hash-ref (rwm-ns railwaymodel) ((caadr loco) 'get-node2)))
                (define x1 (scale (node1 'get-x)))
                (define x2 (scale (node2 'get-x)))
                (define y1 (scale (node1 'get-y)))
                (define y2 (scale (node2 'get-y)))
                (send dc draw-bitmap locomotive (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))
            locos))

(define (draw-all NMBS) ; Get's called to draw all the parts
  (define occupied-list (get-all 'get-all-dt))
  (define loco-list (get-all 'get-all-loco))
  (set! nmbs NMBS)
  (send dc erase)
  (draw-switches)
  (draw-dt occupied-list)
  (draw-tracks)
  (draw-nodes)
  (draw-locos loco-list))


; Make a button in the frame
(define draw-panel (new horizontal-panel% [parent train-frame]
                        [min-height 760] [stretchable-height #f]))

(define btn-panel (new horizontal-panel% [parent train-frame]
                       [min-height 40] [stretchable-height #f]))
(define (train-field-list)
  (define loco-list (build-list (hash-count (rwm-ls railwaymodel)) values))
  (set! loco-list (map (lambda (number)
                         (number->string (+ number 1)))
                       loco-list))
  (set! train-choice (string->number (car loco-list)))
  loco-list)
(define train-choice-list (train-field-list))

(define (train-choice-callback choice)
  (set! train-choice choice))

(define trains-field (new choice%
                          (label "Choose train")
                          (parent btn-panel)
                          (choices train-choice-list)
                          (callback (lambda (c e)
                                      (train-choice-callback (+ 1 (send c get-selection))) ; + 1 because the first train is 1
                                      ))))
(define (dt-field-list)
  (define dt-list (build-list (length (all-dt)) values))
  (set! dt-list (map (lambda (number)
                       (number->string (+ number 1)))
                     dt-list))
  ;(set! dt-choice (car dt-list))
  dt-list)

(define dt-choice-list (dt-field-list))

(define (dt-choice-callback choice)
  (set! dt-choice choice))

(define dt-field (new choice%
                      (label "Choose detectiontrack")
                      (parent btn-panel)
                      (choices dt-choice-list)
                      (callback (lambda (c e)
                                  (dt-choice-callback (+ 1 (send c get-selection))) ; + 1 because the first dt is 1
                                  ))))
(new button% [parent btn-panel]
     [label "Drive"]
     ; Callback procedure for a button click:
     (callback (lambda (button event)
                 ((nmbs 'schedule-destination!) train-choice dt-choice)))) ; Hier de geselecteerde trein laten rijden


; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    (super-new)))

; Make a canvas that handles events in the frame
(define canvas (new my-canvas% [parent draw-panel]))
(define dc (send canvas get-dc))


(send train-frame show #t)
