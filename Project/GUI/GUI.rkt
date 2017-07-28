#lang racket/gui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../ADT/RailwayModel.rkt")
(require "../Abstractions.rkt")

(require racket/gui/base)
(define railwaymodel (load-rwm "../be_simple.txt"))
(define size 800)
; Load all the bitmaps
(define locomotive  (read-bitmap "loco.bmp"))
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

; Get all the things that should be drawn (Locomotives, nodes, tracks switches)
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
   (rwm-ts railwaymodel))
  track-list)

  (define (get-dt)
    (define dt-list '())
    (hash-for-each
     (rwm-dt railwaymodel)
     (lambda (id dt)
       (let*
         ((node1 (hash-ref (rwm-ns railwaymodel) (dt 'get-node1)))
          (node2 (hash-ref (rwm-ns railwaymodel) (dt 'get-node2)))
          (x1 (node1 'get-x))
          (y1 (node1 'get-y))
          (x2 (node2 'get-x))
          (y2 (node2 'get-y))
          (id (dt 'get-id))
          (occupied? (dt 'occupied?)))
       (set! dt-list (cons (list x1 y1 x2 y2 (symbol->string id) occupied?) dt-list)))))
    dt-list)

; Draw functions
(define (draw-nodes nodes)
  (for-each (lambda (node)
              (define x (scale (x-nodelist node)))
              (define y (scale (y-nodelist node)))
              (define id (id-nodelist node))
              (send dc set-text-foreground "orange")
              (send dc draw-text id (+ 4 x) (+ y 1))
              (set-color! "orange")
              (send dc draw-ellipse x y 4 4)); Elke node moet hier getekend worden
            nodes))

(define (draw-tracks)
  (define tracks (get-tracks))
  (for-each (lambda (track)
              (define x1t (scale (x1 track)))
              (define x2t (scale (x2 track)))
              (define y1t (scale (y1 track)))
              (define y2t (scale (y2 track)))
              (set-color! "black")
              (send dc draw-line x1t y1t x2t y2t))
    tracks))


; Make a static text message in the frame
(define msg (new message% [parent train-frame]
                 [label "No events so far..."]))

; Make a button in the frame
(define draw-panel (new horizontal-panel% [parent train-frame]
                        [min-height 760] [stretchable-height #f]))

(define btn-panel (new horizontal-panel% [parent train-frame]
                       [min-height 40] [stretchable-height #f]))

(define combo-field (new combo-field%
                         (label "Train")
                         (parent btn-panel)
                         (choices (list "Field" "Train"))
                         (init-value "Field")))


(new button% [parent btn-panel]
     [label "Drive"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (send msg set-label "Button click"))]) ; Hier de geselecteerde trein laten rijden


; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))

; Make a canvas that handles events in the frame
(define canvas (new my-canvas% [parent draw-panel]))
(define dc (send canvas get-dc))


(send train-frame show #t)
; Draw all the things
(get-dt)