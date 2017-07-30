#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Abstractions.rkt")
(require "../Controlsystems/Infrabel.rkt")

(require racket/gui/base)

(provide
 draw-all)

(define railwaymodel (load-rwm "be_simple.txt"))
(define infra '())
(define train-list '())
;(define infrabel (make-infrabel))

(define size 800)
; Load all the bitmaps
(define locomotive  (read-bitmap "GUI/loco.jpeg"))
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

(define (get-dt infrabel)
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
          (occupied? ((infrabel 'get-light) dt)))
       (set! dt-list (cons (list x1 y1 x2 y2 (number->string id) occupied?) dt-list)))))
  dt-list)

(define (get-switches)
  (define switch-list '())
  (hash-for-each
   (rwm-ss railwaymodel)
   (lambda (id switch)
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
       (set! switch-list (cons (list x1 y1 x2 y2 x3 y3 idx idy (symbol->string (switch 'get-id))) switch-list)))))
  switch-list)

(define (get-locomotives infrabel)
  (define loco-list '())
  (hash-for-each
   (rwm-ls railwaymodel)
   (lambda (id loco)
     (let*
         ((idl (loco 'get-id))
          (det-id ((infrabel 'get-locomotive-location) idl))
          (dt #f)
          )
       (when det-id
         (set! dt (hash-ref (rwm-dt railwaymodel) det-id)))
       (set! loco-list (cons (list id dt) loco-list)))))
  loco-list)

; Draw functions
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

(define (draw-dt infrabel)
  (define dts (get-dt infrabel))
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
              ; (send dc set-text-foreground "black") (send dc draw-text sid (+ 4 xid) (+ yid 4)) ; Is zelfde als de node
              (set-color! "black")
              (send dc draw-line x1 y1 xid yid)
              (send dc draw-line x2 y2 xid yid)
              (send dc draw-line x3 y3 xid yid)
              )
            switches))
(define (draw-locos infrabel)
  (define locos (get-locomotives infrabel))
  (for-each (lambda (loco)
              (define id (car loco))
              (when (cadr loco)
                (define node1 (hash-ref (rwm-ns railwaymodel) ((cadr loco) 'get-node1)))
                (define node2 (hash-ref (rwm-ns railwaymodel) ((cadr loco) 'get-node2)))
                (define x1 (scale (node1 'get-x)))
                (define x2 (scale (node2 'get-x)))
                (define y1 (scale (node1 'get-y)))
                (define y2 (scale (node2 'get-y)))
                (send dc draw-bitmap locomotive (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))
              (when (not (cadr loco))
                (send msg set-label "Not all locs on dt")))
            locos))
(define (draw-all infrabel)
  (set! infra infrabel)
  (send dc clear)
  (draw-switches)
  (draw-dt infrabel)
  (draw-tracks)
  (draw-nodes)
  (draw-locos infrabel)
  )
; Make a static text message in the frame
(define msg (new message% [parent train-frame]
                 [label "No events so far..."]))

; Make a button in the frame
(define draw-panel (new horizontal-panel% [parent train-frame]
                        [min-height 760] [stretchable-height #f]))

(define btn-panel (new horizontal-panel% [parent train-frame]
                       [min-height 40] [stretchable-height #f]))
(define (train-field-list)
  (define loco-list '())
  (hash-for-each
   (rwm-ls railwaymodel)
   (lambda (id loco)
     (set! loco-list (cons (string-append "Train " (number->string (loco 'get-id))) loco-list))))
  loco-list)
(define train-choice-list (train-field-list))
(define chosen-train (car train-choice-list))
(define trains-field (new combo-field%
                          (label "Choose train")
                          (parent btn-panel)
                          (choices train-choice-list)
                          (init-value (car train-choice-list))
                          ))
(define (dt-field-list)
  (define dt-list '())
  (hash-for-each
   (rwm-dt railwaymodel)
   (lambda (id dt)
     (set! dt-list (cons (string-append "Detectiontrack " (number->string (dt 'get-id))) dt-list))))
  dt-list)
(define dt-choice-list (dt-field-list))
(define dt-field (new combo-field%
                      (label "Choose detectiontrack")
                      (parent btn-panel)
                      (choices dt-choice-list)
                      (init-value (car dt-choice-list))
                      ))

(new button% [parent btn-panel]
     [label "Drive"]
     ; Callback procedure for a button click:
     [callback (lambda (button event)
                 (send msg set-label "Button click"))]) ; Hier de geselecteerde trein laten rijden


; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ;; Define overriding method to handle mouse events
    ;(define/override (on-event event)
    ;  (send msg set-label "Canvas mouse"))
    ;; Define overriding method to handle keyboard events
    ;(define/override (on-char event)
    ;  (send msg set-label "Canvas keyboard"))
    ;; Call the superclass init, passing on all init args x1t y1t x2t y2t))
    (super-new)))

; Make a canvas that handles events in the frame
(define canvas (new my-canvas% [parent draw-panel]))
(define dc (send canvas get-dc))


(send train-frame show #t)
; Draw all the things
