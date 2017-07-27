#lang racket/gui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/gui/base)
; Load all the bitmaps
 (define locomotive  (read-bitmap "loco.bmp"))
; Make a frame by instantiating the frame% class
(define train-frame (new frame%
                         [label "Racket Trains"]
                         [style '(no-resize-border)]
                         [width 800]
                         [height 600]))
 
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
(new my-canvas% [parent draw-panel])

(send train-frame show #t)