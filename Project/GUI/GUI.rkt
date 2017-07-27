#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")

;(define (make-gui title infra nmbs)
(define rwm (load-rwm "../be_simple.txt"))
(define title "Racket Trains")
(define frame (new frame% [label title] [width 600] [height 600]))
;(define background (read-bitmap ))
(define btn-panel (new horizontal-panel% [parent frame]
                               [min-height 40] [stretchable-height #f]))
(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 3 3)
        (send dc set-text-foreground "green")
        (send dc draw-text "Don't Panic!" 0 0))])
(define buttons   (list (new button% [parent btn-panel]
                                     [label "Drive"]
                                     [callback (λ (button event)
                                                 (display "lol"))])
                                (new button% [parent btn-panel]
                                     [label "Change switch"]
                                     [callback (λ (button event)
                                                 (display "pap-switch"))])
                                ))
(send frame show #t)


;)
