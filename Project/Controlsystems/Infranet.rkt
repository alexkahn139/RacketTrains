#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Infranet ADT              ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; http://www.cs.utah.edu/plt/mailarch/plt-scheme-2002/msg00955.html, helped me out a lot to start up the server side of things
(require racket/tcp)

(provide set-up-server)

(define server-up #f)

(define infrabel '())

(define listen-thread #f)

(define SERVICE-PORT #f) ; Should be an unused port
(define max-allowed-wait 4)

; Stringifier, om nuttige informatie in een string te duwen
(define (stringify denomifier list)
  (define (number-or-bool-to-string input)
    (if (number? input)
        (number->string input)
        (if input
            "true"
            "false")))
  (define (car-and-cdr-to-string cons-cell)
    (string-append (number-or-bool-to-string (car cons-cell)) "-" (number-or-bool-to-string (cdr cons-cell)) " "))
  (define (list-to-string list string)
    (if (null? list)
        string
        (list-to-string (cdr list) (string-append string (car-and-cdr-to-string (car list))))))
  (define output '())
  ; The first letter of the string is the denomifier
  (cond
    ((eq? denomifier 'locomotive) (set! output "L "))
    ((eq? denomifier 'dt) (set! output "D "))
    ((eq? denomifier 'switch) (set! output "S "))
    (else (error "Denomifier not known - " denomifier))
    )
  ; The list should also be added to the string
  (set! output (string-append (list-to-string list output)))
  output)

(define (translate-path string) ; Changes the string in to a usable path that can be used by a train
  (define path '())
  (set! string (string-split string))
  (define train-id (string->number (car string)))
  (define (path-loop str-list)
    (when (not (null? str-list))
      (set! path (cons (string->symbol (car str-list)) path))
      (path-loop (cdr str-list))))
  (path-loop (cdr string))
  (set! path (reverse path))
  ((infrabel 'set-new-destination!) train-id path)
  (displayln path)
  )

(define (server) ; This is the TCP server. It has a few messages to wich it responds with a specific function
  (let ([listener (tcp-listen SERVICE-PORT)])
    (define (loop)
      (let-values ([(client->me me->client)
                    (tcp-accept listener)])
        (let ((message "msg not understood"))
          (define msg (read client->me))
          (cond
            ; Post commands
            ((eq? msg 'get-all-dt) (set! message (stringify 'dt ((infrabel 'get-all-dt)))))
            ((eq? msg 'get-all-loco) (set! message (stringify 'locomotive ((infrabel 'get-all-loco)))))
            ((eq? msg 'get-all-switch) (set! message (stringify 'switch ((infrabel 'get-all-switch)))))
            ; Put commands
            (else
             (begin
               (translate-path msg)
               (set! message 'Received-new-path))))
          (close-input-port client->me)
          (write message me->client)
          (close-output-port me->client)))
      (loop))
    (loop)))

(define (set-up-server infra port)
  (set! SERVICE-PORT port)
  (set! server-up #t)
  (set! infrabel infra)
  (set! listen-thread (thread server)))
