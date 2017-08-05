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

(define SERVICE-PORT 4000) ; Should be an unused port
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

(define (server)
  (let ([listener (tcp-listen SERVICE-PORT)])
    (define (loop)
      (let-values ([(client->me me->client)
                    (tcp-accept listener)])
        ; Hier conditional, voor al de mogelijke inkomende boodschappen
        (let ((message "msg not understood"))
          (define msg (read client->me))
          (cond
            ; Post commands
            ((eq? msg 'get-all-dt) (set! message (stringify 'dt ((infrabel 'get-all-dt)))))
            ((eq? msg 'get-all-loco) (set! message (stringify 'locomotive ((infrabel 'get-all-loco)))))
            ((eq? msg 'get-all-switch) (set! message (stringify 'switch ((infrabel 'get-all-switch)))))
            ; Put commands
            ; Need to find a way, to split the msg and the args
            )
          (close-input-port client->me)
          (write message me->client)
          (close-output-port me->client)))
      (loop))
    (loop)))

(define (set-up-server infra)
  (set! server-up #t)
  (set! infrabel infra)
  (set! listen-thread (thread server)))
