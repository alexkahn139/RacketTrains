#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             NMBSnet ADT              ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../Abstractions.rkt")


(provide client
				 translate)

; http://www.cs.utah.edu/plt/mailarch/plt-scheme-2002/msg00955.html, helped me out a lot to start up the server side of things
(require racket/tcp)

(define SERVICE-PORT 4000) ; Should be an unused port
(define SERVER-HOST "localhost")

(define (client message)
	(let-values ([(server->me me->server)
								(tcp-connect SERVER-HOST SERVICE-PORT)])
		(write message me->server)
		(close-output-port me->server)
		(let ([response (read server->me)])
        (display response) (newline)
        (close-input-port server->me)
				response)))

(define (translate response)
	(set! response (string-split response))
	(set! response (cdr response)) ; The denomifier is not needed here
	(displayln response)
	(displayln (car response))
	(define output '())
	(define (translate-loop list) ; Takes the list of responses
		(if (null? list)
				(reverse output)
				(begin
					(set! output (cons (cons (string-to-id (car list)) (string-to-number (car list))) output))
					(translate-loop (cdr list)))))
	(translate-loop response))
