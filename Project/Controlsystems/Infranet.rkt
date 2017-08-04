#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Infranet ADT              ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; http://www.cs.utah.edu/plt/mailarch/plt-scheme-2002/msg00955.html, helped me out a lot to start up the server side of things
(require racket/tcp)

(define port 88) ; Should be an unused port
(define max-allowed-wait 4)

; Stringifier, om nuttige informatie in een string te duwen

(define (server)
	(let ([listener (tcp-listen SERVICE-PORT)])
		(let loop ()
	    (let-values ([(client->me me->client)
			  						(tcp-accept listener)])
			; Hier conditional, voor al de mogelijke inkomende boodschappen
              ))))

(define (start infrabel)
  )
