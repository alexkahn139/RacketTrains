#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             NMBSnet ADT              ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide client)

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
        (close-input-port server->me))))
