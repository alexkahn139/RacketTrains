#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Railway Model ADT           ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "Switch.rkt")
(require "Track.rkt")
(require "Train.rkt")
(require "Node.rkt")
(require "DetectionTrack.rkt")

(provide (struct-out rwm)
         load-rwm
         get-track
         track-eqv?
         get-dt
         get-switch
         find-nodes-middle
				 find-railwaypiece
         find-train
         )

(struct rwm (ls ns ss ts dt))
; Based on the file given by the assitants
(define (load-rwm filename)
  (let ([lines (map string-split (file->lines filename))]
        [ls (make-hash)]
        [ns (make-hash)]
        [ss (make-hash)]
        [ts '()]
        [ds (make-hash)])
    (for-each
     (lambda (l)
       (case (string->symbol (car l))
         [(L) (let* ([id (string->symbol (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [res (make-train id)])
                (hash-set! ls id res))]
         [(N) (let* ([id (string->symbol (list-ref l 1))]
                     [x (string->number (list-ref l 2))]
                     [y (string->number (list-ref l 3))]
                     [res (make-node id x y)])
                (hash-set! ns id res))]
         [(S) (let* ([nm (string->symbol (list-ref l 1))]
                     [n0 (string->symbol (list-ref l 2))]
                     [n1 (string->symbol (list-ref l 3))]
                     [n2 (string->symbol (list-ref l 4))]
                     [ms (string->symbol (list-ref l 5))]
                     [res (make-switch nm n0 n1 n2)])
                (hash-set! ss nm res))]
         [(T) (let* ([n1 (string->symbol (list-ref l 1))]
                     [n2 (string->symbol (list-ref l 2))]
                     [ms (string->symbol (list-ref l 3))]
                     [res (make-track n1 n2 ms)])
                (set! ts (cons res ts)))]
         [(D) (let* ([id (string->symbol (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [ms (string->symbol (list-ref l 4))]
                     [res (make-dt id n1 n2 ms)])
                (hash-set! ds id res))]))
     lines)
    (rwm ls ns ss ts ds)))

(define (track-eqv? t1 t2)
  (or (and (eqv? (t1 'get-node1)
                 (t2 'get-node1))
           (eqv? (t1 'get-node2)
                 (t2 'get-node2)))
      (and (eqv? (t1 'get-node1)
                 (t2 'get-node2))
           (eqv? (t1 'get-node2)
                 (t2 'get-node1)))))


(define (get-track node1 node2)
  (define track (findf (lambda (t)
           (define testtrack (make-track node1 node2))
           (track-eqv? track testtrack))
         (rwm-ts)))
	track)
(define (find-railwaypiece node1 node2) ; You don't know if you need a switch, detection-block or track
	(define track '())
	(cond
		((get-dt node1 node2) (set! track (get-dt node1 node2)))
		((get-switch node1 node2) (set! track (get-switch node1 node2)))
		(else
			(set! track (get-track node1 node2))))
	track)


(define (get-dt node1 node2)
  (define detectiontrack #f)
  (hash-for-each (rwm-dt rwm)
                 (lambda (id dt)
                   (define t1 (make-track node1 node2))
                   (define t2 (make-track (dt 'get-node1) (dt 'get-node2)))
                   (when (track-eqv? t1 t2)
                     (set! detectiontrack (id dt)))))
                 detectiontrack)

(define (get-switch rwm node1 node2)
  (define switch #f)
  (hash-for-each (rwm-ss rwm)
                 (lambda (id s)
                   (define t1 (make-track node1 node2))
                   (define t2 (make-track (s 'get-node1) (s 'get-node2)))
                   (define t3 (make-track (s 'get-node1) (s 'get-node2)))
                   (when (or (track-eqv? t1 t2) (track-eqv? t1 t3))
                     (set! switch (id s)))))
                 switch)
(define (find-nodes-middle node1 node2)
  (define x1 (node1 'get-x))
  (define y1 (node1 'get-y))
  (define x2 (node2 'get-x))
  (define y2 (node2 'get-y))
  (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)))

(define (find-train train-id)
  (hash-ref (rwm-ls rwm) train-id))
