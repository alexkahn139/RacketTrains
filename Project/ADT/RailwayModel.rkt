#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Railway Model ADT           ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "Switch.rkt")
(require "Track.rkt")
(require "Train.rkt")
(require "Node.rkt")

(provide (struct-out rwm)
         load-rwm
         ;fetch-track
         ;find-track
         ;find-dt
         ;find-nodes-middle
         ;track-eqv?
         )

(struct rwm (ls ns ss ts ds))
; Based on the file given by the assitants
(define (load-rwm filename)
  (let ([lines (map string-split (file->lines filename))]
        [ls '()]
        [ns '()]
        [ss '()]
        [ts '()]
        [ds '()])
    (for-each
     (lambda (l)
       (case (string->symbol (car l))
         [(L) (let* ([id (string->symbol (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [res (make-train id)])
                (set! ls (cons res ls)))]
         [(N) (let* ([id (string->symbol (list-ref l 1))]
                     [x (string->number (list-ref l 2))]
                     [y (string->number (list-ref l 3))]
                     [res (make-node id x y)])
                (set! ns (cons res ns)))]
         [(S) (let* ([nm (string->symbol (list-ref l 1))]
                     [n0 (string->symbol (list-ref l 2))]
                     [n1 (string->symbol (list-ref l 3))]
                     [n2 (string->symbol (list-ref l 4))]
                     [ms (string->symbol (list-ref l 5))]
                     [res (make-switch nm ms n0 n1 n2)])
                (set! ss (cons res ss)))]
         [(T) (let* ([n1 (string->symbol (list-ref l 1))]
                     [n2 (string->symbol (list-ref l 2))]
                     [ms (string->symbol (list-ref l 3))]
                     [res (make-track ms n1 n2)])
                (set! ts (cons res ts)))]
         [(D) (let* ([id (string->symbol (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [ms (string->symbol (list-ref l 4))]
                     [res (make-track ms n1 n2 'detection-block id)])
                (set! ds (cons res ds)))]))
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

#|
; Find a detection block in a railway model.
(define (find-db rwm n1 n2)
    (define d (findf (lambda (db)
       (let ([t1 (make-track n1 n2)]
             [t2 (make-detection-block-track db)])
                (track-eqv? t1 t2)))
                (rwm-ds rwm)))
                (if d
                    (detection-block-id d)
                    #f))|#
