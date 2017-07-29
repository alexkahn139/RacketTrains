#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*       Unweighted Graphs (Adjacency List Representation)         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (unweighted-graph)
 (export new unweighted-graph? order nr-of-edges directed? 
         for-each-node for-each-edge
         add-edge! delete-edge!
         adjacent?)
 (import (rnrs base)
         (srfi :9)
         (rnrs control)
         (rnrs mutable-pairs))
 
 (define-record-type unweighted-graph
   (make d n s)
   unweighted-graph?
   (d directed?)
   (n nr-of-edges nr-of-edges!)
   (s storage))

 (define (new directed order)
   (make directed 0 (make-vector order ())))
 
 (define (order graph)
   (vector-length (storage graph)))
 
 (define (for-each-node graph proc)
   (define lists (storage graph))
   (let iter-nodes
     ((node 0))
     (proc node)
     (if (< (+ node 1) (vector-length lists))
       (iter-nodes (+ node 1))))
   graph)
 
 (define (for-each-edge graph from proc)
   (define row (vector-ref (storage graph) from))
   (let iter-edges
     ((edges row))
     (when (not (null? edges))
       (proc (car edges))
       (iter-edges (cdr edges))))
   graph)
 
 (define (add-edge! graph from to)
   (define lists (storage graph))
   (define (insert-sorted to prev next! next)
     (cond 
       ((or (null? next)
            (> to (car next)))
        (next! prev (cons to next))
        #t)
       ((= to (car next))
        #f)
       (else
        (insert-sorted to next set-cdr! (cdr next)))))
   (define (head-setter head) 
     (lambda (ignore next)
       (vector-set! lists head next)))
   (if (insert-sorted to '() (head-setter from) (vector-ref lists from))
     (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
   (if (not (directed? graph))
     (insert-sorted from '() (head-setter to) (vector-ref lists to)))
   graph)
 
 (define (delete-edge! graph from to)
   (define lists (storage graph))
   (define (delete-sorted to prev next! next)
     (cond
       ((or (null? next)
            (> to (car next)))
        #f)
       ((= to (car next))
        (next! prev (cdr next))
        #t)
       (else
        (delete-sorted to next set-cdr! (cdr next)))))
   (define (head-setter head) 
     (lambda (ignore next)
       (vector-set! lists head next)))
   (if (delete-sorted to '() (head-setter from) (vector-ref lists from))
     (nr-of-edges! graph (- (nr-of-edges graph) 1)))     
   (if (not (directed? graph))
     (delete-sorted from '() (head-setter to) (vector-ref lists to)))
   graph)
 
 (define (adjacent? graph from to)
   (define lists (storage graph))
   (let search-sorted
     ((current (vector-ref lists from)))
     (cond 
       ((or (null? current)
            (< (car current) to))
        #f)
       ((= (car current) to)
        #t)
       (else
        (search-sorted (cdr current)))))))