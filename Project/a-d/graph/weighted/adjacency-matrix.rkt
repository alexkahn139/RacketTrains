#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*        Weighted Graphs (Adjacency Matrix Representation)        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (weighted-graph)
 (export new weighted-graph? order directed? nr-of-edges
         for-each-node for-each-edge
         add-edge! delete-edge! 
         adjacent? weight)
 (import (rnrs base)
         (srfi :9)
         (rnrs control)
         (rnrs mutable-pairs))
 
 (define-record-type weighted-graph
   (make d n s)
   weighted-graph?
   (d directed?)
   (n nr-of-edges nr-of-edges!)
   (s storage))

 (define (new directed order)
   (define (make-row i)
     (if directed
       (make-vector order +inf.0)
       (make-vector (- i 1) +inf.0)))
   (define rows (make-vector order))
   (make directed
         0 ; nr of edges
         (let for-all
           ((row 1))
           (vector-set! rows (- row 1) (make-row row))
           (if (< row order)
               (for-all (+ row 1))
               rows))))
 
 (define (order graph)
   (vector-length (storage graph)))
 
 (define (for-each-node graph proc)
   (let iter-nodes
     ((node 0))
     (proc node)
     (if (< (+ node 1) (order graph))
       (iter-nodes (+ node 1))))
   graph)
 
 (define (for-each-edge graph from proc)
   (let iter-edges
     ((to 0)
      (wg (weight graph from 0)))
     (if (not (eq? wg +inf.0))
       (proc wg to))
     (if (< (+ to 1) (order graph))
       (iter-edges (+ to 1) (weight graph from (+ to 1)))))
   graph)
 
 (define (add-edge! graph from to weight)
   (define rows (storage graph))
   (define row-idx (if (directed? graph) from (max from to)))
   (define col-idx (if (directed? graph) to (min from to)))
   (define row (vector-ref rows row-idx))
   (when (= (vector-ref row col-idx) +inf.0)
     (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
   (vector-set! row col-idx weight)
   graph)
 
 (define (delete-edge! graph from to)
   (define rows (storage graph))
   (define row-idx (if (directed? graph) from (max from to)))
   (define col-idx (if (directed? graph) to (min from to)))
   (define row (vector-ref rows row-idx))
   (when (not (= (vector-ref row col-idx) +inf.0))
     (nr-of-edges! graph (- (nr-of-edges graph) 1)))
   (vector-set! row col-idx +inf.0)
   graph)
 
 (define (adjacent? graph from to)
   (not (eq? (weight graph from to) +inf.0)))
 
  (define (weight graph from to)
   (define rows (storage graph))
   (cond ((directed? graph)
          (let ((row (vector-ref rows from)))
            (vector-ref row to)))
         ((= from to)
          +inf.0)
         (else
          (let ((row (vector-ref rows (max from to))))
            (vector-ref row (min from to)))))))