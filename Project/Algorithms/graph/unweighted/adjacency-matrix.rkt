#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*       Unweighted Graphs (Adjacency Matrix Representation)       *-*-
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
   (define (make-row i)
     (if directed
       (make-vector order #f)
       (make-vector (- i 1) #f)))
   (define rows (make-vector order))
   (make directed
         0 ; nr-of-edges
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
     ((to 0))
     (if (adjacent? graph from to)
         (proc to))
     (if (< (+ to 1) (order graph))
         (iter-edges (+ to 1))))
   graph)
 
 (define (add-edge! graph from to)
   (define rows (storage graph))
   (define row-idx (if (directed? graph) from (max from to)))
   (define col-idx (if (directed? graph) to (min from to)))
   (define row (vector-ref rows row-idx))
   (when (not (vector-ref row col-idx))
     (vector-set! row col-idx #t)
     (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
   graph)
 
 (define (delete-edge! graph from to)
   (define rows (storage graph))
   (define row-idx (if (directed? graph) from (max from to)))
   (define col-idx (if (directed? graph) to (min from to)))
   (define row (vector-ref rows row-idx))
   (when (vector-ref row col-idx)
     (vector-set! row col-idx #f)
     (nr-of-edges! graph (- (nr-of-edges graph) 1)))
   graph)

 (define (adjacent? graph from to)
   (define rows (storage graph))
   (if (directed? graph)
       (let ((row (vector-ref rows from)))
         (vector-ref row to))
       (and (not (= from to))
            (vector-ref (vector-ref rows (max from to)) 
                        (min from to))))))