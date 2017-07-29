#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Minimum Spanning Forests                     *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (basic algorithms)
 (export mst-prim-jarnik mst-kruskal mst-boruvka)
 (import (rnrs base)
         (rnrs control)
         (rename (a-d sorting internal comparative quicksort) (sort quicksort))
         (a-d scheme-tools)
         (a-d graph weighted config)
         (prefix (a-d priority-queue modifiable-heap) pq:)
         (prefix (a-d disjoint-sets optimized) dset:))
 
 (define (mst-prim-jarnik g)
   (define tree (make-vector (order g) '()))
   (define pq-ids (make-vector (order g) '()))
   (define (track-ids id node weight) 
     (vector-set! pq-ids node id))
   (define (id-of node)
     (vector-ref pq-ids node))
   (define (pty< edge1 edge2)
     (< (cdr edge1) (cdr edge2)))
   (define pq (pq:new (order g) pty<))
   (define (prim-jarnik-iter closest-node&edge)
     (define closest-node (car closest-node&edge))
     (define closest-edge (cdr closest-node&edge))
     (vector-set! tree closest-node closest-edge)
     (for-each-edge
      g
      closest-node
      (lambda (weight to)
        (define edge-to-to (cons closest-node weight))
        (if (null? (id-of to))
            (pq:enqueue! pq to edge-to-to track-ids)
            (if (and (null? (vector-ref tree to)) ; to is still worth considering since it is not part of the MST yet
                     (pty< edge-to-to (pq:priority-of pq (id-of to)))) ; lazy evaluation!
                (pq:reschedule! pq (id-of to) edge-to-to track-ids)))))
     (unless (pq:empty? pq)
       (prim-jarnik-iter (pq:serve! pq track-ids))))
   (for-each-node g (lambda (node)
                      (when (null? (vector-ref tree node))
                        (pq:enqueue! pq node (cons '() +inf.0) track-ids)
                        (prim-jarnik-iter (pq:serve! pq track-ids)))))
   tree)
 
 (define (all-edges g)
   (define n-e (nr-of-edges g))
   (define edges (make-vector n-e))
   (define edge-count 0)
   (for-each-node
    g
    (lambda (from)
      (for-each-edge
       g
       from
       (lambda (weight to)
         (when (<= from to) ; don't take the reverse edges
           (vector-set! edges edge-count (cons from (cons weight to)))
           (set! edge-count (+ 1 edge-count)))))))
   edges)
 
 (define (mst-kruskal g)
   (define edges (all-edges g))
   (define n-e (vector-length edges))
   (define forest '())
   (define node-sets (dset:new (order g)))
   (quicksort edges (lambda (edge1 edge2) (< (cadr edge1) (cadr edge2))))
   (do ((edge-idx 0 (+ edge-idx 1)))
     ((= edge-idx n-e) forest)
     (let* ((edge (vector-ref edges edge-idx))
            (from (car edge))
            (weight (cadr edge))
            (to (cddr edge))
            (from-set (dset:find node-sets from))
            (to-set (dset:find node-sets to)))
       (when (not (dset:same-set? from-set to-set)) 
         (set! forest (cons (list from weight to) forest))
         (dset:union! node-sets from-set to-set)))))
 
 (define (mst-boruvka g)
   (define n-e (nr-of-edges g))
   (define edges (all-edges g))
   (define infty-edge (cons 0 (cons +inf.0 0)))
   (define (edge< edge1 edge2)
     (< (cadr edge1) (cadr edge2)))
   (define selected-edges (make-vector (order g) infty-edge))
   (define node-sets (dset:new (order g)))
   (define (select-edges remaining-edges)
     (let find-minimals
       ((edge-idx 0)
        (first-free 0))
       (if (< edge-idx remaining-edges)
           (let* ((edge (vector-ref edges edge-idx))
                  (from (car edge))
                  (to   (cddr edge))
                  (from-set (dset:find node-sets from))
                  (to-set   (dset:find node-sets to)))
             (cond ((dset:same-set? from-set to-set) ; edge already in MST-forest
                    (find-minimals (+ edge-idx 1) first-free)) ; skip it
                   (else (if (edge< edge (vector-ref selected-edges from-set))
                             (vector-set! selected-edges from-set edge)) ; closer=>register the edge
                         (if (edge< edge (vector-ref selected-edges to-set))
                             (vector-set! selected-edges to-set edge))   ; dito
                         (vector-set! edges first-free edge) ; store it in first-free
                         (find-minimals (+ edge-idx 1) (+ first-free 1)))))
           first-free))) 
   (define tree '())
   (do ((remaining-edges (select-edges n-e) (select-edges remaining-edges)))
     ((= remaining-edges 0) tree)
     (vector-map!
      selected-edges
      (lambda (idx edge)
        (define mst-1 (dset:find node-sets (car edge)))
        (define mst-2 (dset:find node-sets (cddr edge)))
        (when (and (not (eq? edge infty-edge))
                   (not (dset:same-set? mst-1 mst-2))) ; mst-1 and mst-2 can be the same because of this very loop
          (dset:union! node-sets mst-1 mst-2)
          (set! tree (cons edge tree)))
        infty-edge)))))