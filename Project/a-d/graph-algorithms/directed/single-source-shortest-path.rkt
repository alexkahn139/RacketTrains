#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Single Source Shortest Path Algorithms             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (shortest-paths)
 (export dijkstra bellman-ford lawler)
 (import (rnrs base)
         (rnrs control)
         (a-d graph weighted config)
         (a-d graph-algorithms directed topological-sorting)
         (prefix (a-d priority-queue modifiable-heap) pq:))
 
 (define (relax! distances how-to-reach u v weight)
   (when (< (+ (vector-ref distances u) weight)
            (vector-ref distances v))
     (vector-set! distances v (+ (vector-ref distances u) weight))
     (vector-set! how-to-reach v u)))
 
 (define (lawler g source) ; only works for DAGs
   (define nodes (dfs-weighted-topological-sort g))
   (define distances (make-vector (order g) +inf.0))
   (define how-to-reach (make-vector (order g) '()))
   (vector-set! distances source 0)
   (map (lambda (from)
          (for-each-edge
           g
           from
           (lambda (weight to)
             (relax! distances how-to-reach from to weight))))
        nodes)
   (cons how-to-reach distances))
 
 (define (bellman-ford g source) ; always works
   (define distances (make-vector (order g) +inf.0))
   (define how-to-reach (make-vector (order g) '()))
   (vector-set! distances source 0)
   (do ((i 1 (+ i 1)))
     ((= i (order g)))
     (for-each-node
      g
      (lambda (from)
        (for-each-edge
         g
         from
         (lambda (weight to)
           (relax! distances how-to-reach from to weight))))))
   (call-with-current-continuation
    (lambda (exit)
      (for-each-node
       g
       (lambda (from)
         (for-each-edge
          g
          from
          (lambda (weight to)
            (if (< (+ (vector-ref distances from) weight)
                   (vector-ref distances to))
                (exit #f))))))
      (cons how-to-reach distances))))
 
 (define (dijkstra g source) ; only works for positive weights
   (define distances (make-vector (order g) +inf.0))
   (define how-to-reach (make-vector (order g) '()))
   (define pq-ids (make-vector (order g) '()))
   (define (track-ids id node distance)
     (vector-set! pq-ids node id))
   (define (id-of node)
     (vector-ref pq-ids node))
   (define pq (pq:new (order g) <))
   (vector-set! distances source 0)
   (for-each-node
    g
    (lambda (node)
      (pq:enqueue! pq node +inf.0 track-ids))) 
   (pq:reschedule! pq (id-of source) 0 track-ids)
   (let loop
     ((node&distance (pq:serve! pq track-ids)))
     (let ((from (car node&distance))
           (distance (cdr node&distance)))
       (for-each-edge
        g
        from
        (lambda (weight to) 
          (relax! distances how-to-reach from to weight) 
          (pq:reschedule! pq (id-of to) (vector-ref distances to) track-ids))))
     (unless (pq:empty? pq)
       (loop (pq:serve! pq track-ids))))
   (cons how-to-reach distances)))