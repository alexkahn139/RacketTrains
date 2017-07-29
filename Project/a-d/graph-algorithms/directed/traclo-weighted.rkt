#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*        Transitive Closure Algorithms for Weighted Graphs        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (traclo-weighted)
 (export floyd-warshall traclo-weighted-exp)
 (import (rnrs base)
         (rnrs control)
         (a-d scheme-tools)
         (a-d graph weighted config)
         (a-d graph-algorithms directed basic)
         (a-d graph-algorithms directed connectivity)
         (a-d graph-traversing dft-unweighted))
 
 (define (traclo-weighted-exp g)
   (define (rec from to via)
     (if (= via -1)
       (weight g from to)
       (let ((old (rec from to (- via 1)))
             (p1  (rec from via (- via 1)))
             (p2  (rec via to   (- via 1))))
         (min old (+ p1 p2)))))
   (define traclo-distances 
     (make-2D-vector
      (order g)
      (order g)
      (lambda (i j)
        (rec i j (- (order g) 1)))))
   traclo-distances)
 
 (define (floyd-warshall g)
   (define traclo-distances
     (make-2D-vector (order g)
                     (order g)
                     (lambda (i j)
                       (weight g i j))))
   (for-each-node
    g
    (lambda (via)
      (for-each-node
       g
       (lambda (from)
         (for-each-node
          g
          (lambda (to)
            (ij! 
             traclo-distances 
             from to (min (ij? traclo-distances from to)
                          (+ (ij? traclo-distances from via)
                             (ij? traclo-distances via to))))))))))
   traclo-distances))