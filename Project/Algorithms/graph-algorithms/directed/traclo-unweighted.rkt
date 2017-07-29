#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*       Transitive Closure Algorithms for Unweighted Graphs       *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (traclos-unweighted)
 (export traclo-very-naive traclo-naive traclo-exp traclo-warshall 
         traclo-warshall* traclo-dfs traclo-scc)
 (import (rnrs base)
         (rnrs control)
         (a-d scheme-tools)
         (a-d graph unweighted config)
         (a-d graph-algorithms directed basic)
         (a-d graph-algorithms directed connectivity)
         (a-d graph-traversing dft-unweighted))
 
 (define (multiply g1 g2 res)
   (for-each-node
    g1
    (lambda (from)
      (for-each-node
       g2
       (lambda (to)
         (define edge #f)
         (for-each-node
          g1
          (lambda (via)
            (set! edge (or edge (and (adjacent? g1 from via)
                                     (adjacent? g2 via to))))))
         (if edge
             (add-edge! res from to))))))
   res)
 
 (define (traclo-very-naive g)
   (do ((i (order g) (- i 1))
        (res1 (copy g) res2)
        (res2 (multiply g g (copy g)) res1))
     ((= i 0) res2)
     (multiply res2 g res1)))
 
 (define (traclo-naive g)
   (do ((i (order g) (div i 2))
        (res1 (copy g) res2)
        (res2 (multiply g g (copy g)) res1))
     ((= i 0) res2)
     (multiply res2 res2 res1)))
 
 (define (traclo-exp g)
   (define res (copy g))
   (define (rec from to via)
     (if (= via -1)
       (adjacent? g from to)
       (let ((no-via (rec from to (- via 1)))
             (p1     (rec from via (- via 1)))
             (p2     (rec via  to  (- via 1))))
         (or no-via (and p1 p2)))))
   (for-each-node
    g
    (lambda (from)
      (for-each-node
       g
       (lambda (to)
         (if (rec from to (- (order g) 1))
           (add-edge! res from to)))))))
 
 (define (traclo-warshall g)
   (for-each-node
    g
    (lambda (via)
      (for-each-node
       g
       (lambda (from)
         (for-each-node
          g
          (lambda (to)
            (if (and (adjacent? g from via)
                     (adjacent? g via to))
                (add-edge! g from to)))))))))
 
 (define (traclo-warshall* g)
   (for-each-node
    g
    (lambda (via)
      (for-each-node
       g
       (lambda (from)
         (if (adjacent? g from via)
             (for-each-node
              g
              (lambda (to)
                (if (adjacent? g via to)
                    (add-edge! g from to))))))))))
 
 (define (traclo-dfs g)
   (define (dfs-rec v w)
     (for-each-edge
      g
      w
      (lambda (t)
        (when (not (adjacent? g v t))
          (add-edge! g v t)
          (dfs-rec v t)))))
   (for-each-node
    g
    (lambda (from)
      (for-each-edge
       g
       from
       (lambda (to)
         (dfs-rec from to))))))
 
 (define (traclo-scc g)
   (define scc (scc-tarjan g))
   (define K (new #t (car scc)))
   (define components (cdr scc))
   (for-each-node
    g
    (lambda (from)
      (for-each-edge
       g
       from
       (lambda (to)
         (add-edge!
          K
          (vector-ref components from)
          (vector-ref components to))))))
   K))