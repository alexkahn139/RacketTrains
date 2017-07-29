#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Directed Acyclic Graphs: Topological Sorting          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (topological-sorting)
 (export dfs-topological-sort dfs-weighted-topological-sort bfs-topological-sort)
 (import (rnrs base) 
         (a-d scheme-tools)
         (a-d graph unweighted config)
         (a-d graph-traversing bft)
         (except (a-d graph-traversing dft-unweighted) root-nop node-nop edge-nop)
         (prefix (except (a-d graph-traversing dft-weighted) root-nop node-nop edge-nop) w:))
 
 (define (dfs-topological-sort g) ; postorder list construction
   (define series '())
   (dft g
        root-nop
        node-nop
        (lambda (node) 
          (set! series (cons node series)))
        edge-nop
        edge-nop
        edge-nop)
   series)
 
 (define (dfs-weighted-topological-sort g) ; postorder list construction
   (define series '())
   (w:dft g
          root-nop
          node-nop
          (lambda (node) 
            (set! series (cons node series)))
          edge-nop
          edge-nop
          edge-nop)
   series)
 
 (define (bfs-topological-sort g) ; sedgewick page 195 by source-reduction
   (define series '())
   (define sources '())
   (define in-degrees (make-vector (order g) 0))
   (bft g
        root-nop
        node-nop
        (lambda (from to) 
          (vector-set! in-degrees to (+ 1 (vector-ref in-degrees to))))
        (lambda (from to)
          (vector-set! in-degrees to (+ 1 (vector-ref in-degrees to)))))
   (vector-for-each+ in-degrees (lambda (node deg)
                                  (if (= deg 0)
                                      (set! sources (cons node sources))))) 
   (bft g
        root-nop
        (lambda (node)
          (set! series (cons node series)))
        (lambda (from to) 
          (vector-set! in-degrees to (- (vector-ref in-degrees to) 1))
          (= (vector-ref in-degrees to) 0))
        edge-nop
        sources)
   series))