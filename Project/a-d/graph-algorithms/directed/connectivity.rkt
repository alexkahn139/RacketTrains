#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Strong Connectivity                        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (connectivity)
 (export scc-tarjan scc-kosaraju)
 (import (rnrs base)
         (rnrs lists)
         (rnrs control)
         (prefix (a-d stack linked) stack:)
         (a-d graph-algorithms directed basic)
         (a-d graph unweighted config)
         (a-d graph-traversing dft-unweighted))
 
 (define (scc-kosaraju g)
   (define g~ (transpose g))
   (define greatest-postorder '())
   (define nr-of-components 0)
   (define sc-components (make-vector (order g) 0))
   (dft
    g
    root-nop
    node-nop
    (lambda (node)
      (set! greatest-postorder (cons node greatest-postorder))) ; reverse post-order
    edge-nop
    edge-nop
    edge-nop)
   (dft
    g~ 
    (lambda (root)
      (set! nr-of-components (+ 1 nr-of-components)))
    (lambda (node)
      (vector-set! sc-components node nr-of-components))
    node-nop
    edge-nop
    edge-nop
    edge-nop
    greatest-postorder) ; greatest post-order first
   (cons nr-of-components sc-components))
 
 (define (scc-tarjan g)
   (define preorder-time 0)
   (define preorder-numbers (make-vector (order g) -1))
   (define highest-back-edge (make-vector (order g) -1))
   (define sc-components (make-vector (order g) -1))
   (define included (make-vector (order g) #f))
   (define nr-of-components 0)
   (define stack (stack:new))
   (dft
    g
    root-nop
    (lambda (node)
      (stack:push! stack node)
      (vector-set! preorder-numbers node preorder-time)
      (vector-set! highest-back-edge node preorder-time)
      (set! preorder-time (+ preorder-time 1)))
    (lambda (node)
      (when (= (vector-ref highest-back-edge node)
               (vector-ref preorder-numbers node)) 
        (set! nr-of-components (+ 1 nr-of-components))
        (let loop
          ((t (stack:pop! stack))) 
          (vector-set! sc-components t nr-of-components)
          (vector-set! included t #t)
          (unless (eq? t node)
            (loop (stack:pop! stack))))))
    edge-nop  ; before
    (lambda (from to) ; after => 
      (if (not (vector-ref included to))
          (vector-set! highest-back-edge
                       from (min (vector-ref highest-back-edge from)
                                 (vector-ref highest-back-edge to)))))
    (lambda (from to) ; bump  => avoid cross-edges
      (if (not (vector-ref included to))
          (vector-set! highest-back-edge
                       from (min (vector-ref highest-back-edge from)
                                 (vector-ref preorder-numbers to))))))
   (cons nr-of-components sc-components))
 
 ; The gabow algorithm hasn't been maintained very well
 ; This is old code that may have bugs because all the rest
 ; of the graph code went through several evolution steps
 (define (scc-gabow g)
   (define components (make-vector (order g) -1))
   (define pre (make-vector (order g) -1))
   (define stack1 '())
   (define stack2 '())
   (define comp-count 0)
   (define count 0)
   (dft
    g
    root-nop
    (lambda (node)
      (set! stack1 (cons node stack1))
      (set! stack2 (cons node stack2))
      (vector-set! pre node count)
      (set! count (+ count 1)))
    (lambda (node)
      (when (eq? (car stack2) node)
        (set! stack2 (cdr stack2))
        (let loop
          ((v (car stack1))) 
          (set! stack1 (cdr stack1))
          (vector-set! components v comp-count)
          (if (not (eq? v node))
              (loop (car stack1))
              (set! comp-count (+ comp-count 1))))))
    edge-nop
    edge-nop
    (lambda (from to)
      (if (= -1 (vector-ref components to))
          (let loop
            ()
            (when (> (vector-ref pre (car stack2)) (vector-ref pre to))
              (set! stack2 (cdr stack2))
              (loop))))))
   (cons comp-count components))
 
 )