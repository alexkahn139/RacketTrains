#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Connectivity Tests for Undirected Graphs            *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (connectivity)
 (export connected-components/dft connected-components/bft
         biconnected-components edge-connected-components 
         bipartite/dft? bipartite/bft?)
 (import (rnrs base)
         (rnrs control)
         (a-d graph unweighted config)
         (a-d graph-traversing dft-unweighted)
         (except (a-d graph-traversing bft) node-nop root-nop edge-nop))
 
 
 (define (connected-components/dft g)
   (define number-of-components 0)
   (define connected-components (make-vector (order g) '()))
   (dft g
        (lambda (root)
          (set! number-of-components (+ number-of-components 1)))
        (lambda (node)
          (vector-set! connected-components node number-of-components))
        node-nop
        edge-nop
        edge-nop
        edge-nop)
   (cons number-of-components connected-components))
 
 (define (connected-components/bft g)
   (define number-of-components 0)
   (define connected-components (make-vector (order g) '()))
   (bft g
        (lambda (root)
          (set! number-of-components (+ number-of-components 1)))
        (lambda (node) 
          (vector-set! connected-components node number-of-components))
        edge-nop
        edge-nop)
   (cons number-of-components connected-components))
 
 (define (biconnected-components g) ; algoritme van hopcroft
   (define preorder-time 0)
   (define preorder-numbers (make-vector (order g) 0))
   (define parents (make-vector (order g) '()))
   (define highest-back-edge (make-vector (order g) -1))
   (define articulation-points (make-vector (order g) #f))
   (define current-root '())
   (define branch-count 0)
   (define (root-branch? node)
     (if (= node current-root) 
         (set! branch-count (+ 1 branch-count))))
   (define (root-node! node) 
     (set! branch-count 0) 
     (set! current-root node))
   (define (root-node? node)
     (= node current-root))
   (dft g
        root-node!
        (lambda (node)
          (vector-set! preorder-numbers node preorder-time)
          (vector-set! highest-back-edge node preorder-time) 
          (set! preorder-time (+ preorder-time 1)))
        (lambda (node) 
          (if (root-node? node)
              (vector-set! articulation-points node (>= branch-count 2))))
        (lambda (from to)
          (root-branch? from)
          (vector-set! parents to from))
        (lambda (from to)
          (vector-set! highest-back-edge 
                       from (min (vector-ref highest-back-edge from)
                                 (vector-ref highest-back-edge to)))
          (if (>= (vector-ref highest-back-edge to)
                  (vector-ref preorder-numbers from))
              (vector-set! articulation-points from #t)))
        (lambda (from to)
          (if (not (eq? (vector-ref parents from) to)) 
              (vector-set! highest-back-edge 
                           from (min (vector-ref highest-back-edge from)
                                     (vector-ref preorder-numbers to))))))
   articulation-points)
 
 (define (edge-connected-components g)
   (define preorder-time 0)
   (define preorder-numbers (make-vector (order g) 0))
   (define parents (make-vector (order g) '()))
   (define highest-back-edge (make-vector (order g) 0))
   (define bridges '())
   (dft g
        root-nop
        (lambda (node)
          (vector-set! preorder-numbers node preorder-time)
          (vector-set! highest-back-edge node preorder-time)
          (set! preorder-time (+ preorder-time 1)))
        node-nop
        (lambda (from to)
          (vector-set! parents to from))
        (lambda (from to)
          (vector-set! highest-back-edge
                       from (min (vector-ref highest-back-edge from)
                                 (vector-ref highest-back-edge to)))
          (when (= (vector-ref preorder-numbers to)
                   (vector-ref highest-back-edge to))
            (set! bridges (cons (cons from to) bridges))))
        (lambda (from to)
          (if (not (eq? (vector-ref parents from) to))
              (vector-set! highest-back-edge 
                           from (min (vector-ref highest-back-edge from) 
                                     (vector-ref preorder-numbers to))))))
   bridges)
 
  (define (bipartite/dft? g)
   (define bipartite #t)
   (define colors (make-vector (order g) ()))
   (dft g
        (lambda (root)
          (vector-set! colors root #t))
        node-nop
        (lambda (node) 
          bipartite)
        (lambda (from to)
          (vector-set! colors to (not (vector-ref colors from))))
        edge-nop
        (lambda (from to)
          (set! bipartite  (and bipartite 
                                (not (eq? (vector-ref colors from)
                                          (vector-ref colors to)))))))
   (cons bipartite colors))

 (define (bipartite/bft? g)
   (define bipartite #t)
   (define colors (make-vector (order g) '()))
   (bft g
        (lambda (root)
          (vector-set! colors root #t))
        (lambda (node)
          bipartite)
        (lambda (from to)
          (vector-set! colors to (not (vector-ref colors from))))
        (lambda (from to)
          (set! bipartite (and bipartite 
                               (not (eq? (vector-ref colors to) 
                                         (vector-ref colors from)))))))
   (cons bipartite colors)))