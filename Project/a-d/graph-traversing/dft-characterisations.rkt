#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Depth First Traversal Characterisations            *-*-
;-*-*                                                                 *-*-
;-*-*                 Wolfgang De Meuter & Kevin Pinte                *-*-
;-*-*                 2009-2010  Software Languages Lab               *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (dft-characterisations)
 (export dft-forest dft-undirected-edge-classification dft-directed-edge-classification dft-node-numbering)
 (import (rnrs base)
         (rnrs mutable-pairs)
         (rnrs io simple)
         (a-d scheme-tools)
         (a-d graph unweighted config)
         (a-d graph-traversing dft-unweighted) 
         (a-d graph examples undirected-unweighted)
         (prefix (a-d graph labeled adjacency-matrix) labeled:))
 
 (define (dft-forest g)
   (define children (make-vector (order g) '()))
   (define roots ())
   (dft g
        (lambda (root)
          (set! roots (cons root roots)))
        node-nop
        node-nop
        (lambda (from to) 
          (vector-set! children from (cons to (vector-ref children from))))
        edge-nop
        edge-nop)
   (cons roots children))
 
 (define (dft-node-numbering g)
   (define preorder-numbers (make-vector (order g) 0))
   (define postorder-numbers (make-vector (order g) 0))
   (define visit-times (make-vector (order g)))
   (define time 0)
   (define preorder 0)
   (define postorder 0)
   (vector-map! visit-times (lambda (idx val) (cons 0 0)))
   (dft g
        root-nop
        (lambda (node)
          (set! preorder (+ preorder 1))
          (set! time (+ time 1))
          (vector-set! preorder-numbers node preorder)
          (set-car! (vector-ref visit-times node) time))
        (lambda (node)
          (set! postorder (+ postorder 1))
          (set! time (+ time 1))
          (vector-set! postorder-numbers node postorder)
          (set-cdr! (vector-ref visit-times node) time))
        edge-nop
        edge-nop
        edge-nop)
   (list preorder-numbers postorder-numbers visit-times))
 
 (define (dft-undirected-edge-classification g)
   (define labels (labeled:new #f (order g))) ; #f is crucial to the algorithm!
   (define roots ())
   (dft g
        (lambda (root)
          (set! roots (cons root roots)))
        node-nop
        node-nop
        (lambda (from to)
          (labeled:add-edge! labels from to 'tree))
        edge-nop
        (lambda (from to)
          (if (not (labeled:edge-label labels from to)) ; undirected is crucial!
              (labeled:add-edge! labels from to 'back))))
   (cons roots labels))
 
 (define (dft-directed-edge-classification g)
   (define labels (labeled:new #t (order g))) ; #t is crucial to the algorithm!
   (define roots ())
   (define preorder-numbers (make-vector (order g) 0))
   (define postorder-numbers (make-vector (order g) +inf.0))
   (define preorder 0)
   (define postorder 0)
   (dft g
        (lambda (root)
          (set! roots (cons root roots)))
        (lambda (node)
          (set! preorder (+ preorder 1))
          (vector-set! preorder-numbers node preorder))
        (lambda (node)
          (set! postorder (+ postorder 1))
          (vector-set! postorder-numbers node postorder))
        (lambda (from to)
          (labeled:add-edge! labels from to 'tree))
        edge-nop
        (lambda (from to)
          (let* ((from-pre (vector-ref preorder-numbers from))
                 (from-post (vector-ref postorder-numbers from))
                 (to-pre (vector-ref preorder-numbers to))
                 (to-post (vector-ref postorder-numbers to))
                 (label
                  (cond ((= from to) 'back) ; self-loops
                        ((and (= to-post from-post)
                              (< to-pre from-pre))
                         ; from and to in same subtree (both not yet processed)
                         ; from ancestor of to (from discovered before to)
                         'back)
                        ((and (< to-post from-post)
                              (> to-pre from-pre))
                         ; from is ancestor of to
                         'down)
                        ((and (< to-post from-post)
                              (< to-pre from-pre))
                         ; from is in different tree than to
                         'cross)
                        (else 'unknown)))) ; we should never get into the else case
            (labeled:add-edge! labels from to label))))
   (cons roots labels)))