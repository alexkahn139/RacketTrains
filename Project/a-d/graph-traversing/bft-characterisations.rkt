#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Breadth First Traversal Characterisations           *-*-
;-*-*                                                                 *-*-
;-*-*                 Wolfgang De Meuter & Kevin Pinte                *-*-
;-*-*                 2009-2010  Software Languages Lab               *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (bft-characterisations)
 (export bft-forest bft-undirected-edge-classification bft-directed-edge-classification bft-node-numbering)
 (import (rnrs base)
         (rnrs io simple)
         (a-d graph-traversing bft)
         (prefix (a-d graph labeled adjacency-matrix) labeled:)
         (a-d graph unweighted config))
 
 (define (bft-forest g)
   (define children (make-vector (order g) '()))
   (define roots '())
   (bft g
        (lambda (root)
          (set! roots (cons root roots)))
        node-nop
        (lambda (from to)
          (vector-set! children from (cons to (vector-ref children from))))
        edge-nop)
   (cons roots children))
 
 (define (bft-node-numbering g)
   (define distances (make-vector (order g) 0))
   (define visit-times (make-vector (order g) 0))
   (define time 0)
   (bft g
        root-nop
        (lambda (node)
          (set! time (+ time 1))
          (vector-set! visit-times node time))
        (lambda (from to)
          (vector-set! distances to (+ 1 (vector-ref distances from))))
        edge-nop)
   (list distances visit-times))
 
 (define (bft-undirected-edge-classification g)
   (define labels (labeled:new #f (order g)))
   (define roots '())
   (bft g
        (lambda (root)
          (set! roots (cons root roots))) 
        node-nop 
        (lambda (from to) 
          (labeled:add-edge! labels from to 'tree))
        (lambda (from to) 
          (if (not (labeled:edge-label labels from to)) ; undirected is crucial!
              (labeled:add-edge! labels from to 'cross))))
   (cons roots labels))
 
 (define (bft-directed-edge-classification g)
   (define labels (labeled:new #t (order g)))
   (define parent-vector (make-vector (order g) '())) ; store and decrement the order of each node
   (define roots '())
   (define (is-parent-of? a b)
     (let ((parent-of-b (vector-ref parent-vector b)))
       (cond ((null? parent-of-b) #f)
             ((= parent-of-b a) #t)
             (else (is-parent-of? a parent-of-b)))))
   (bft g
        (lambda (root) ;root-discovered)
          (set! roots (cons root roots)))
        node-nop ;node discovered
        (lambda (from to) ;edge discovered
          (vector-set! parent-vector to from)
          (labeled:add-edge! labels from to 'tree))
        (lambda (from to) ;edge bumped
          (if (or (= to from) ;self-loop = back edge
                  (is-parent-of? to from))
              (labeled:add-edge! labels from to 'back)
              (labeled:add-edge! labels from to 'cross))))
   (cons roots labels)))