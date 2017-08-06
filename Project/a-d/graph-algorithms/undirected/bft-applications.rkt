#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Undirected BFS Applications                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (basic algorithms)
 (export shortest-path distance)
 (import (rnrs base)
         (a-d graph labeled config)
         (a-d graph-traversing bft-labeled))

 (define (shortest-path g from to)
   (define paths (make-vector (order g) '()))
   (vector-set! paths from (list from))
   (bft g
        root-nop
        (lambda (node label) ; Needed changes to work with labels
          (not (eq? node to)))
        (lambda (from to edge-label) ; Needed changes to work with labels
          (vector-set! paths to (cons to (vector-ref paths from))))
        edge-nop
        (list from))
   (vector-ref paths to))

 (define (distance g from to)
   (define distances (make-vector (order g) +inf.0))
   (vector-set! distances from 0)
   (bft g
        root-nop
        (lambda (node)
          (not (eq? node to)))
        (lambda (from to)
          (vector-set! distances to (+ (vector-ref distances from) 1)))
        edge-nop
        (list from))
   (vector-ref distances to)))
