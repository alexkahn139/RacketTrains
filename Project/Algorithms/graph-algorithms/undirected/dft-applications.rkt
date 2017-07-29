#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Undirected DFT Applications                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (dfs-based algorithms)
 (export cyclic? exists-path?)
 (import (rnrs base)
         (a-d graph unweighted config)
         (a-d graph-traversing dft-unweighted))
 
 (define (cyclic? g)
   (define tree (make-vector (order g) '()))
   (define cyclic #f)
   (dft g
        root-nop
        node-nop
        node-nop
        (lambda (from to)
          (vector-set! tree to from))
        edge-nop
        (lambda (from to) 
          (if (not (eq? (vector-ref tree from) to))
              (set! cyclic #t))))
   cyclic)
 
 (define (exists-path? g from to)
   (define encountered #f)
   (dft g
        root-nop
        (lambda (node)
          (if (eq? node to)
              (set! encountered #t))
          (not encountered))
        node-nop
        edge-nop
        edge-nop
        edge-nop
        (list from))
   encountered))