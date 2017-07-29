#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Basic Directed Algorithms                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (basic)
 (export in/out-degrees copy transpose)
 (import (rnrs base)
         (a-d graph unweighted config))
 
 (define (in/out-degrees g)
   (define in-degs (make-vector (order g) 0))
   (define out-degs (make-vector (order g) 0))
   (for-each-node
    g
    (lambda (from)
      (for-each-edge
       g
       from
       (lambda (to)
         (vector-set! out-degs from (+ 1 (vector-ref out-degs from)))
         (vector-set! in-degs to (+ 1 (vector-ref in-degs to)))))))
   (cons in-degs out-degs))
 
 (define (copy g)
   (define cpy (new (directed? g) (order g)))
   (for-each-node
    g
    (lambda (u)
      (for-each-edge
       g
       u
       (lambda (v)
         (add-edge! cpy u v)))))
   cpy)
 
 (define (transpose g)
   (define g~ (new #t (order g)))
   (for-each-node
    g
    (lambda (u)
      (for-each-edge
       g
       u
       (lambda (v)
         (add-edge! g~ v u)))))
   g~))