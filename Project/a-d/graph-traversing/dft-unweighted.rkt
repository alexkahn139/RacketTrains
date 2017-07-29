#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Depth First Traversal (version: unweighted)           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (dft-unweighted)
 (export dft node-nop root-nop edge-nop)
 (import (rnrs base)
         (rnrs control)
         (a-d graph unweighted config))
 
 (define (dft graph
              root-discovered
              node-discovered
              node-processed
              edge-discovered
              edge-processed
              edge-bumped
              . roots)
   (define visited (make-vector (order graph) #f))
   (define exit '())
   (define (dft-component root)
     (define (dft-rec from)
       (unless (node-discovered from)
         (exit #f))
       (vector-set! visited from #t)
       (for-each-edge
        graph
        from
        (lambda (to)
          (if (vector-ref visited to)
              (unless (edge-bumped from to)
                (exit #f))
              (unless (and (edge-discovered from to)
                           (dft-rec to)
                           (edge-processed from to))
                (exit #f)))))
       (unless (node-processed from)
         (exit from)))
     (when (not (vector-ref visited root))
       (if (root-discovered root)
           (dft-rec root)
           (exit #f))))
   (call-with-current-continuation
    (lambda (cont)
      (set! exit cont)
      (if (null? roots)
          (for-each-node graph dft-component)
          (for-each dft-component (car roots))))))

 (define (root-nop root) #t)
 (define (node-nop node) #t)
 (define (edge-nop from to) #t))