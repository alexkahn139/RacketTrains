#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Breadth First Traversal                     *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (bft)
 (export bft node-nop edge-nop root-nop)
 (import (rnrs base)
         (rnrs control)
         (prefix (a-d queue linked) q:)
         (a-d graph unweighted config))
 
 (define (bft graph 
              root-discovered
              node-discovered 
              edge-discovered 
              edge-bumped 
              . roots)
   (define visited (make-vector (order graph) #f))
   (define q (q:new))
   (define exit '())
   (define (bft-component root)
     (define (bft-iter from)
       (unless (node-discovered from)
         (exit #t))
       (for-each-edge
        graph
        from
        (lambda (to)
          (if (vector-ref visited to)
              (unless (edge-bumped from to)
                (exit #t))
              (when (edge-discovered from to)
                (vector-set! visited to #t)
                (q:enqueue! q to)))))
       (unless (q:empty? q)
         (bft-iter (q:serve! q))))
     (when (not (vector-ref visited root))
       (vector-set! visited root #t)
       (if (root-discovered root)
           (bft-iter root)
           (exit #t))))
   (call-with-current-continuation
    (lambda (cont)
      (set! exit cont)
      (if (null? roots)
          (for-each-node graph bft-component)
          (for-each bft-component (car roots))))))
 
 (define (root-nop root) #t)
 (define (node-nop node) #t)
 (define (edge-nop from to) #t))