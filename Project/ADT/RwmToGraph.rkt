#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Rwm To Graph ADT           ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Abstractions.rkt")
(require "../a-d/graph/unweighted/config.rkt")
(require "../a-d/graph-algorithms/undirected/bft-applications.rkt")

(provide make-rwm-to-graph)


(define (make-rwm-to-graph)
  (define number-of-nodes (hash-count (rwm-ns railwaymodel)))
  (define node-graph (new #f number-of-nodes))

  ; Each node should in the graph should be linked with a real node
  ;; Best done with dictionary
  (define node-dict (make-hash))

  (define (link-nodes-to-graph)
    (define i 0)
    (hash-for-each
     (rwm-ns railwaymodel)
     (lambda (id n)
       (hash-set! node-dict id i)
       (set! i (+ 1 i)))))

  (define (real-node graph-n)
    (define res #f)
    (hash-for-each
     node-dict
     (lambda (n g)
       (when (eq? g graph-n)
         (set! res n)))
     res))

  ; The edges of the graph should represent the ways between the edges
  ;; For the calculation of a path, it shouldn't matter if the edge is a track,
  ;; detection track or switch. Each edge should be between to nodes

  (define (build-graph)
    (define (add-track! node1 node2)
      (add-edge! node-graph node1 node2))
    (link-nodes-to-graph)
    (hash-for-each
     (rwm-dt railwaymodel)
     (lambda (id dt)
       (let*
           ((node1 (hash-ref node-dict (dt 'get-node1)))
            (node2 (hash-ref node-dict (dt 'get-node2))))
         (add-track! node1 node2))))
    (hash-for-each
     (rwm-ss railwaymodel)
     (lambda (id ss)
       (let*
           ((node1 (hash-ref node-dict (ss 'get-node1)))
            (node2 (hash-ref node-dict (ss 'get-node2)))
            (node3 (hash-ref node-dict (ss 'get-node3))))
         (add-track! node1 node2)
         (add-track! node1 node3))))

    (for-each
     (lambda (track)
       (let*
           ((node1 (hash-ref node-dict (track 'get-node1)))
            (node2 (hash-ref node-dict (track 'get-node2))))
         (add-track! node1 node2)))
     (rwm-ts railwaymodel))
    )

  (define (calculate-path block-1 block-2)
    (define (list-from-mcons mlist list)
      (if (null? mlist)
          (reverse list)
          (list-from-mcons (mcdr mlist) (cons (mcar mlist) list))))
    (define start-node ((hash-ref (rwm-dt railwaymodel) block-1) 'get-node1))
    (define stop-node ((hash-ref (rwm-dt railwaymodel) block-2) 'get-node1))
    (define start-vertex (hash-ref node-dict start-node))
    (define stop-vertex (hash-ref node-dict stop-node))
    (map real-node (list-from-mcons (shortest-path node-graph start-vertex stop-vertex) '())))
 ; Calculates the path, changes it to a list en finally, returns the list with the number-of-nodes

	(build-graph)
  (define (dispatch msg)
    (cond
      ;((eq? msg 'build-graph) build-graph)
      ((eq? msg 'calculate-path) calculate-path)
      (else (error "Unknown message"))
      ))
  dispatch)
