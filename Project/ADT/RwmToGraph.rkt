#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Rwm To Graph ADT           ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Abstractions.rkt")
(require (prefix-in graph: "../a-d/graph/unweighted/config.rkt"))

(provide make-rwm-to-graph)


(define (make-rwm-to-graph)
  (define number-of-nodes (hash-count (rwm-ns railwaymodel)))
  (define node-graph (graph:new #f number-of-nodes))

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

  (define (find-node graph-n)
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
      (graph:add-edge! node-graph node1 node2))
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
            (node3 (hash-ref node-dict (ss 'get-node3)))
            (id (hash-ref node-dict (ss 'get-id))))
         (add-track! node1 id)
         (add-track! node2 id)
         (add-track! node3 id))))

    (for-each
     (lambda (track)
       (let*
           ((node1 (hash-ref node-dict (track 'get-node1)))
            (node2 (hash-ref node-dict (track 'get-node2))))
         (add-track! node1 node2)))
       (rwm-ts railwaymodel))
    )

  (define (dispatch msg)
    (cond
      ((eq? msg 'build-graph) build-graph)
      
      (else (error "Unknown message"))
      ))
  dispatch)

;(define gg (make-rwm-to-graph))
;((gg 'build-graph))
