#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Rwm To Graph ADT           ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Try to put the RWM directly in graph, would be more memory efficient

(require "Switch.rkt")
(require "Track.rkt")
(require "Train.rkt")
(require "Node.rkt")
(require "DetectionTrack.rkt")
(require "../Abstractions.rkt")
(require "../a-d/graph/labeled/config.rkt")
(require "../a-d/graph-algorithms/undirected/bft-applications.rkt")
(require "../a-d/graph-traversing/dft-labeled.rkt")

(provide (struct-out rwm)
         load-rwm
         find-railwaypiece
         find-train
         railwaymodel
         all-dt
         all-tracks
         all-pieces
         all-switches
         find-dt
         )

(struct rwm (ls ns node-graph))
; Based on the file given by the asistants
(define number-of-nodes 0)
(define node-dict (make-hash))
(define node-graph #f)

(define (link-nodes-to-graph set)
  (define i 0)
  (hash-for-each
   set
   (lambda (id n)
     (hash-set! node-dict id i)
     (set! i (+ 1 i)))))

(define (load-rwm filename)
  (let ([lines (map string-split (file->lines filename))]
        [ls (make-hash)] ; Hash set is nessecary for the locomotives
        [ns (make-hash)]) ; Hash set is nessecary for the nodes, to connect the real nodes with the graph
    (for-each
     (lambda (l)
       (case (string->symbol (car l))
         [(L) (let* ([id (string->number (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [res (make-train id)])
                (hash-set! ls id res))]
         [(N) (let* ([id (string->symbol (list-ref l 1))]
                     [x (string->number (list-ref l 2))]
                     [y (string->number (list-ref l 3))]
                     [res (make-node id x y)])
                (hash-set! ns id res)
                (set! number-of-nodes (hash-count ns)))
              (set! node-graph (new #f number-of-nodes))
              (link-nodes-to-graph ns)] ; Add the ADT as data structure
         [(S) (let* ([id (string->symbol (list-ref l 1))]
                     [n0 (string->symbol (list-ref l 2))]
                     [n1 (string->symbol (list-ref l 3))]
                     [n2 (string->symbol (list-ref l 4))]
                     [res (make-switch id n0 n1 n2)]
                     [graph-id (hash-ref node-dict id)]
                     [graph-n0 (hash-ref node-dict n0)]
                     [graph-n1 (hash-ref node-dict n1)]
                     [graph-n2 (hash-ref node-dict n2)])
                (label! node-graph graph-id 'id)
                (add-edge! node-graph graph-n0 graph-n1 res)    ; The nodes should be put in the correct form
                (add-edge! node-graph graph-n0 graph-n2 res))]  ; Add the ADT as data structure
         [(T) (let* ([n1 (string->symbol (list-ref l 1))]
                     [n2 (string->symbol (list-ref l 2))]
                     [res (make-track n1 n2)]
                     [graph-n1 (hash-ref node-dict n1)]
                     [graph-n2 (hash-ref node-dict n2)])
                (add-edge! node-graph graph-n1 graph-n2 res))]  ; Add the ADT as data structure
         [(D) (let* ([id (string->number (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [res (make-dt id n1 n2)]
                     [graph-n1 (hash-ref node-dict n1)]
                     [graph-n2 (hash-ref node-dict n2)])
                (add-edge! node-graph graph-n1 graph-n2 res))])) ; Add the ADT as data structure
     lines)
    (rwm ls ns node-graph)))

(define (find-railwaypiece node1 node2)
  (let*
      ((graph-n1 (hash-ref node-dict node1))
       (graph-n2 (hash-ref node-dict node2)))
    (edge-label node-graph graph-n1 graph-n2)))

(define (all-pieces)
  (define tracks '())
  (dft node-graph
       root-nop
       node-nop
       (lambda (from to edge-label)
         (set! tracks (cons edge-label tracks)))
       edge-nop)
  (remove-duplicates tracks))

(define (all-dt)
  (define tracks '())
  (dft node-graph
       root-nop
       node-nop
       node-nop
       edge-nop
       edge-nop
       (lambda (from to edge-label)
         (when (eq? (edge-label 'get-type) 'detection-track)
           (set! tracks (cons edge-label tracks)))))
  (remove-duplicates tracks))

(define (all-switches)
  (define tracks '())
  (dft node-graph
       root-nop
       node-nop
       node-nop
       edge-nop
       edge-nop
       (lambda (from to edge-label)
         (when (eq? (edge-label 'get-type) 'switch)
           (set! tracks (cons edge-label tracks)))))
  (remove-duplicates tracks))

(define (all-tracks)
  (define tracks '())
  (dft node-graph
       root-nop
       node-nop
       node-nop
       edge-nop
       edge-nop
       (lambda (from to edge-label)
         (when (eq? (edge-label 'get-type) 'track)
           (set! tracks (cons edge-label tracks)))))
  (remove-duplicates tracks))

(define (find-train train-id)
  (hash-ref (rwm-ls railwaymodel) train-id))

(define (find-dt id)
  (filter (lambda (dt)
            (eq? (dt 'get-id) id))
          (all-dt))
  )

(define railwaymodel (load-rwm "be_simple.txt"))
