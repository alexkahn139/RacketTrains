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
         (set! res n))))
    res)

  ; The edges of the graph should represent the ways between the edges
  ;; For the calculation of a path, it shouldn't matter if the edge is a track,
  ;; detection track or switch. Each edge should be between to nodes

  (define (add-track! node1 node2)
    (add-edge! node-graph node1 node2))

  (define (delete-track! node1 node2)
    (delete-edge! node-graph node1 node2))

  (define (build-graph)
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
            )
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

  (define (fix-switches path block1 block2)
    (display "Attempt to switch")
    (define correct-path path)
    (define (fix-switch rest-of-path)
      ; Delete the nB and nC, recalculate the shortest path and readd the deleted edge
      (displayln "fixing")
      (define node1 (car rest-of-path))
      (set! node1 (hash-ref node-dict node1))
      (define node2 (cadr rest-of-path))
      (set! node2 (hash-ref node-dict node2))
      (delete-track! node1 node2)
      (displayln "deleted")
      (set! correct-path (calculate-path block1 block2))
      (displayln correct-path)
      (add-track! node1 node2))
    (define (check-loop rest-of-path) ; Only if the track could be a switch a test is needed
      (when (<= 3 (length rest-of-path))
        (displayln "In the when")
        (if (eqv? (find-railwaypiece (car rest-of-path) (cadr rest-of-path)) (find-railwaypiece (cadr rest-of-path) (caddr rest-of-path)))
            (fix-switch rest-of-path)
            (check-loop (cdr rest-of-path)))))
    (check-loop path)
    (display "correct-path ")(displayln correct-path)
    correct-path)

  (define (calculate-path block-1 block-2)
    (define (list-from-mcons mlist)
      (define (iterloop m-l l)
        (if (null? m-l)
            (reverse l)
            (iterloop (mcdr m-l) (cons (mcar m-l) l))))
      (iterloop mlist '()))
    (define start-block (hash-ref (rwm-dt railwaymodel) block-1))
    (define start-node (start-block 'get-node1))
    (define stop-block (hash-ref (rwm-dt railwaymodel) block-2))
    (define stop-node (stop-block 'get-node1))
    (define start-vertex (hash-ref node-dict start-node))
    (define stop-vertex (hash-ref node-dict stop-node))
    (define schedule (reverse (map real-node (list-from-mcons (shortest-path node-graph start-vertex stop-vertex)))))
    (displayln schedule)
    (set! schedule (make-usable-path schedule start-block stop-block))
    (displayln schedule)
    (set! schedule (fix-switches schedule block-1 block-2))
    (displayln schedule)
    schedule)

  (define (make-usable-path path begin end)
    (display "usablle ") (displayln path)
    (define node-B   (begin 'get-node2))
    (define node-D   (end 'get-node2))
    (when (or (and (not (= (length path) 1)) (not (eq? (cadr path) node-B)))
              (not (eq? (car path) node-B)))
      (set! path (cons node-B path)))
    (set! path (reverse path))
    (when (not (eq? (cadr path) node-D))
      (set! path (cons node-D path)))
    (reverse path))
  ; Calculates the path, changes it to a list en finally, returns the list with the number-of-nodes

  (build-graph)
  (define (dispatch msg)
    (cond
      ;((eq? msg 'build-graph) build-graph)
      ((eq? msg 'calculate-path) calculate-path)
      (else (error "Unknown message"))
      ))
  dispatch)
