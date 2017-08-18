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
         calculate-path
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

(define (real-node graph-n)
  (define res #f)
  (hash-for-each
   node-dict
   (lambda (n g)
     (when (eq? g graph-n)
       (set! res n))))
  res)


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
                (label! node-graph graph-id res)
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

(define (calculate-path block-1 block-2) ; block-1 and block-2 are the ID's
  (define (list-from-mcons mlist) ; Changes the mcons list to a normal list
    (define (iterloop m-l l)
      (if (null? m-l)
          (reverse l)
          (iterloop (mcdr m-l) (cons (mcar m-l) l))))
    (iterloop mlist '()))
  (define start-block (car (find-dt block-1))) ; lookup the block object
  (define start-node (start-block 'get-node1))
  (define stop-block (car (find-dt block-2))) ; lookup the block object
  (define stop-node (stop-block 'get-node1))
  (define start-vertex (hash-ref node-dict start-node)) ; Gets the notation in the graph
  (define stop-vertex (hash-ref node-dict stop-node))
  (define schedule (reverse (list-from-mcons (shortest-path node-graph start-vertex stop-vertex))))
  (set! schedule (map real-node schedule)) ; hash-ref is fast if we need to retranslate
  (set! schedule (fix-switches schedule))
  (set! schedule (make-usable-path schedule start-block stop-block))
  (flatten schedule))

(define (make-usable-path path begin end)
  (define node-B   (begin 'get-node2))
  (define node-D   (end 'get-node2))
  (when (or (and (not (= (length path) 1)) (not (eq? (cadr path) node-B)))
            (eq? (car path) node-B))
    (set! path (cons node-B path)))
  (set! path (reverse path))
  (when (not (eq? (cadr path) node-D))
    (set! path (cons node-D path)))
  (reverse path))

;; Fixen van switchen,
;;; Kijken of 2 deel uitmaken van dezelfde switche
;;; Zo nee ID ertussen proppen en dan verder zien

(define (fix-switches schedule)
  (displayln "SWITCHES ARE ")
  ; Different options can cause problems
  ;;; - The path uses the same switch in a row (nB->nA->nC). The solution is to make the train drive another path until a DT
  ;;; - The other problem is when to switches follow each other and the second switch is not detected correctly
  (define (make-detour rest-of-path result-path)
    (define (neighbours rest-of-path result-path)
      (define (dt-loop node1 prev result)
        ;(displayln (find-neighbours node1))
        (define node2 (find-next-neighbour node1 prev (find-neighbours node1)))
        (display "node2 ")(displayln node2)
        (define track (find-railwaypiece node1 node2))
        (set! result (cons node1 result))
        (display "Result is : ")(displayln result)
        (if (eq? 'detection-track (track 'get-type))
 						(list (reverse result) node2 result)
            (dt-loop node2 node1 result)))
      (define node1 (cadr rest-of-path))
      (define prev (car rest-of-path))
      (set! result-path (dt-loop node1 prev '())
                              )
      (displayln result-path)
      result-path)
    (set! result-path (cons (car rest-of-path) result-path)) ; Add nB to the result
    ;(set! result-path (cons (cadr rest-of-path) result-path)) ; Add nA to the result
    ;; Now the detour should be calculated and added
    (displayln (reverse result-path))
    (displayln (neighbours rest-of-path result-path))
    (set! result-path (cons (neighbours rest-of-path result-path) result-path))
    (check-loop (cddr rest-of-path) result-path)
    )
  (define path '())
  (define (check-loop rest-of-path result-path) ; Only if the track could be a switch a test is needed
    (cond
      ((<= 3 (length rest-of-path))
       (if (eq? (find-railwaypiece (car rest-of-path) (cadr rest-of-path))    ; The first option, in this
                (find-railwaypiece (cadr rest-of-path) (caddr rest-of-path))) ; case we have to make a detour
           (make-detour rest-of-path result-path)
           (check-loop (cdr rest-of-path) (cons (car rest-of-path) result-path))))
      ((null? rest-of-path) (reverse result-path))
      (else (check-loop (cdr rest-of-path) (cons (car rest-of-path) result-path)))))
  (check-loop schedule '()))

(define (fix-double-switches schedule)
  (define doubles #f)
  (define (check-loop current-path rest-of-path)
    (if (> 2 (length rest-of-path))
        (if doubles
            (reverse (cons (car rest-of-path) current-path))
            (fix-switches (reverse (cons (car rest-of-path) current-path))))
        (let* ((label (label node-graph (hash-ref node-dict (cadr rest-of-path)))))
          (if (eq? 'no-label label)
              (check-loop (cons (car rest-of-path) current-path) (cdr rest-of-path))
              (let* ((switch label)
                     (id1 ((find-railwaypiece (car rest-of-path) (cadr rest-of-path)) 'get-id))
                     (id2 ((find-railwaypiece (cadr rest-of-path) (caddr rest-of-path)) 'get-id))
                     (n1 (switch 'get-node1))
                     )
                (if (and (not (eq? id1 n1))
                         (not (eq? id2 n1)))
                    (fix-the-path id1 id2 n1 current-path rest-of-path)
                    (check-loop (cons (car rest-of-path) current-path) (cdr rest-of-path))))))))
  (define (fix-the-path id1 id2 n1 current-path rest-of-path)
    ;(set! doubles #t)
    (set! current-path (cons (car rest-of-path) current-path)) ;A2
    (set! current-path (cons (cadr rest-of-path) current-path)) ;Mx ; Has to stay for setting tth switch
    (set! current-path (cons id1 current-path)) ;A1
    (set! current-path (cons n1 current-path)) ;Bx
    (set! current-path (cons (cadr rest-of-path) current-path)) ;Mx
    (define neighbours (find-neighbours (cadr rest-of-path)))
    (define neighbour (correct-neigbour (cadr rest-of-path) n1 neighbours))
    (set! current-path (cons neighbour current-path)) ;Nm
    (set! neighbours (find-neighbours neighbour))
    (set! neighbours (remove neighbour neighbours))
    (set! current-path (cons (car neighbours) current-path)) ;Lg
    (set! current-path (cons neighbour current-path)) ;Nm
    (set! current-path (cons (cadr rest-of-path) current-path)) ;Mx
    (set! current-path (cons n1 current-path)) ;Bx
    (set! current-path (cons id2 current-path)) ;Gt
    (set! current-path (cons (cadr rest-of-path) current-path)) ;Mx
    (set! current-path (cons (caddr rest-of-path) current-path)) ;Bg
    (check-loop current-path (cddr rest-of-path)))
  (check-loop '() schedule))



(define (find-neighbours node)
  (set! node (hash-ref node-dict node))
  (define neighbours '())
  (for-each-edge node-graph node (lambda (edge-to label)
                                   (set! neighbours (cons (real-node edge-to) neighbours))))
  neighbours)

(define (correct-neigbour node id neighbours)
  (define (loop rem-neighbours)
    (if (eq? ((find-railwaypiece node (car rem-neighbours)) 'get-id) id)
        (car rem-neighbours)
        (loop (cdr rem-neighbours))))
  (loop neighbours))

(define (find-next-neighbour node switch-node neighbours)
  (define switch (find-railwaypiece node switch-node))
  (define (loop rem-neighbours)
    (when (not (null? rem-neighbours))
               (if (eq? switch (find-railwaypiece node (car rem-neighbours)))
                   (loop (cdr rem-neighbours))
                   (car rem-neighbours))))
    (loop neighbours))

  (define railwaymodel (load-rwm "Railway.txt"))
