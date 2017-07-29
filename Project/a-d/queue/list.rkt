#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Queues (Slow Positional List Implementation)          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library 
 (queue)
 (export new queue? enqueue! serve! peek full? empty?)
 (import (prefix (a-d positional-list adt) plist:)
         (srfi :9)
         (except (rnrs base) length map for-each ))
 
 (define-record-type queue
   (make p)
   queue?
   (p plist))
 
 (define (new)
   (make (plist:new eq?)))
 
 (define (enqueue! q val)
   (define plst (plist q))
   (if (full? q)
     (error "full queue (enqueue!)" q)
     (plist:add-before! plst val)))
 
 (define (peek q)
   (define plst (plist q))
   (if (= (plist:length plst) 0)
     (error "empty queue (peek)" q))
   (plist:peek plst (plist:last plst)))
 
 (define (serve! q)
   (define plst (plist q))
   (define last-position (plist:last plst))
   (if (plist:empty? plst)
     (error "queue empty (pop)" q))
   (let ((val (plist:peek plst last-position)))
     (plist:delete! plst last-position)
     val))
 
 (define (empty? q)
   (plist:empty? (plist q)))
 
 (define (full? q)
   (plist:full? (plist q))))