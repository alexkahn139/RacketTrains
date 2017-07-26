#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             NMBS ADT                 ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "../ADT/RailwayModel.rkt")
(require "../Simulator/interface.rkt")
(require "../Abstractions.rkt")

(provide make-nmbs)

  (define (make-nmbs)

  (define (dispatch msg)
    (cond
      (else (error "Unkown Message"))))
  dispatch)
