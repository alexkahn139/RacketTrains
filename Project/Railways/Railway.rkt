#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Railway ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in sim: "../Railways/Simulator/interface.rkt"))
(require (prefix-in real: "../Railways/z21-scheme/FullAPI/Z21Socket.rkt"))
(require (prefix-in real: "../Railways/z21-scheme/FullAPI/Z21MessageDriving.rkt"))
(require (prefix-in real: "../Railways/z21-scheme/FullAPI/Z21MessageSwitches.rkt"))
(require (prefix-in real: "../Railways/z21-scheme/FullAPI/Z21MessageLocation.rkt"))
(require (prefix-in real: "../Railways/z21-scheme/APITesting/MessageHandler.rkt"))
(require "../Railways/z21-scheme/FullAPI/racket-bits-utils-master/bits.rkt")

(provide make-railway)

(define (make-railway sim) ; Sim should be a boolean

  (define scale 100)
  (define socket #f)

  (define (startZ21)
    (set! socket (real:setup))
    (real:listen socket real:handle-msg))

  (define (find-correct-id id)
    (define locations real:locations)
    (define (list-loop rest-locations)
      (if (pair? rest-locations)
          (let* ((pair (car rest-locations)))
            (+ (* 8 (- (car pair) 1)) (cdr pair)))
          (list-loop (cdr rest-locations))))
    (if (not (empty? locations))
        (list-loop locations)
        #f))


  (define (set-loco-speed! id speed)
    (if sim
        (sim:set-loco-speed! id speed)
        (let*
            ((lsb (byte->hex-string 3))
             (msb "00")
             (dir (> speed 0))
             (speed (abs (* 100 speed))))
          (real:send socket (real:make-set-loco-drive-msg lsb msb 128 dir speed)))))

  (define (get-locomotive-location id)
    (if sim
        (sim:get-loco-detection-block id)
        (begin
          (real:send socket (real:make-rmbus-get-data-msg "00"))
          (sleep 0.1)
          (find-correct-id id))))

  (define (get-switch-position id)
    (if sim
        (get-switch-position id)
        (let*
            ((lsb (byte->hex-string id))
             (msb "00"))
          (real:make-get-switch-info-msg lsb msb))))

  (define (set-switch-position! id pos)
    (if sim
        (sim:set-switch-position! id pos)
        (let*
            ((lsb (map-switch-id id))
             (msb "01"))
          (if lsb
              (real:send socket (real:make-set-switch-msg lsb msb #t pos))
              (display "!!!SWITCH NOT IN MODELTRACK!!!")))))

  (define (map-switch-id i)
    (define res (assoc i adress-list))
    (if res
        (cadr res)
        #f))

  (define adress-list ; Other switches don't work
    (list
     (list '|1| "00")
     (list '|2| "01")
     (list '|3| "02")
     (list '|4| "03")
     (list '|6| "00")
     (list '|7| "02")
     (list '|9| "00")
     (list '|10| "01")
     (list '|11| "02")
     (list '|12| "03")
     (list '|20| "03")
     (list '|23| "02")
     (list '|25| "00")
     (list '|26| "01")
     (list '|28| "03")))

  (define (dispatch msg)
    (cond
      ((eq? msg 'get-locomotive-location) get-locomotive-location)
      ((eq? msg 'get-switch-position) get-switch-position)
      ((eq? msg 'get-loco-detection-block) get-locomotive-location)

      ((eq? msg 'set-switch-position!) set-switch-position!)
      ((eq? msg 'set-loco-speed!) set-loco-speed!)
      (else (error "Unknown message" msg))))

  ; Initializes the simulator or the Z21
  (if sim
      (sim:start-simulator)
      (startZ21))
  dispatch)
