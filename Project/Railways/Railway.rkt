#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Railway ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in sim: "simulator/interface.rkt"))
(require (prefix-in real: "z21-scheme/FullAPI/Z21Socket.rkt"))
(require (prefix-in real: "z21-scheme/FullAPI/Z21MessageDriving.rkt"))
(require (prefix-in real: "z21-scheme/FullAPI/Z21MessageSwitches.rkt"))
(require (prefix-in real: "z21-scheme/FullAPI/Z21MessageLocation.rkt"))
(require (prefix-in real: "z21-scheme/APITesting/MessageHandler.rkt"))
(require "z21-scheme/FullAPI/racket-bits-utils-master/bits.rkt")

(provide make-railway)

(define (make-railway sim) ; Sim should be a boolean

	(define scale 10)
	(define socket #f)

	(define (startZ21)
		(set! socket (real:setup))
		(real:listen socket real:handle-msg))

	(define (find-correct-id id msg)
		(display msg))


	(define (set-loco-speed! id speed)
		(if sim
				(sim:set-loco-speed! id speed)
				(let*
					((lsb (byte->hex-string id))
					 (msb "OO")
					 (dir (> speed 0))
					 (speed (abs (* scale speed))))
				 (real:send socket (real:make-set-loco-drive-msg lsb msb 128 dir speed)))))

	(define (get-locomotive-location id)
		(if sim
				(sim:get-loco-detection-block id)
				(find-correct-id id (real:send socket (real:make-rmbus-get-data-msg "00")))))

	(define (get-switch-position id)
		(if sim
				(get-switch-position id)
				(let*
					((lsb (byte->hex-string id))
					 (msb "OO"))
				 (real:make-get-switch-info-msg lsb msb))))

	(define (set-switch-position! id pos)
		(if sim
				(sim:set-switch-position! id pos)
				(let*
					((lsb (byte->hex-string id))
					 (msb "O1"))
				  (real:send socket (real:make-set-switch-msg lsb msb #t pos)))))

	(define (dispatch msg)
		(cond
			((eq? msg 'get-locomotive-location) get-locomotive-location)
			((eq? msg 'get-switch-position) get-switch-position)
			((eq? msg 'get-loco-detection-block) get-locomotive-location)

			((eq? msg 'set-switch-position!) set-switch-position!)
			((eq? msg 'set-loco-speed!) set-loco-speed!)
			(else (error "Unknown message" msg))
		))
	(if sim
		(sim:start-simulator)
		(startZ21))
	dispatch)
