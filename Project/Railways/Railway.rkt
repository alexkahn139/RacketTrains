#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Railway ADT               ;;
;; Copyright 2017 Alexandre Kahn 2BA CW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in sim: "simulator/interface.rkt"))
(require (prefix-in real: "z21/FullAPI/Z21Socket.rkt"))
(require (prefix-in real: "z21/FullAPI/Z21MessageDriving.rkt"))
(require (prefix-in real: "z21/FullAPI/Z21MessageSwitches.rkt"))
(require (prefix-in real: "z21/FullAPI/Z21MessageLocation.rkt"))
(require "z21/FullAPI/racket-bits-utils-master/bits.rkt")

(provide make-railway)

(define (make-railway sim) ; Sim should be a boolean

	(define scale 10)
	(define socket #f)

	(define (startZ21)
		(set! socket (Z21:setup))
		(Z21:listen socket Z21:handle-msg))

	(define (find-correct-id id msg)
		(display msg))


	(define (set-loco-speed! id speed)
		(if sim
				(sim:set-loco-speed! id speed)
				(let*
					((lsb (byte->hex-string id))
					 (msb "OO")
					 (dir (> speed 0))
					 (speed (abs (* scale speed)))
				 (Z21:send socket (Z21:make-set-loco-drive-msg lsb msb Z21:high-speed dir speed))))))

	(define (get-locomotive-location id)
		(if sim
				(sim:get-loco-detection-block id)
				(find-correct-id id (Z21:send socket (Z21:make-rmbus-get-data-msg "00")))))

	(define (get-switch-position id)
		(if sim
				(get-switch-position id)
				(let*
					((lsb (byte->hex-string id))
					 (msb "OO"))
				 (Z21:make-get-switch-info-msg lsb msb))))

	(define (set-switch-position! id)
		(if sim
				(sim:set-switch-position! id new-pos)
				(let*
					((lsb (byte->hex-string id))
					 (msb "O1"))
				  (Z21:send socket (Z21:make-set-switch-msg lsb msb #t pos)))))

	(define (dispatch msg)
		(cond
			((eq? msg 'get-locomotive-location) get-locomotive-location)
			((eq? msg 'get-switch-position) get-switch-position)

			((eq? msg 'set-switch-position!) set-switch-position!)
			(else (error "Unknown message"))
		))
	(if sim
		(sim:start-simulator)
		(startZ21))
	dispatch)
