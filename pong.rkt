;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Pong - G. Graham 9/25/2012

(require 2htdp/image)
(require 2htdp/universe)

;-----------------
; Data Definitions
;-----------------
; A PaddleY is in the range [0, FIELD-HEIGHT)

; A Command is one of
; - "a" move left paddle up
; - "z" move left paddle down
; - "up" move right paddle up
; - "down" move right paddle down

(define-struct paddles (left-y right-y))
; A Paddles is a structure: (make-paddles Number Number)
; interp. (make-paddles left-y right-y) means that the paddle state is
; made up of the y value of the left paddle and the y value of the
; right paddle

;-------------------
; Physical Constants
;-------------------
(define FIELD-WIDTH 800)
(define FIELD-HEIGHT 600)
(define PADDLE-DELTA 20) ;amount paddle moves at a time
(define LEFT-PADDLE-X 70)
(define RIGHT-PADDLE-X (- FIELD-WIDTH LEFT-PADDLE-X))
(define PADDLE-HEIGHT 80)
(define PADDLE-WIDTH 10)

(define INITIAL-PADDLES (make-paddles 100 500))
(define TEST-PADDLES-1 (make-paddles (- 100 PADDLE-DELTA) 500))
(define TEST-PADDLES-2 (make-paddles (+ 100 PADDLE-DELTA) 500))
(define TEST-PADDLES-3 (make-paddles 100 (- 500 PADDLE-DELTA)))
(define TEST-PADDLES-4 (make-paddles 100 (+ 500 PADDLE-DELTA)))
(define TEST-PADDLES-5 (make-paddles 0 500))
(define TEST-PADDLES-6 (make-paddles 100 0))


;--------------------------
; Core Function Definitions
;--------------------------

; Number -> PaddleY
; limit the input value to the range of PaddleY
(check-expect (limit-paddle-y 500) 500)
(check-expect (limit-paddle-y -1) 0)
(check-expect (limit-paddle-y 602) (sub1 FIELD-HEIGHT))

(define (limit-paddle-y y)
  (cond
    [(< y 0) 0]
    [(>= y FIELD-HEIGHT) (sub1 FIELD-HEIGHT)]
    [else y]))


; Paddles Number -> Paddles
; move the left paddle the given amount
(check-expect (move-left-paddle INITIAL-PADDLES PADDLE-DELTA) TEST-PADDLES-2)
(check-expect (move-left-paddle INITIAL-PADDLES -1000) TEST-PADDLES-5)
(define (move-left-paddle s amount)
  (make-paddles (limit-paddle-y (+ (paddles-left-y s) amount))
             (paddles-right-y s)))


; Paddles Number -> Paddles
; move the right paddle the given amount
(check-expect (move-right-paddle INITIAL-PADDLES PADDLE-DELTA) TEST-PADDLES-4)
(check-expect (move-right-paddle INITIAL-PADDLES -1000) TEST-PADDLES-6)
(define (move-right-paddle s amount)
  (make-paddles (paddles-left-y s)
             (limit-paddle-y (+ (paddles-right-y s) amount))))


; Paddles Command -> Paddles
; move the paddle based on the command
(check-expect (move-paddles INITIAL-PADDLES "a") TEST-PADDLES-1)
(check-expect (move-paddles INITIAL-PADDLES "z") TEST-PADDLES-2)
(check-expect (move-paddles INITIAL-PADDLES "up") TEST-PADDLES-3)
(check-expect (move-paddles INITIAL-PADDLES "down") TEST-PADDLES-4)
(check-expect (move-paddles INITIAL-PADDLES "x") INITIAL-PADDLES)

(define (move-paddles s cmd)
  (cond
    [(key=? cmd "a") (move-left-paddle s (- PADDLE-DELTA))]
    [(key=? cmd "z") (move-left-paddle s PADDLE-DELTA)]
    [(key=? cmd "up") (move-right-paddle s (- PADDLE-DELTA))]
    [(key=? cmd "down") (move-right-paddle s PADDLE-DELTA)]
    [else s]))


;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (empty-scene FIELD-WIDTH FIELD-HEIGHT "black"))
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" "white"))


; PaddleY -> Scene
; render the left paddle on the screen
(define (render-paddles s)
  (place-image PADDLE
               LEFT-PADDLE-X (paddles-left-y s) 
               (place-image PADDLE
                            RIGHT-PADDLE-X (paddles-right-y s)
                            FIELD)))


; Create the world
(big-bang INITIAL-PADDLES
          (on-key move-paddles)
          (to-draw render-paddles))