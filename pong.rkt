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

(define-struct game (left-y right-y))
; A Game is a structure: (make-game Number Number)
; interp. (make-game left-y right-y) means that the game state is
; made up of the y value of the left paddle and the y value of the
; right paddle

;-------------------
; Physical Constants
;-------------------
(define FIELD-WIDTH 800)
(define FIELD-HEIGHT 600)
(define PADDLE-DELTA 20) ;amount paddle moves at a time
(define LEFT-PADDLE-X 70)
(define PADDLE-HEIGHT 80)
(define PADDLE-WIDTH 10)
(define INITIAL-STATE (make-game 100 500))
(define TEST-STATE-1 (make-game (- 100 PADDLE-DELTA) 500))
(define TEST-STATE-2 (make-game (+ 100 PADDLE-DELTA) 500))
(define TEST-STATE-3 (make-game 100 (- 500 PADDLE-DELTA)))
(define TEST-STATE-4 (make-game 100 (+ 500 PADDLE-DELTA)))
(define TEST-STATE-5 (make-game 0 500))


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


; Game Number -> Game
; move the left paddle the given amount
(check-expect (move-left-paddle INITIAL-STATE PADDLE-DELTA) TEST-STATE-2)
(check-expect (move-left-paddle INITIAL-STATE -1000) TEST-STATE-5)
(define (move-left-paddle s amount)
  (make-game (limit-paddle-y (+ (game-left-y s) amount))
             (game-right-y s)))

; Game Command -> Game
; move the paddle based on the command
(check-expect (move-paddle INITIAL-STATE "a") TEST-STATE-1)
(check-expect (move-paddle INITIAL-STATE "z") TEST-STATE-2)
(check-expect (move-paddle INITIAL-STATE "up") TEST-STATE-3)
(check-expect (move-paddle INITIAL-STATE "down") TEST-STATE-4)
(check-expect (move-paddle INITIAL-STATE "x") INITIAL-STATE)

(define (move-paddle y cmd)
  (cond
    [(key=? cmd "a") (- y PADDLE-DELTA)]
    [(key=? cmd "z") (+ y PADDLE-DELTA)]
    [else y]))


; PaddleY Command -> PaddleY
; move the paddle while keeping it on the screen
(check-expect (move-paddle-ltd 500 "a") (- 500 PADDLE-DELTA))
(check-expect (move-paddle-ltd 500 "z") (+ 500 PADDLE-DELTA))
(check-expect (move-paddle-ltd 400 "x") 400)
(check-expect (move-paddle-ltd 0 "a") 0)
(check-expect (move-paddle-ltd (sub1 FIELD-HEIGHT) "z") (sub1 FIELD-HEIGHT))

(define (move-paddle-ltd y cmd)
  (limit-paddle-y (move-paddle y cmd)))


;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (empty-scene FIELD-WIDTH FIELD-HEIGHT "black"))
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" "white"))


; PaddleY -> Scene
; render the left paddle on the screen
(define (render-left-paddle y)
  (place-image PADDLE LEFT-PADDLE-X y FIELD))


; Create the world
(big-bang 100
          (on-key move-paddle-ltd)
          (to-draw render-left-paddle))