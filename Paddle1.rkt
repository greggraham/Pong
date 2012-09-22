;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Paddle1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Paddle1 - G. Graham 9/21/2012

(require 2htdp/image)
(require 2htdp/universe)

; Physical Constants
(define FIELD-WIDTH 800)
(define FIELD-HEIGHT 600)
(define PADDLE-DELTA 20) ;amount paddle move at a time
(define LEFT-PADDLE-X 70)
(define PADDLE-HEIGHT 80)
(define PADDLE-WIDTH 10)

; Data Definitions
; A PaddleY is in the range [0, FIELD-HEIGHT)

; A Command is one of
; - "a" move left paddle up
; - "z" move left paddle down


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


; PaddleY Command -> PaddleY
; move the paddle based on the command
(check-expect (move-paddle 500 "a") (- 500 PADDLE-DELTA))
(check-expect (move-paddle 500 "z") (+ 500 PADDLE-DELTA))
(check-expect (move-paddle 0 "a") 0)
(check-expect (move-paddle (sub1 FIELD-HEIGHT) "z") (sub1 FIELD-HEIGHT))
(check-expect (move-paddle 400 "x") 400)

(define (move-paddle y cmd)
  (limit-paddle-y (cond
    [(key=? cmd "a") (- y PADDLE-DELTA)]
    [(key=? cmd "z") (+ y PADDLE-DELTA)]
    [else y])))


; Graphical Constants
(define FIELD (empty-scene FIELD-WIDTH FIELD-HEIGHT "black"))
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" "white"))


; PaddleY -> Scene
; render the left paddle on the screen
(define (render-left-paddle y)
  (place-image PADDLE LEFT-PADDLE-X y FIELD))


; Create the world
(big-bang 100
          (on-key move-paddle)
          (to-draw render-left-paddle))