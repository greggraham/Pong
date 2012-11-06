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

(define-struct vel (dx dy))
; A Velocity is a structure: (make-vel Number Number)
; interp. (make-vel dx dy) means that the velocity is made up of
; a delta x and delta y value indicating the number of pixels
; to move in the x and y directions for each frame.

(define-struct ball (p v))
; A Ball is a structure: (make-ball Posn Vel)
; interp. (make-ball p v) means that the ball state is made up of
; its position (Posn) and its velocity (Vel).

(define-struct game (ball paddles))
; A Game is a structure: (make-game Ball Paddles)
; interp. (make-ball ball paddles) means that the state of the game
; is made up of the state of the ball and the state of the paddles


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
(define BALL-RADIUS 5)

(define INITIAL-PADDLES (make-paddles 100 500))
(define TEST-PADDLES-1 (make-paddles (- 100 PADDLE-DELTA) 500))
(define TEST-PADDLES-2 (make-paddles (+ 100 PADDLE-DELTA) 500))
(define TEST-PADDLES-3 (make-paddles 100 (- 500 PADDLE-DELTA)))
(define TEST-PADDLES-4 (make-paddles 100 (+ 500 PADDLE-DELTA)))
(define TEST-PADDLES-5 (make-paddles 0 500))
(define TEST-PADDLES-6 (make-paddles 100 0))

(define INITIAL-BALL (make-ball (make-posn 0 0) (make-vel 5 2)))
(define TEST-BALL-1 (make-ball (make-posn 5 2) (make-vel 5 2)))
(define INITIAL-GAME (make-game INITIAL-BALL INITIAL-PADDLES))
(define TEST-GAME-1 (make-game INITIAL-BALL TEST-PADDLES-1))
(define TEST-GAME-2 (make-game TEST-BALL-1 INITIAL-PADDLES))

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


; Game Command -> Game
; change the game state based on the command
(check-expect (control-game INITIAL-GAME "a") TEST-GAME-1)

(define (control-game g cmd)
  (make-game (game-ball g) (move-paddles (game-paddles g) cmd)))


; Posn Vel -> Posn
; Compute a new position by applying the given velocity
(check-expect (translate (make-posn 10 20) (make-vel 3 5)) (make-posn 13 25))

(define (translate p v)
  (make-posn (+ (posn-x p) (vel-dx v)) (+ (posn-y p) (vel-dy v))))


; Ball -> Ball
; Change the position of the ball according to its current velocity
(check-expect (update-posn INITIAL-BALL) TEST-BALL-1)

(define (update-posn b)
  (make-ball (translate (ball-p b) (ball-v b)) (ball-v b)))


; Game->Game
; Update the game state by moving the ball
(check-expect (move-ball INITIAL-GAME) TEST-GAME-2)

(define (move-ball g) 
  (make-game (update-posn (game-ball g)) (game-paddles g)))


;------------------
; Display Rendering
;------------------

; Graphical Constants
(define FIELD (rectangle FIELD-WIDTH FIELD-HEIGHT "solid" "black"))
(define PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" "white"))
(define BALL (circle BALL-RADIUS "solid" "white"))


; Paddles -> Scene
; render both paddles on the screen
(define (render-paddles s)
  (place-image PADDLE
               LEFT-PADDLE-X (paddles-left-y s) 
               (place-image PADDLE
                            RIGHT-PADDLE-X (paddles-right-y s)
                            FIELD)))


; Game -> Scene
; render the entire game state
(define (render-game g)
  (place-image BALL
               (posn-x (ball-p (game-ball g)))
               (posn-y (ball-p (game-ball g)))
               (render-paddles (game-paddles g))))


; Create the world
(big-bang INITIAL-GAME
          (on-tick move-ball)
          (on-key control-game)
          (to-draw render-game))