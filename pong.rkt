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
(define-struct post-score (timer left))
; A Ball is one of
; - a structure: (make-ball Posn Vel)
;   interp. (make-ball p v) means that the ball state is made up of
;   its position (Posn) and its velocity (Vel).
; - a structure: (make-post-score Number Boolean)
;   interp. (make-post-score timer left) means that instead of displaying
;   the ball, the score should be displayed until the timer value reaches 0.
;   Left is true if the left player just scored, and false if the right
;   player just scored.

(define-struct game (ball paddles l-score r-score))
; A Game is a structure: (make-game Ball Paddles Number Number)
; interp. (make-ball ball paddles l-score r-score) means that the
; state of the game is made up of the state of the ball, the state
; of the paddles, and the scores of the two players.


;-------------------
; Physical Constants
;-------------------
(define FIELD-WIDTH 800)
(define FIELD-HEIGHT 600)
(define PADDLE-DELTA 20) ;amount paddle moves at a time
(define LEFT-PADDLE-X 70)
(define RIGHT-PADDLE-X (- FIELD-WIDTH LEFT-PADDLE-X))
(define TO-LEFT -1)
(define TO-RIGHT 1)
(define PADDLE-HEIGHT 80)
(define PADDLE-WIDTH 10)
(define BALL-RADIUS 5)
(define TIMER-VALUE 65)

(define INITIAL-PADDLES (make-paddles 100 500))
(define TEST-PADDLES-1 (make-paddles (- 100 PADDLE-DELTA) 500))
(define TEST-PADDLES-2 (make-paddles (+ 100 PADDLE-DELTA) 500))
(define TEST-PADDLES-3 (make-paddles 100 (- 500 PADDLE-DELTA)))
(define TEST-PADDLES-4 (make-paddles 100 (+ 500 PADDLE-DELTA)))
(define TEST-PADDLES-5 (make-paddles 0 500))
(define TEST-PADDLES-6 (make-paddles 100 0))

(define INITIAL-BALL (make-ball (make-posn 0 0) (make-vel 7 3)))
(define TEST-BALL-1 (make-ball (make-posn 7 3) (make-vel 7 3)))
(define TEST-BALL-2 (make-ball (make-posn 0 50) (make-vel 3 -10)))
(define INITIAL-GAME (make-game INITIAL-BALL INITIAL-PADDLES 0 0))
(define TEST-GAME-1 (make-game INITIAL-BALL TEST-PADDLES-1 0 0))
(define TEST-GAME-2 (make-game TEST-BALL-1 INITIAL-PADDLES 0 0))

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
  (make-game (game-ball g) (move-paddles (game-paddles g) cmd) (game-l-score g) (game-r-score g)))


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


; Ball -> Boolean
; Check to see if the ball is going off of the top of the screen
(check-expect (off-top (make-ball (make-posn 50 0) (make-vel -5 -3))) true)
(check-expect (off-top (make-ball (make-posn 50 10) (make-vel -5 -3))) false)
(check-expect (off-top (make-ball (make-posn 50 0) (make-vel -5 3))) false)

(define (off-top b)
  (and (< (vel-dy (ball-v b)) 0) (<= (posn-y (ball-p b)) BALL-RADIUS)))


; Ball -> Boolean
; Check to see if the ball is going off of the bottom of the screen
(check-expect (off-bottom (make-ball (make-posn 50 FIELD-HEIGHT) (make-vel 5 3))) true)
(check-expect (off-bottom (make-ball (make-posn 50 (- FIELD-HEIGHT 10)) (make-vel 5 3))) false)
(check-expect (off-bottom (make-ball (make-posn 50 FIELD-HEIGHT) (make-vel 5 -3))) false)

(define (off-bottom b)
  (and (> (vel-dy (ball-v b)) 0) (>= (posn-y (ball-p b)) (- FIELD-HEIGHT BALL-RADIUS))))


; Number -> Number
; Return -1 for negative numbers, 1 for positive numbers, and 0 for 0.
(check-expect (sign -5) -1)
(check-expect (sign 10) 1)
(check-expect (sign 0) 0)

(define (sign n)
  (cond
    [(< n 0) -1]
    [(> n 0) 1]
    [else 0]))


; Number Number -> Boolean
; Compare the y values of the ball and paddle to see if they are within range
; for a collision.
(check-expect (within-vertical-range 100 101) true)
(check-expect (within-vertical-range 100 1) false)

(define (within-vertical-range by py)
  (<= (abs (- by py))
      (+ BALL-RADIUS (/ PADDLE-HEIGHT 2))))


; Number Number -> Boolean
; Compare the x values of the ball and paddle to see if they are within range
; for a collision.
(check-expect (within-horizontal-range 100 101) true)
(check-expect (within-horizontal-range 100 1) false)

(define (within-horizontal-range bx px)
  (<= (abs (- bx px))
      (+ BALL-RADIUS (/ PADDLE-WIDTH 2))))


; Ball Paddles -> Boolean
; Check to see if the ball collided with the left paddle
(check-expect (collide-paddle (make-ball (make-posn RIGHT-PADDLE-X (paddles-right-y INITIAL-PADDLES))
                                         (make-vel 5 -2))
                              RIGHT-PADDLE-X
                              (paddles-right-y INITIAL-PADDLES)
                              TO-RIGHT)
              true)

(define (collide-paddle b px py dir)
  (and (= (sign (vel-dx (ball-v b))) dir)
       (within-vertical-range (posn-y (ball-p b)) py)
       (within-horizontal-range (posn-x (ball-p b)) px)))


; Ball Paddles -> Ball
; Update the velocity of the ball in the case of a collision
(check-expect (detect-collision (make-ball (make-posn 100 0)
                                           (make-vel 5 -2))
                                INITIAL-PADDLES)
              (make-ball (make-posn 100 0) (make-vel 5 2)))
(check-expect (detect-collision (make-ball (make-posn RIGHT-PADDLE-X (paddles-right-y INITIAL-PADDLES))
                                           (make-vel 5 -2))
                                INITIAL-PADDLES)
              (make-ball (make-posn RIGHT-PADDLE-X (paddles-right-y INITIAL-PADDLES))
                         (make-vel -5 -2)))
(check-expect (detect-collision (make-ball (make-posn 300 300)
                                           (make-vel 5 -2))
                                INITIAL-PADDLES)
              (make-ball (make-posn 300 300)
                         (make-vel 5 -2)))

(define (detect-collision b p)
  (cond
    [(or (off-top b) (off-bottom b))
     (make-ball (ball-p b) (make-vel (vel-dx (ball-v b)) (- (vel-dy (ball-v b)))))]
    [(or (collide-paddle b LEFT-PADDLE-X (paddles-left-y p) TO-LEFT)
         (collide-paddle b RIGHT-PADDLE-X (paddles-right-y p) TO-RIGHT))
     (make-ball (ball-p b) (make-vel (- (vel-dx (ball-v b))) (vel-dy (ball-v b))))]
    [else b]))


; Boolean -> Ball
; Generate a random ball
(define (random-ball left)
  (make-ball
   (make-posn (if left 0 FIELD-WIDTH) (random FIELD-HEIGHT))
   (make-vel (if left 7 -7) (- (random 7) 3))))


; Ball -> Ball
; Process post-score representation of ball by either counting down the timer or
; creating a new ball when the timer expires.
(check-expect (process-post-score (make-post-score 100 true))
              (make-post-score 99 true))

(define (process-post-score b)
  (if (> (post-score-timer b) 0)
      (make-post-score (- (post-score-timer b) 1) (post-score-left b))
      (random-ball (post-score-left b))))


; Ball -> Boolean
; Check to see if the left player scored.
(check-expect (left-scored? (make-ball (make-posn FIELD-WIDTH 50) (make-vel 7 3))) true)
(check-expect (left-scored? (make-ball (make-posn FIELD-WIDTH 50) (make-vel -7 3))) false)
(check-expect (left-scored? (make-ball (make-posn 100 50) (make-vel 7 3))) false)

(define (left-scored? b)
  (and (ball? b)
       (>= (posn-x (ball-p b)) FIELD-WIDTH)
       (> (vel-dx (ball-v b)) 0)))


; Ball -> Boolean
; Check to see if the right player scored.
(check-expect (right-scored? (make-ball (make-posn -1 50) (make-vel 7 3))) false)
(check-expect (right-scored? (make-ball (make-posn -1 50) (make-vel -7 3))) true)
(check-expect (right-scored? (make-ball (make-posn 100 50) (make-vel 7 3))) false)

(define (right-scored? b)
  (and (ball? b)
       (< (posn-x (ball-p b)) 0)
       (< (vel-dx (ball-v b)) 0)))


; Game -> Game
; Update the game state each frame
(check-expect (update INITIAL-GAME) TEST-GAME-2)

(define (update g)
  (cond
    [(left-scored? (game-ball g))
     (make-game (make-post-score TIMER-VALUE true)
                (game-paddles g)
                (add1 (game-l-score g))
                (game-r-score g))]
    [(right-scored? (game-ball g))
     (make-game (make-post-score TIMER-VALUE false)
                (game-paddles g)
                (game-l-score g)
                (add1 (game-r-score g)))]
    [ else (make-game (if (post-score? (game-ball g))
                          (process-post-score (game-ball g))
                          (update-posn (detect-collision (game-ball g) (game-paddles g))))
                      (game-paddles g)
                      (game-l-score g) (game-r-score g))]))


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

; Number Number -> Image
(define (render-score ls rs)
  (text (string-append (number->string ls) "  :  " (number->string rs))
        50 "white"))

; Game -> Scene
; render the entire game state
(define (render-game g)
  (cond
    [(post-score? (game-ball g))
     (place-image (render-score (game-l-score g) (game-r-score g))
                  (/ FIELD-WIDTH 2) (/ FIELD-HEIGHT 4)
                  (render-paddles (game-paddles g)))]
    [else (place-image BALL
                       (posn-x (ball-p (game-ball g)))
                       (posn-y (ball-p (game-ball g)))
                       (render-paddles (game-paddles g)))]))


; Create the world
(big-bang INITIAL-GAME
          (on-tick update)
          (on-key control-game)
          (to-draw render-game))
