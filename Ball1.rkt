;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ball1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Ball1 - G. Graham 9/17/2012

(requires 2htdp/image 2htdp/universe)

; Data Definitions
(define-struct ball (posn vel))
; Ball is (make-ball Posn Vel)

(define-struct vel (dx dy))
; Vel is (make-vel dx dy)
; interp. dx is the movement amount in the x direction; dy is for the y.

; Physical Constants
(define field-width 800)
(define field-height 600)
