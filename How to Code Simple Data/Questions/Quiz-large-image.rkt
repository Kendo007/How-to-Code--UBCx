;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Quiz-large-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; SOLUTION

; Image, Image -> Boolean
; Check if image1 > image2 (compare area)
(check-expect (large-img? (rectangle 10 20 "solid" "black") (rectangle 15 25 "solid" "black")) false)
(check-expect (large-img? (rectangle 20 30 "solid" "black") (rectangle 15 25 "solid" "black")) true)
(check-expect (large-img? (rectangle 20 30 "solid" "black") (rectangle 20 30 "solid" "black")) false)
(check-expect (large-img? (rectangle 20 30 "solid" "black") (rectangle 20 25 "solid" "black")) true)
(check-expect (large-img? (rectangle 20 30 "solid" "black") (rectangle 15 30 "solid" "black")) true)
(check-expect (large-img? (rectangle 20 30 "solid" "black") (rectangle 20 25 "solid" "black")) true)

;(define (large-img? Img1 Img2)
;  false)

;(define (large-img? Img1 Img2)
;  (... Area1 Area2))

(define (Area Img)
    (* (image-height Img) (image-width Img)))

(define (large-img? Img1 Img2)
  (> (Area Img1) (Area Img2)))