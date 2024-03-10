;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Medium-htdf-boxify-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; boxify-starter.rkt


; SOLUTION

; Image -> Image
; Puts box around the given image by overlaying image on it
(check-expect (boxify (ellipse 60 30 "solid" "red"))
              (overlay (ellipse 60 30 "solid" "red")
                       (rectangle 62 32 "outline" "black")))
(check-expect (boxify (star 40 "solid" "gray")) 
              (overlay (rectangle 67 64 "outline" "black")
                       (star 40 "solid" "gray")))

;(define (boxify img)          ; stub
;  (rectangle 30 60 "solid" "Black"))

;(define (boxify img)
;   (... img))

(define (boxify img)
  (overlay img
           (rectangle (+ (image-width img) 2) (+ (image-height img) 2) "outline" "black")))