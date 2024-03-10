;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Pluralize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; SOLUTION

; String -> String
; Pluralize the word
(check-expect (plural "apple") "apples")
(check-expect (plural "banana") "bananas")

;(define (plural n)
 ; "ok")

;(define (plural n)
;  (... n))

(define (plural n)
  (string-append n "s"))