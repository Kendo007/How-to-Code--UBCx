;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstraction-quiz-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;  PROBLEM 1:
;
;  Design an abstract function called arrange-all to simplify the
;  above-all and beside-all functions defined below. Rewrite above-all and
;  beside-all using your abstract function.


;; (listof Image) -> Image
;; combines a list of images into a single image, each image above the next one
(check-expect (above-all empty) empty-image)
(check-expect (above-all (list (rectangle 20 40 "solid" "red") (star 30 "solid" "yellow")))
              (above (rectangle 20 40 "solid" "red") (star 30 "solid" "yellow")))
(check-expect (above-all (list (circle 30 "outline" "black") (circle 50 "outline" "black") (circle 70 "outline" "black")))
              (above (circle 30 "outline" "black") (circle 50 "outline" "black") (circle 70 "outline" "black")))

;(define (above-all loi) empty-image)  ;stub

#;
(define (above-all loi)
  (cond [(empty? loi) empty-image]
        [else
         (above (first loi)
                (above-all (rest loi)))]))

(define (above-all loi)
  (arrange-all above empty-image loi))

;; (listof Image) -> Image
;; combines a list of images into a single image, each image beside the next one
(check-expect (beside-all empty) (rectangle 0 0 "solid" "white"))
(check-expect (beside-all (list (rectangle 50 40 "solid" "blue") (triangle 30 "solid" "pink")))
              (beside (rectangle 50 40 "solid" "blue") (triangle 30 "solid" "pink")))
(check-expect (beside-all (list (circle 10 "outline" "red") (circle 20 "outline" "blue") (circle 10 "outline" "yellow")))
              (beside (circle 10 "outline" "red") (circle 20 "outline" "blue") (circle 10 "outline" "yellow")))

;(define (beside-all loi) empty-image)  ;stub

#;
(define (beside-all loi)
  (cond [(empty? loi) (rectangle 0 0 "solid" "white")]
        [else
         (beside (first loi)
                 (beside-all (rest loi)))]))

(define (beside-all loi)
  (arrange-all beside (rectangle 0 0 "solid" "white") loi))

;; (X -> Y) Y (lisof X) -> Y
;; applies the given on the whole list

(define (arrange-all fn b loi)
  (cond [(empty? loi) b]
        [else
         (fn (first loi)
             (arrange-all fn b (rest loi)))]))

;  PROBLEM 2:
;
;  Finish the design of the following functions, using built-in abstract functions.
;


;; Function 1
;; ==========

;; (listof String) -> (listof Natural)
;; produces a list of the lengths of each string in los
(check-expect (lengths empty) empty)
(check-expect (lengths (list "apple" "banana" "pear")) (list 5 6 4))

; (define (lengths lst) empty) ;stub

(define (lengths lst) (map string-length lst))

;; Function 2
;; ==========

;; (listof Natural) -> (listof Natural)
;; produces a list of just the odd elements of lon
(check-expect (odd-only empty) empty)
(check-expect (odd-only (list 1 2 3 4 5)) (list 1 3 5))

; (define (odd-only lon) empty)

(define (odd-only lon) (filter odd? lon))

;; Function 3
;; ==========

;; (listof Natural -> Boolean
;; produce true if all elements of the list are odd
(check-expect (all-odd? empty) true)
(check-expect (all-odd? (list 1 2 3 4 5)) false)
(check-expect (all-odd? (list 5 5 79 13)) true)

; (define (all-odd? lon) empty)

(define (all-odd? lon) (andmap odd? lon))

;; Function 4
;; ==========

;; (listof Natural) -> (listof Natural)
;; subtracts n from each element of the list
(check-expect (minus-n empty 5) empty)
(check-expect (minus-n (list 4 5 6) 1) (list 3 4 5))
(check-expect (minus-n (list 10 5 7) 4) (list 6 1 3))

;(define (minus-n lon n) empty)

(define (minus-n lon n)
  (local [(define (sub i) (- i n))]
    (map sub lon)))

;  PROBLEM 3
;
;  Consider the data definition below for Region. Design an abstract fold function for region,
;  and then use it do design a function that produces a list of all the names of all the
;  regions in that region.
;
;  For consistency when answering the multiple choice questions, please order the arguments in your
;  fold function with combination functions first, then bases, then region. Please number the bases
;  and combination functions in order of where they appear in the function.
;
;  So (all-regions CANADA) would produce
;  (list "Canada" "British Columbia" "Vancouver" "Victoria" "Alberta" "Calgary" "Edmonton")


(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

;; (String X Z -> Y) (Y Z -> Z) X X X X X Z Region -> Y
(check-expect (all-regions CANADA) (list "Canada" "British Columbia" "Vancouver" "Victoria" "Alberta" "Calgary" "Edmonton"))

(define (fold-region c1 c2 b1 b2 b3 b4 b5 b6 r)                      ; returns Y
  (local [(define (fn-for-region r)                                  ; returns Y
            (c1 (region-name r)                                         ; String
                (fn-for-type (region-type r))                                ; X
                (fn-for-lor (region-subregions r))))                         ; Z

          (define (fn-for-type t)                                    ; returns X
            (cond [(string=? t "Continent") b1]                 ; each b is an X
                  [(string=? t "Country") b2]
                  [(string=? t "Province") b3]
                  [(string=? t "State") b4]
                  [(string=? t "City") b5]))

          (define (fn-for-lor lor)                                   ; returns Z
            (cond [(empty? lor) b6]
                  [else
                   (c2 (fn-for-region (first lor))                           ; Y
                       (fn-for-lor (rest lor)))]))]                          ; Z
    (fn-for-region r)))                                                      ; Y

(define (all-regions r)
  (local [(define (c1 name type lor)                     ; String X list -> list
            (cons name lor))]           ; c2 = append: Y Z -> Z, so Y = Z = list
    (fold-region c1 append empty empty empty empty empty empty r)))
