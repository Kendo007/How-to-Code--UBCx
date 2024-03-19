;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname accumulators-quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;  PROBLEM 1:
;
;  Assuming the use of at least one accumulator, design a function that consumes a list of strings,
;  and produces the length of the longest string in the list.
;

;; (lisOf String) -> Natural
;; produces the length of the longest string in the list
(check-expect (lengthiest empty) 0)
(check-expect (lengthiest (list "a" "ab" "abc" "beru")) 4)
(check-expect (lengthiest (list "audbsiu" "abudb" "abc" "beru")) 7)
(check-expect (lengthiest (list "a" "uib" "abc")) 3)

; (define (lengthiest los) 0)    ; stub

(define (lengthiest los)
  ; rsf: the length of longest string till now
  
  (local [(define (fn-for-los los rsf)
            (cond [(empty? los) rsf]
                  [(> rsf (string-length (first los))) (fn-for-los (rest los) rsf)]
                  [else (fn-for-los (rest los) (string-length (first los)))]))]
    (fn-for-los los 0)))

;  PROBLEM 2:
;
;  The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to
;  n-2 + n-1.
;
;  Design a function that given a list of numbers at least two elements long,
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every
;  element in the list. The sequence does not have to start at zero, so for
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule.
;

;; (listOf Numbers) -> Boolean
;; produces true iff the list follows fibonacci sequence

(check-expect (fibb (list 4 8 63)) false)
(check-expect (fibb (list 4 5 9 14 23)) true)
(check-expect (fibb (list 2 3)) true)

(define (fibb lon)
  ; n1: the second last element visited
  ; n2: the last element visited
  
  (local [(define (fn-for-lon lon n1 n2)
            (cond [(empty? lon) true]
                  [(= (first lon) (+ n1 n2)) (fn-for-lon (rest lon) n2 (first lon))]
                  [else false]))]
    (fn-for-lon (rest (rest lon)) (first lon) (first (rest lon)))))

;  PROBLEM 3:
;
;  Refactor the function below to make it tail recursive.
;


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)

#;
(define (fact n)
  (cond [(zero? n) 1]
        [else
         (* n (fact (sub1 n)))]))

(define (fact n)
  (local [(define (fn-for-n n rsf)
            (if (zero? n)
                rsf
                (fn-for-n (sub1 n) (* n rsf))))]
    (fn-for-n n 1)))

;  PROBLEM 4:
;
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist
;  accumulator to design a tail recursive function that counts the number of regions
;  within and including a given region.
;  So (count-regions CANADA) should produce 7



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

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))

          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))

;; Region -> Natural
;; counts the number of regions within and including a given region
(check-expect (count-regions CALGARY) 1)
(check-expect (count-regions ALBERTA) 3)
(check-expect (count-regions CANADA) 7)

; (define (count-regions r) 0)    ; stub

(define (count-regions r)
  ; rsf: number of regions covered till now

  (local [(define (fn-for-r r todo rsf)
            (fn-for-lor (append (region-subregions r) todo)
                        (add1 rsf)))

          (define (fn-for-lor todo rsf)
            (if (empty? todo)
                rsf
                (fn-for-r (first todo) (rest todo) rsf)))]
    (fn-for-r r empty 0)))
  
