;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt

;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

; ==============================
; DATA DEFINITION

(define-struct user (name verf? following))
;; User is (make-user String Boolean (ListOf user)
;; interp. name as the name of the user
;;         verf as if the user is verified
;;         following as the people user follows
(define U1 (shared ((-K- (make-user "Kendo" true (list -D-)))
                    (-D- (make-user "Demon" false (list -K-))))
             -K-))

(define U2
  (shared ((-K- (make-user "Kendo" true (list -R- -D-)))
           (-D- (make-user "Demon" true (list -R-)))
           (-R- (make-user "Raju" true (list -K-))))
    -D-))

(define U3 (shared ((-D- (make-user "Demon" false (list -C- -F-)))
                    (-A- (make-user "Alice" true empty))
                    (-B- (make-user "Bob" false (list -K- -A- -F-)))
                    (-K- (make-user "Kendo" true (list -G- -D-)))
                    (-C- (make-user "Charlie" true (list -E- -F-)))
                    (-E- (make-user "Eva" true (list -K- -B-)))
                    (-F- (make-user "Frank" false (list -E- -K-)))
                    (-G- (make-user "George" true (list -A- -C- -E- -K- -D-))))
             -D-))

; ==============================
; TEMPLATE
#;
(define (fn-for-chirper user)
  ; visited: all the users visited till now
  ; todo: all the users we need to visit
  
  (local [(define (fn-for-u u todo visited)
            (if (member u visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-following u) todo)    ; (... (user-name u))
                            (cons (user-name u) visited))))     ; (... (user-verified u))

          (define (fn-for-lou todo visited)
            (if (empty? todo)
                (...)
                (fn-for-u (first todo) (rest todo) visited)))]
    (fn-for-u user empty empty)))

; ==============================
; FUNCTIONS

;; User -> User
;; consumes a user and produces the user with most followers in that network
(check-expect (most-followers U1) U1)
(check-expect (most-followers U2) (first (user-following U2)))
(check-expect (most-followers U3) (first (user-following                               ; Kendo
                                          (first (user-following                       ; Eva
                                                  (first (user-following U3)))))))     ; Charlie

; (define (most-followers user) user)     ; stub

(define (most-followers u0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof myMap); context preserving accumulator, count of users already visited
  
  (local [(define-struct myMap (followers u))
          ; myMap is (make-myMap Natural Room)
          ; interp. as the rumber of time the given user is visited

          ;; User (listOf MyMap) -> Boolean
          ;; Produces true if the given User is in listOfMaps
          (define (checkMap u lom)
            (cond [(empty? lom) false]
                  [(equal? u (myMap-u (first lom))) true]
                  [else (checkMap u (rest lom))]))

          ;; User (listOf MyMap) -> MyMap
          ;; Increases the count of given user by 1
          (define (updateMap u lom)
            (cond [(empty? lom) (error "Not Possible")]
                  [(equal? u (myMap-u (first lom))) (cons (make-myMap (add1 (myMap-followers (first lom))) u)
                                                          (rest lom))]
                  [else (cons (first lom) (updateMap u (rest lom)))]))

          ;; (listOf Map) -> User
          ;; returns the user with highest number of followers
          (define (maxMap visited)
            ;; rsf: the user with most followers so far
            
            (local [(define (maxMap visited rsf)
                      (cond [(empty? visited) rsf]
                            [(> (myMap-followers (first visited)) (myMap-followers rsf))
                             (maxMap (rest visited) (first visited))]
                            [else (maxMap (rest visited) rsf)]))]
              (myMap-u (maxMap visited (first visited)))))

          (define (fn-for-user u todo visited)
            (if (checkMap u visited)
                (fn-for-lou todo (updateMap u visited))
                (fn-for-lou (append (user-following u) todo)
                            (cons (make-myMap 1 u) visited))))
          
          (define (fn-for-lou todo visited)
            (if (empty? todo)
                (maxMap visited)
                (fn-for-user (first todo) (rest todo) visited)))]
    (fn-for-user u0 empty empty)))
 
;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))
(define NOODLE-TAs (list SOBA UDON RAMEN))

;; From Solution File
(define ERIKA (make-ta "Erika" 1 (list 1 3 7 9)))
(define RYAN (make-ta "Ryan" 1 (list 1 8 10)))
(define REECE (make-ta "Reece" 1 (list 5 6)))
(define GORDON (make-ta "Gordon" 2 (list 2 3 9)))
(define DAVID (make-ta "David" 2 (list 2 8 9)))
(define KATIE (make-ta "Katie" 1 (list 4 6)))
(define AASHISH (make-ta "Aashish" 2 (list 1 10)))
(define GRANT (make-ta "Grant" 2 (list 1 11)))
(define RAEANNE (make-ta "Raeanne" 2 (list 1 11 12)))
(define ALEX (make-ta "Alex" 1 (list 7)))
(define ERIN (make-ta "Erin" 1 (list 4)))
(define QUIZ-TAs-1 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE))
(define QUIZ-TAs-2 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ALEX))
(define QUIZ-TAs-3 (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE ERIN))


(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)
;
(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 1)
                                                          (make-assignment SOBA 3)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment RAMEN 2)
               (make-assignment UDON 4)
               (make-assignment SOBA 1)
               (make-assignment SOBA 3)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)

(check-expect (schedule-tas QUIZ-TAs-1 (list 1 2 3 4 5 6 7 8 9 10 11 12)) false)
(check-expect (schedule-tas QUIZ-TAs-2 (list 1 2 3 4 5 6 7 8 9 10 11 12)) false)
(check-expect (schedule-tas QUIZ-TAs-3 (list 1 2 3 4 5 6 7 8 9 10 11 12))
              (list (make-assignment (make-ta "Erin" 1 (list 4)) 4)
                    (make-assignment (make-ta "Raeanne" 2 (list 1 11 12)) 12)
                    (make-assignment (make-ta "Grant" 2 (list 1 11)) 11)
                    (make-assignment (make-ta "Aashish" 2 (list 1 10)) 10)
                    (make-assignment (make-ta "Katie" 1 (list 4 6)) 6)
                    (make-assignment (make-ta "David" 2 (list 2 8 9)) 8)
                    (make-assignment (make-ta "David" 2 (list 2 8 9)) 9)
                    (make-assignment (make-ta "Gordon" 2 (list 2 3 9)) 2)
                    (make-assignment (make-ta "Gordon" 2 (list 2 3 9)) 3)
                    (make-assignment (make-ta "Reece" 1 (list 5 6)) 5)
                    (make-assignment (make-ta "Ryan" 1 (list 1 8 10)) 1)
                    (make-assignment (make-ta "Erika" 1 (list 1 3 7 9)) 7)))


; (define (schedule-tas tas slots) empty) ;stub

(define (schedule-tas tas slots)
  ; sch: Schedule made till now
  
  (local [(define (fn-for-sch sch tas slots)
            (local [(define f-slots (filter-slots sch slots))]  ; filterd slots
              (if (empty? f-slots)
                  sch
                  (fn-for-losch tas f-slots
                                (next-schedules tas f-slots sch)))))

          (define (fn-for-losch tas slots losch)
            (cond [(empty? losch) false]
                  [else (local [(define try (fn-for-sch (first losch) (rest tas) slots))]
                          (if (not (false? try))
                              try
                              (fn-for-losch tas slots (rest losch))))]))]
    (fn-for-sch empty tas slots)))

;; Schedule Slot -> Boolean
;; produces true if the given slot is done in schedule else false
(check-expect (present empty 4) false)
(check-expect (present (list (make-assignment UDON 3) (make-assignment RAMEN 2)) 3) true)
(check-expect (present (list (make-assignment UDON 3) (make-assignment RAMEN 2)) 5) false)

(define (present sch slot)
  (cond [(empty? sch) false]
        [(= (assignment-slot (first sch)) slot) true]
        [else (present (rest sch) slot)]))

;; Schedule (listOf Slot) -> (listOf Slot)
;; removes all the slots which are present in schedule
(check-expect (filter-slots empty (list 3 4 5)) (list 3 4 5))
(check-expect (filter-slots (list (make-assignment UDON 3) (make-assignment RAMEN 2)) (list 3 4 5)) (list 4 5))
(check-expect (filter-slots (list (make-assignment UDON 3) (make-assignment RAMEN 2)) (list 3)) empty)
(check-expect (filter-slots (list (make-assignment UDON 3) (make-assignment RAMEN 2)) (list 1)) (list 1))

(define (filter-slots sch slots)
  (cond [(empty? slots) empty]
        [(present sch (first slots)) (filter-slots sch (rest slots))]
        [else (cons (first slots) (filter-slots sch (rest slots)))]))

;; Natural (listOf X) -> (listOf (listOf X))
;; produces a list of subsets of the given size from the list
;; or return the list if size >= (length (listOf X))
(check-expect (subsets 1 (list 1 2 3 4)) (list (list 1) (list 2) (list 3) (list 4)))
(check-expect (subsets 2 (list 1 2 3 4)) (list (list 1 2) (list 1 3) (list 1 4) (list 2 3) (list 2 4) (list 3 4)))
(check-expect (subsets 3 (list 1 2 3 4)) (list (list 1 2 3) (list 1 2 4) (list 1 3 4) (list 2 3 4)))
(check-expect (subsets 4 (list 1 2 3 4)) (list (list 1 2 3 4)))
(check-expect (subsets 5 (list 1 2 3 4)) (list (list 1 2 3 4)))

; (define (subsets size lox) empty)    ; stub

(define (subsets size lox)
  (local [(define (fn-for-lox count rsf lox)
            (if (empty? lox)
                (list rsf)
                (append (fn-for-lox (add1 count) (append rsf (list (first lox))) (rest lox))
                        (fn-for-lox (add1 count) rsf (rest lox)))))]
                
    (if (>= size (length lox))
        (list lox)
        (filter (λ (x) (= size (length x))) (fn-for-lox 0 empty lox)))))

;; TA (listOf Slots) Schedule -> (listOf Schedule)
;; produces a list of all the possible schedules if the given ta is assigned
(check-expect (next-schedules empty (list 1 2 3 4) empty) empty)
(check-expect (next-schedules (list UDON SOBA) (list 1 2 3 4) empty)
              (list (list (make-assignment UDON 3))
                    (list (make-assignment UDON 4))))
(check-expect (next-schedules (list UDON SOBA) (list 1 3) (list (make-assignment RAMEN 2)))
              (list (list (make-assignment UDON 3) (make-assignment RAMEN 2))))
(check-expect (next-schedules (list UDON SOBA) (list 1 3 4) (list (make-assignment RAMEN 2)))
              (list (list (make-assignment UDON 3) (make-assignment RAMEN 2))
                    (list (make-assignment UDON 4) (make-assignment RAMEN 2))))
               
(define (next-schedules tas slots sch)
  (if (empty? tas)
      empty
      (local [(define t (first tas))
              (define useful-avail (filter (λ (x) (member x slots)) (ta-avail t)))
              (define subs (subsets (ta-max t) useful-avail))]
        (map (λ (z) (append z sch))
             (map (λ (x)
                    (map (λ (y) (make-assignment t y)) x))
                  subs)))))
