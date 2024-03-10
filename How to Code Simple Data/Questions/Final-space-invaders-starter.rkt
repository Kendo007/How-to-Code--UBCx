;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Final-space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10) ; Not used

(define INVADE-RATE 70)  ; Decrease to increase number of ships

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define ICUT (/ (image-width INVADER) 2))
(define TCUT (/ (image-width TANK) 2))
(define VICUT (/ (image-height INVADER) 2))

;; Data Definitions:

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 INVADER-X-SPEED))           ;not landed, moving right
(define I2 (make-invader 150 200 (* -1 INVADER-X-SPEED)))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) INVADER-X-SPEED)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position 

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; main function

;; world starts with (main G0)
(define (main g)
  (big-bang g
    (on-tick next-game)
    (to-draw render-game)
    (on-key  move-game)
    (stop-when game-over? GAME-OVER)))

;; Functions

;; Primary Function 
;; game -> game
;; produces the next event of game
(check-random (next-game G1)
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-random (next-game G3)
              (make-game (cons (make-invader 151.5 101.5 1.5) (cons (make-invader 148.5 201.5 -1.5)
                                                                    (if (= (remainder (random 400) INVADE-RATE) 0)
                              (cons (make-invader (+ ICUT (random (- WIDTH (+ ICUT ICUT)))) 0 (if (= (remainder (random 10) 2) 0)
                                                                                                  INVADER-X-SPEED
                                                                                                  (* -1 INVADER-X-SPEED))) empty)
                              empty)))
                         (list (make-missile 150 290) (make-missile 150 100))
                         (make-tank 52 1)))

;;(define (next-game g) G0) ; stub

(define (next-game g)
  (make-game (next-invaders (game-invaders g) (game-missiles g))
             (next-missiles (game-missiles g) (game-invaders g))
             (next-tank (game-tank g))))

;; Helper Function 
;; ListofOfIvaders ListofMissiles -> ListofInvaders
;; Moves the current invaders, add new invaders and deletes invaders if hit by missile
(check-random (next-invaders empty empty) (if (= (remainder (random 400) INVADE-RATE) 0)
                                              (cons (make-invader (+ ICUT (random (- WIDTH (+ ICUT ICUT)))) 0 (if (= (remainder (random 10) 2) 0)
                                                                                                                  INVADER-X-SPEED
                                                                                                                  (* -1 INVADER-X-SPEED))) empty)
                                              empty))
(check-random (next-invaders (list I1 I2 I3) (list M1 M2 M3))
              (cons (make-invader 148.5 201.5 -1.5)
                    (cons (make-invader 151.5 511.5 1.5)
                          (if (= (remainder (random 400) INVADE-RATE) 0)
                              (cons (make-invader (+ ICUT (random (- WIDTH (+ ICUT ICUT)))) 0 (if (= (remainder (random 10) 2) 0)
                                                                                                  INVADER-X-SPEED
                                                                                                  (* -1 INVADER-X-SPEED))) empty)
                              empty))))

; (define (next-invaders loi lom) loi)      ; stub

(define (next-invaders loi lom)
  (cond [(empty? loi) (if (= (remainder (random 400) INVADE-RATE) 0)
                          (cons (make-invader (+ ICUT (random (- WIDTH (+ ICUT ICUT)))) 0 (if (= (remainder (random 10) 2) 0)
                                                                                              INVADER-X-SPEED
                                                                                              (* -1 INVADER-X-SPEED))) empty)
                          empty)]
        [else
         (if (missile-hit? lom (first loi))
             (next-invaders (rest loi) lom)
             (cons (move-invader (first loi))
                   (next-invaders (rest loi) lom)))]))

;; Helpers Helper Function 
;; ListofInvaders ListofMissiles -> Boolean
;; delete invaders from the list if hit by missile
(check-expect (missile-hit? (cons M1 empty) I1) false)
(check-expect (missile-hit? (cons M2 empty) I1) false)
(check-expect (missile-hit? (cons M1 (cons M3 empty)) I1) true)
(check-expect (missile-hit? (cons M1 (cons M3 empty)) I1) true)

; (define (hit-by-missile lom i) false)     ; stub

(define (missile-hit? lom i)
  (cond [(empty? lom) false]
        [else
         (if (and (and (<= (- (invader-x i) ICUT) (missile-x (first lom)))
                       (>= (+ (invader-x i) ICUT) (missile-x (first lom))))
                  (and (>= (+ (invader-y i) VICUT) (missile-y (first lom)))
                       (<= (- (invader-y i) VICUT) (missile-y (first lom)))))
             true
             (missile-hit? (rest lom) i))]))

;; Helpers Helper Function 
;; invader -> invader
;; increases the position of invader
(check-expect (move-invader (make-invader 150 100 INVADER-X-SPEED)) (make-invader (+ 150 INVADER-X-SPEED)
                                                                                  (+ 100 INVADER-Y-SPEED)
                                                                                  INVADER-X-SPEED))
(check-expect (move-invader (make-invader (- WIDTH ICUT) 100 INVADER-X-SPEED)) (make-invader (- (- WIDTH ICUT) INVADER-X-SPEED)
                                                                                             (+ 100 INVADER-Y-SPEED)
                                                                                             (* -1 INVADER-X-SPEED)))
(check-expect (move-invader (make-invader ICUT 100 INVADER-X-SPEED)) (make-invader (- ICUT INVADER-X-SPEED)
                                                                                   (+ 100 INVADER-Y-SPEED)
                                                                                   (* -1 INVADER-X-SPEED)))

; (define (move-invader i) i)       ; stub

(define (move-invader i)
  (if (or (>= (invader-x i) (- WIDTH ICUT))
          (<= (invader-x i) ICUT))
      (make-invader (- (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (* -1 (invader-dx i)))
      (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))))


;; Helper Function 
;; ListofMissiles ListofInvaders -> ListofMissiles
;; Moves the current missile and deletes missile if hit invader
(check-expect (next-missiles empty empty) empty)
(check-expect (next-missiles (list M1) (list I1)) (list (make-missile 150 290)))
(check-expect (next-missiles (list M3) (list I1)) empty)
(check-expect (next-missiles (list M3 M1 M2) (list I1 I2)) (list (make-missile 150 290) (make-missile 150 100)))

; (define (next-missiles lom loi) lom)  ; stub

(define (next-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or (del-missile? (first lom) loi)
                 (< (+ 10 (missile-y (first lom))) 0))
             (next-missiles (rest lom) loi)
             (cons (move-missile (first lom))
                   (next-missiles (rest lom) loi)))]))

;; Helpers Helper Function
;; missile ListofInvaders -> Boolean
(check-expect (del-missile? M1 (cons I1 empty)) false)
(check-expect (del-missile? M3 (cons I1 empty)) true)
(check-expect (del-missile? M3 (cons I1 (cons I2 (cons I3 empty)))) true)
(check-expect (del-missile? M3 (cons I2 (cons I3 empty))) false)

; (define (del-missile m loi) false)      ; stub

(define (del-missile? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (and (<= (- (invader-x (first loi)) ICUT) (missile-x m))
                       (>= (+ (invader-x (first loi)) ICUT) (missile-x m)))
                  (and (>= (+ (invader-y (first loi)) VICUT) (missile-y m))
                       (<= (- (invader-y (first loi)) VICUT) (missile-y m))))
             true
             (del-missile? m (rest loi)))]))

;; Helpers Helper Function 
;; Missile -> Missile
;; moves missile by MISSILE SPEED
(check-expect (move-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))

; (define (move-missile m) m)   ; stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; tank -> tank
;; moves tank left and right for the given direction
(check-expect (next-tank (make-tank 200 1)) (make-tank (+ 200 TANK-SPEED) 1))

(define (next-tank t)
  (cond [(>= (tank-x t) (- WIDTH TCUT))
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) -1)]
        [(<= (tank-x t) TCUT)
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 1)]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; Primary Function 
;; game -> Image
;; produces image of the game

; (define (render-game G0) BACKGROUND)

(define (render-game g)
  (place-images (list-img INVADER (length (game-invaders g)))
                (list-iposn (game-invaders g))
                (place-images (list-img MISSILE (length (game-missiles g)))
                              (list-mposn (game-missiles g))
                              (place-image TANK (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))

;; Natural -> ListOfImageofInvdaders
;; creates a list ofimages of given image for the given natural
(check-expect (list-img INVADER 3) (cons INVADER (cons INVADER (cons INVADER empty))))
(check-expect (list-img MISSILE 4) (list MISSILE MISSILE MISSILE MISSILE))

; (define (list-invader n) empty)  ; stub

(define (list-img img n)
  (cond [(zero? n) empty]
        [else
         (cons img (list-img img (sub1 n)))]))

;; ListofInvaders -> ListofInvadersPositions
;; Gives a list of inaders position
(check-expect (list-iposn (list I1)) (list (make-posn 150 100)))
(check-expect (list-iposn (list I1 I2)) (list (make-posn 150 100) (make-posn 150 200)))

; (define (list-iposn loi) empty)  ; stub

(define (list-iposn loi)
  (cond [(empty? loi) empty]
        [else
         (cons (make-posn (invader-x (first loi))
                          (invader-y (first loi)))
               (list-iposn (rest loi)))]))

;; ListofMissiles -> ListofmissilesPositions
;; Gives a list of inaders position
(check-expect (list-mposn (list M1)) (list (make-posn 150 300)))
(check-expect (list-mposn (list M1 M2)) (list (make-posn 150 300) (make-posn 150 110)))

; (define (list-iposn loi) empty)  ; stub

(define (list-mposn lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (missile-x (first lom))
                          (missile-y (first lom)))
               (list-mposn (rest lom)))]))

;; Primary Function 
;; game -> game
;; moves tank left and right if <- or -> is pressed
;; fires missiles if space key is pressed
(check-expect (move-game (make-game empty empty (make-tank 150 1)) "left") (make-game empty empty (make-tank 150 -1)))
(check-expect (move-game (make-game (list I1) (list M1) (make-tank 150 -1)) "right")
              (make-game (list I1) (list M1) (make-tank 150 1)))
(check-expect (move-game (make-game (list I1 I2 I3) (list M1 M2) (make-tank 230 -1)) " ")
              (make-game
               (list I1 I2 I3) (list (make-missile 230 488) M1 M2) (make-tank 230 -1)))
(check-expect (move-game G2 "a") G2)

; (define (move-game g key) G0)

(define (move-game g key)
  (cond [(key=? "left" key)
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? "right" key)
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(key=? " " key)
         (make-game (game-invaders g) (add-missile g) (make-tank (tank-x (game-tank g))
                                                                 (tank-dir (game-tank g))))]
        [else g]))

;; Helper Function 
;; game -> ListofMissiles
;; adds a missile at the tank postion to the given list of missiles
(check-expect (add-missile G1) (list (make-missile 50 488)))
(check-expect (add-missile G2) (list (make-missile 50 488) (make-missile 150 300)))

(define (add-missile g)
  (cons (make-missile (tank-x (game-tank g))
                      (- HEIGHT TANK-HEIGHT/2)) (game-missiles g)))

;; Primary Functiom
;; Game -> Boolean
;; stops the game if invader reaches ground

(define (game-over? g)
  (invader-ground? (game-invaders g)))

;; Helper Function
;; ListofInvaders -> Boolean
(check-expect (invader-ground? (cons (make-invader 150 HEIGHT INVADER-X-SPEED) empty)) true)
(check-expect (invader-ground? (cons (make-invader 150 209 INVADER-X-SPEED) empty)) false)

(define (invader-ground? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (invader-ground? (rest loi)))]))

;; Nothing -> Image
;; produces game over on screen

(define (GAME-OVER g)
  (place-image (text "Game Over!" 50 "red") (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND))