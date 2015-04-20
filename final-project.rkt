#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(include "tile-map.rkt")

(define nil '())

;****************** SPRITE STUFF **********************

(define make-sprite list)
(define sprite-type first)
(define sprite-image second)
(define (sprite-realX sprite) (car (third sprite)))  ;X
(define (sprite-realY sprite) (cadr (third sprite))) ;Y
(define (sprite-more-variables sprite) (cddr (third sprite))) ; for special conditions (only exist in player for now)
(define (sprite-jumpspeed sprite) (car (sprite-more-variables sprite)))
(define sprite-state fourth)
(define sprite-update fifth)
(define sprite-draw sixth)
(define sprite-width seventh)
(define sprite-height eighth)
(define sprite-frame-counter ninth)

(define (sprite-gridX sprite)
     (floor (/ (sprite-realX sprite) 64)))
(define (sprite-gridY sprite)
  (floor (/ (sprite-realY sprite) 64)))

; convenience functions
(define (change-sprite-state sprite state)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite)) (sprite-more-variables sprite)) 
               state 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)))

(define (change-sprite-coords sprite x y)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list x y) (sprite-more-variables sprite)) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)))

(define (change-sprite-jumpspeed sprite jumpspeed)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite) jumpspeed) (cdr (sprite-more-variables sprite))) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)))

(define (change-sprite-frame-counter sprite count)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite)) (sprite-more-variables sprite)) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               count))

; sprite predicates
(define (player? sprite)
  (is-type? "player" sprite))

(define (enemy? sprite)
  (equal? (substring (sprite-type sprite) 0 5) "enemy"))

(define (cursor? sprite)
  (is-type? "cursor" sprite))

(define (is-type? the-type sprite)
  (eq? the-type (sprite-type sprite)))

(define (is-state? sprite the-state)
  (eq? the-state (sprite-state sprite)))

; For detecting collision with walls
; MOVE LEFT
(define (move-left sprite speed)
  (cond ((and (= 1 (tile-at-xy current-map (- (sprite-gridX sprite) 1) (sprite-gridY sprite)))
              (<= (- (sprite-realX sprite) speed) (* 64 (sprite-gridX sprite))))
         (change-sprite-coords sprite (* 64 (sprite-gridX sprite)) (sprite-realY sprite)))
        ((and (= 1 (tile-at-xy current-map (- (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1)))
              (<= (- (sprite-realX sprite) speed) (* 64 (sprite-gridX sprite)))
              (> (sprite-realY sprite) (* 64 (sprite-gridY sprite))))
         (change-sprite-coords sprite (* 64 (sprite-gridX sprite)) (sprite-realY sprite)))
        (else (change-sprite-coords sprite (- (sprite-realX sprite) speed) (sprite-realY sprite)))))
; MOVE RIGHT
(define (move-right sprite speed)
  (cond ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (sprite-gridY sprite)))
              (>= (+ (sprite-realX sprite) (sprite-width sprite) speed) (* 64 (+ (sprite-gridX sprite) 1))))
         (change-sprite-coords sprite (+ (* 64 (sprite-gridX sprite)) (- 64 (sprite-width sprite))) (sprite-realY sprite)))
        
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1)))
              (>= (+ (sprite-realX sprite) (sprite-width sprite) speed) (* 64 (+ (sprite-gridX sprite) 1)))
              (> (sprite-realY sprite) (- (* 64 (+ (sprite-gridY sprite) 1)) (sprite-width sprite))))
         (change-sprite-coords sprite (+ (* 64 (sprite-gridX sprite)) (- 64 (sprite-width sprite))) (sprite-realY sprite)))
        #| ... I forget what these cases are for?
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 2) (sprite-gridY sprite)))
              (>= (+ (sprite-realX sprite) 64 speed) (* 64 (+ (sprite-gridX sprite) 2))))
         (change-sprite-coords sprite (* 64 (+ (sprite-gridX sprite) 1)) (sprite-realY sprite)))
        
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 2) (+ (sprite-gridY sprite) 1)))
              (>= (+ (sprite-realX sprite) 64 speed) (* 64 (+ (sprite-gridX sprite) 2))))
         (change-sprite-coords sprite (* 64 (+ (sprite-gridX sprite) 1)) (sprite-realY sprite)))|#
        (else (change-sprite-coords sprite (+ (sprite-realX sprite) speed) (sprite-realY sprite)))))
; JUMP
(define (jump sprite)
  (cond ; HITTING CEILING
        ((and (> (sprite-jumpspeed sprite) 0) 
              (<= (- (sprite-realY sprite) (sprite-jumpspeed sprite)) (* (sprite-gridY sprite) 64))
              (or (= 1 (tile-at-xy current-map (sprite-gridX sprite) (- (sprite-gridY sprite) 1)))
                  (and (> (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
                       (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (- (sprite-gridY sprite) 1))))))
         (change-sprite-jumpspeed 
          (change-sprite-coords sprite (sprite-realX sprite) (* 64 (sprite-gridY sprite)))
          0))
        ; HITTING FLOOR
        ((and (< (sprite-jumpspeed sprite) 0)
              (>= (- (sprite-realY sprite) (sprite-jumpspeed sprite)) (* (+ (sprite-gridY sprite) 1) 64))
              (> (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
              (or (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 2)))
                  (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 2)))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (- (* 64 (+ (sprite-gridY sprite) 2)) (sprite-height sprite)))
           "stand-right")
          16))
        
        ((and (< (sprite-jumpspeed sprite) 0)
              (>= (- (sprite-realY sprite) (sprite-jumpspeed sprite)) (* (+ (sprite-gridY sprite) 1) 64))
              (= (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
              (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 2))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (- (* 64 (+ (sprite-gridY sprite) 2)) (sprite-height sprite)))
           "stand-right")
          16))
        
        ((and (< (sprite-jumpspeed sprite) 0) 
              (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (- (* 64 (+ (sprite-gridY sprite) 1)) (sprite-height sprite)))
           "stand-right")
          16))
        (else (change-sprite-jumpspeed 
               (change-sprite-coords sprite (sprite-realX sprite) (- (sprite-realY sprite) (sprite-jumpspeed sprite))) 
               (- (sprite-jumpspeed sprite) 1)))))

(define (left-or-right sprite)
  (cond (left-button (move-left sprite 7))
        (right-button (move-right sprite 7))
        (else sprite)))

; PLAYER COLLISION WITH ENEMIES
(define (enemy-collision sprites)
  (let ((player (filter player? sprites)))
    (foldr (lambda (x y) (or x y)) #f
           (map (lambda (sprite) (if (enemy? sprite)
                                     (and (> (sprite-realX (car player)) (- (sprite-realX sprite) 64))
                                          (< (sprite-realX (car player)) (+ (sprite-realX sprite) (sprite-width sprite))) 
                                          (> (sprite-realY (car player)) (- (sprite-realY sprite) 64))
                                          (< (sprite-realY (car player)) (+ (sprite-realY sprite) (sprite-height sprite))))
                                     #f))
                sprites))))

; PLAYER UPDATE PROCEDURE
(define (player-update-proc sprite)
  (let* ((new-sprite-l-r (left-or-right sprite))
         (new-sprite (if (>= (sprite-frame-counter new-sprite-l-r) 48)
                         (change-sprite-frame-counter new-sprite-l-r 0)
                         (change-sprite-frame-counter new-sprite-l-r (+ (sprite-frame-counter new-sprite-l-r) 1)))))
    (cond ; LAND IN PIT
          ((eq? 3 (tile-at-xy current-map (sprite-gridX sprite) (sprite-gridY sprite)))
           (change-sprite-coords sprite 128 64))
          ; FALL
          ((and (not (is-state? new-sprite "jump"))
                (= (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
                (= 0 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1))))
           (change-sprite-jumpspeed (change-sprite-state new-sprite "jump") 0))
          ((and (not (is-state? new-sprite "jump"))
                (= 0 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1)))
                (= 0 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1))))
           (change-sprite-jumpspeed (change-sprite-state new-sprite "jump") 0))
          ; SHOOT WEAPON
          (x-button 
           (set! x-button #f)
           (list "pop-me" 
                 (make-sprite "projectile" bubble (list (+ (sprite-realX sprite) 20) (+ (sprite-realY sprite) 20) 10)
                              "start" weapon-one-update sprite-display-image 20 20 40)
                 (if (is-state? new-sprite "jump")
                     (jump new-sprite)
                     new-sprite)))
          ; JUMP BUTTON PRESS
          ((and z-button (not (is-state? new-sprite "jump")))
           (change-sprite-state new-sprite "jump"))
          ; JUMP
          ((is-state? new-sprite "jump")
           (jump new-sprite))
          (else new-sprite))))

; MISC SPRITE UPDATE FUNCTIONS
(define (sprite-null-update sprite) sprite)

(define (title-screen-cursor-update sprite)
  (cond ((and (or down-button up-button) (is-state? sprite "start"))
         (set! down-button #f)
         (set! up-button #f)
         (change-sprite-coords (change-sprite-state sprite "continue") 210 257))
        ((and (or down-button up-button) (is-state? sprite "continue"))
         (set! down-button #f)
         (set! up-button #f)
         (change-sprite-coords (change-sprite-state sprite "start") 210 225))
        (else sprite)))

(define (sprite-decay-update sprite)
  (if (<= (sprite-frame-counter sprite) 0)
      (list "kill-me")
      (change-sprite-frame-counter sprite (- (sprite-frame-counter sprite) 1))))

(define (projectile-1 sprite)
  (sprite-decay-update (move-right sprite 10)))
(define (projectile-1-and-up sprite)
  (sprite-decay-update (move-right (change-sprite-coords sprite (sprite-realX sprite) (- (sprite-realY sprite) .5)) 4)))
(define (projectile-2 sprite)
  (sprite-decay-update (move-left sprite 10)))

(define (weapon-one-update sprite)
  (let* ((new-sprite-maybe-kill (sprite-decay-update (jump (move-right sprite 8))))
         (new-sprite (if (and (not (eq? (car new-sprite-maybe-kill) "kill-me")) (= (sprite-realY sprite) (sprite-realY new-sprite-maybe-kill)))
                         (change-sprite-jumpspeed new-sprite-maybe-kill (- (sprite-jumpspeed sprite) 1))
                         new-sprite-maybe-kill)))
    (if (or (eq? (car new-sprite) "kill-me") (= (sprite-realX new-sprite) (sprite-realX sprite)))
        (make-sprite "projectile" bubble (list (sprite-realX sprite) (sprite-realY sprite)) "pop" sprite-decay-update weapon-one-pop-draw 20 20 6)
        new-sprite)))

; ENEMY UPDATE PROCEDURES
(define (fly-up sprite X Y) ; ALWAYS SET TO FLY LEFT, can be changed by passing + or -
  (cond ((< (sprite-realY sprite) 0)
         sprite)
        (else (change-sprite-coords sprite (- (sprite-realX sprite) X) (- (sprite-realY sprite) Y)))))

(define (fly-down sprite X Y)
  (cond ((> (sprite-realY sprite) 100)
         sprite)
        (else (change-sprite-coords sprite (- (sprite-realX sprite) X) (+ (sprite-realY sprite) Y)))))

(define (enemy-one-update-proc sprite)
  (cond ((<= (sprite-realX sprite) 0)
         (list "kill-me"))
        ((eq? (sprite-state sprite) "fly-up")
         (if (eq? (fly-up sprite 6 6) sprite)
                  (change-sprite-state sprite "fly-down")
                  (fly-up sprite 6 6)))
        (else 
         (if (eq? (fly-down sprite 6 6) sprite)
             (change-sprite-state sprite "fly-up")
             (fly-down sprite 6 6)))))

(define (enemy-two-update-proc sprite)
  (cond ((<= (sprite-realX sprite) 0)
         (list "kill-me"))
        ((eq? (sprite-state sprite) "fly-up")
         (if (eq? (fly-up sprite 6 6) sprite)
                  (change-sprite-state sprite "fly-down")
                  (fly-up sprite 6 6)))
        (else 
         (if (eq? (fly-down sprite 6 6) sprite)
             (change-sprite-state sprite "fly-up")
             (fly-down sprite 6 6)))))

(define (enemy-three-update-proc sprite)
  (cond ((eq? (sprite-state sprite) "walk-left")
         (if (equal? (move-left sprite 2) sprite)
             (change-sprite-state sprite "walk-right")
             (move-left sprite 2)))
        (else
         (if (equal? (move-right sprite 2) sprite)
             (change-sprite-state sprite "walk-left")
             (move-right sprite 2)))))

(define (enemy-four-update-proc sprite)
  (if (<= (sprite-jumpspeed sprite) 0)
      (list "pop-me"
            (change-sprite-jumpspeed sprite 120)
            (make-sprite "enemy-4-projectile" (rectangle 20 20 "solid" "white") 
                         (list (sprite-realX sprite) (sprite-realY sprite)) "move-left" projectile-2 sprite-display-image 20 20 100))
      (change-sprite-jumpspeed sprite (- (sprite-jumpspeed sprite) 1))))


(define (projectile-update-proc sprite)
  (if (= 1 (tile-at-xy current-map (sprite-gridX sprite) (sprite-gridY sprite)))
      (change-sprite-coords sprite 590 200)
      (change-sprite-coords sprite (- (sprite-realX sprite) 10) (sprite-realY sprite))))

; DRAW FUNCTIONS
(define (player-draw-proc sprite)
  (cond ((and (is-state? sprite "jump") (> (sprite-jumpspeed sprite) 0))
         player-jump-right-1)
        ((and (is-state? sprite "jump") (<= (sprite-jumpspeed sprite) 0))
         player-jump-right-2)
        ((and (>= (sprite-frame-counter sprite) 0) (< (sprite-frame-counter sprite) 12))
         player-walk-right-1)
        ((or (and (>= (sprite-frame-counter sprite) 12) (< (sprite-frame-counter sprite) 24))
             (and (>= (sprite-frame-counter sprite) 36) (<= (sprite-frame-counter sprite) 48)))
         player-walk-right-2)
        ((and (>= (sprite-frame-counter sprite) 24) (< (sprite-frame-counter sprite) 36))
         player-walk-right-3)
        (else (sprite-image sprite))))

(define (sprite-display-image sprite)
  (sprite-image sprite))

(define (weapon-one-pop-draw sprite)
  (if (< 3 (sprite-frame-counter sprite))
      bubble-pop-1
      bubble-pop-2))

(define arrow-image (bitmap "images/sprites/arrow.png"))
(define player-stand-right (bitmap "images/sprites/player1.png"))
(define player-jump-right-1 (bitmap "images/sprites/player-jump-right-1.png"))
(define player-jump-right-2 (bitmap "images/sprites/player-jump-right-2.png"))
(define player-walk-right-1 (bitmap "images/sprites/player-walk-right-1.png"))
(define player-walk-right-2 (bitmap "images/sprites/player-walk-right-2.png"))
(define player-walk-right-3 (bitmap "images/sprites/player-walk-right-3.png"))
(define bubble (bitmap "images/sprites/bubble.png"))
(define bubble-pop-1 (bitmap "images/sprites/bubble-pop-1.png"))
(define bubble-pop-2 (bitmap "images/sprites/bubble-pop-2.png"))
;(define player-sprites (list player-stand-right player-stand-left))

(define sprite-list-one   (list (make-sprite "cursor" arrow-image '(210 225) "start" title-screen-cursor-update sprite-display-image 64 64 0)))
(define sprite-list-two (list (make-sprite "player" player-stand-right (list 64 320 16) "stand-right" player-update-proc player-draw-proc 64 64 0)
                                ;(make-sprite "enemy-1" (rectangle 45 45 "solid" "red") '(590 30) "fly-up" enemy-one-update-proc sprite-display-image)
                                ;(make-sprite "enemy-2" (rectangle 45 45 "solid" "blue") '(320 30) "fly-left" enemy-two-update-proc sprite-display-image)
                                (make-sprite "enemy-3" (rectangle 20 64 "solid" "purple") '(256 320) "walk-left" enemy-three-update-proc sprite-display-image 20 64 0)
                                (make-sprite "enemy-4" (rectangle 64 64 "solid" "white") '(576 192 120) "walk-left" enemy-four-update-proc sprite-display-image 64 64 0)
                                ;(make-sprite "enemy-4" (rectangle 32 64 "solid" "white") '(576 192 120) "walk-left" sprite-null-update sprite-display-image 32 64)
                                ))


(define (draw-sprites sprite-list img offset)
  (foldr (lambda (x y) (place-image/align ((sprite-draw x) x) (- (sprite-realX x) offset) (sprite-realY x) 'left 'top y)) 
         img
         sprite-list))

;******************************************************


;******************** MAP STUFF ***********************

(define map-offset 0)
(define tiles-on-left 0)

(define map-one (list (list w1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai b1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai d1 ai ai ai ai ai b1 ai ai ai ai ai)
                      (list w1 ai ai ai ai ai d2 ai ai ai ai ai ai d2 ai d1 ai ai ai ai ai ai ai ai w4 w3 ai ai ai ai ai b1 ai ai ai ai)
                      (list w1 ai ai ai ai ai b1 ai ai b1 ai ai w4 g1 g1 w3 ai ai ai ai ai ai w4 g1 g3 g2 w3 ai ai ai ai ai ai ai ai ai)
                      (list w1 ai b1 d1 d1 b1 ai ai d2 ai ai w4 g3 g4 g4 w1 b1 b1 ai ai ai ai w2 g4 g4 g4 w1 d1 ai ai ai ai d1 d1 ai ai)
                      (list g2 g1 g1 g1 g1 g1 g1 g1 g1 g1 g1 g3 g4 g4 g4 g2 g1 g1 g1 w3 ai ai w2 g4 g4 g4 g2 g1 g1 g1 g1 g1 g1 g1 g1 g1)))

(define map-two (list (list ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai t1 ai ai ai ai ai ai ai)
                      (list ai t1 t1 t1 ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai t1 ai ai t1)
                      (list t1 t1 ai ai ai ai t1 ai ai ai)
                      (list t1 t1 t1 t1 t1 t1 t1 ai t1 t1)))

(define map-three 
                (list (list ai ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai b1)
                      (list ai ai ai ai ai ai ai ai ai b1 b1)
                      (list ai ai ai ai ai ai ai ai b1 b1 b1)
                      (list ai ai ai ai ai ai ai b1 b1 b1 b1)
                      (list b1 ai ai ai ai ai b1 b1 b1 b1 b1)
                      (list b1 b1 b1 b1 b1 b1 b1 b1 b1 b1 b1)))

(define current-map map-one)

(define (choose-map map-num)
  (cond ((= map-num 1) (sub-map map-one tiles-on-left (+ tiles-on-left 11)))
        ((= map-num 2) map-two)
        ((= map-num 3) map-three)
        (else map-one)))

(define (draw-map my-map img tile-offset)
  (define (draw-map-help the-map x-max y-max result)
    (cond ((null? the-map) result)
          ((null? (car the-map)) (draw-map-help (cdr the-map) x-max y-max result))
          (else (draw-map-help (append (list (cdr (car the-map))) (cdr the-map)) x-max y-max 
                      (place-image/align (draw-tile (car (car the-map)))
                                         (- (* (- x-max (length (car the-map))) 64) map-offset)
                                         (* (- y-max (length the-map)) 64)
                                         'left 'top 
                                         result)))))
  (draw-map-help my-map (length (car my-map)) (length my-map) img))

(define (draw-tile tile)
  (tile-image tile))

;******************************************************

(define make-world list)
(define get-state first)
(define get-map second)
(define get-sprites third)
(define get-bg fourth)
(define get-map-offset fifth)
(define get-tiles-on-left sixth)
(define get-health seventh)
(define get-inventory-sprites eighth)

(define (change-world-sprites y sprites)
  (make-world (get-state y) (get-map y) sprites (get-bg y) (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-inventory-sprites y)))
(define (change-world-inventory-sprites y sprites)
  (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) (get-health y) sprites))

(define (get-player y)
  (define maybe-player (filter player? (get-sprites y)))
  (if (eq? maybe-player nil)
      nil
      (car maybe-player)))

(define (main y)
  (big-bang y
            [on-tick update]
            [stop-when (lambda (y) (= (get-map y) 2))]
            [to-draw draw]
            [on-key keys-down]
            [on-release key-release]))

(define (side-scroll-going-right)
  (let ((new-map-offset (+ map-offset 7)))
    (cond ((and (>= new-map-offset 64) (< tiles-on-left (- (length (car current-map)) 11)))
           (set! tiles-on-left (+ tiles-on-left 1))
           (set! map-offset (- new-map-offset 64)))
          ((and (= tiles-on-left (- (length (car current-map)) 11)) (< new-map-offset 64))
           (set! map-offset new-map-offset))
          ((and (= tiles-on-left (- (length (car current-map)) 11)) (>= new-map-offset 64))
           (set! map-offset 64))
          ((< tiles-on-left (- (length (car current-map)) 11))
           (set! map-offset new-map-offset))
        ;else ??
          )))

(define (side-scroll-going-left)
  (let ((new-map-offset (- map-offset 7)))
    (cond ((and (<= new-map-offset 0) (> tiles-on-left 0))
           (set! tiles-on-left (- tiles-on-left 1))
           (set! map-offset (+ new-map-offset 64)))
          ((and (= tiles-on-left 0) (> new-map-offset 0))
           (set! map-offset new-map-offset))
          ((and (= tiles-on-left 0) (<= new-map-offset 0))
           (set! map-offset 0))
          ((> tiles-on-left 0)
           (set! map-offset new-map-offset))
        ;else ??
          )))

(define (side-scroll sprites)
  (let ((player (filter player? sprites)))
    (cond ((>= (- (sprite-realX (car player)) (+ map-offset (* 64 tiles-on-left))) 352)
           (side-scroll-going-right))
          ((< (- (sprite-realX (car player)) (+ map-offset (* 64 tiles-on-left))) 288)
           (side-scroll-going-left)))))

(define (update y)
  (cond ; PAUSE BUTTON PRESS
        ((and shift-button (eq? (get-state y) "playing"))
         (set! shift-button #f)
         (make-world "pause menu" (get-map y) (get-sprites y) pause-menu (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-inventory-sprites y)))
        ((and shift-button (eq? (get-state y) "pause menu"))
         (set! shift-button #f)
         (make-world "playing" (get-map y) (get-sprites y) bg-1 (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-inventory-sprites y)))
        ; PLAYING
        ((eq? (get-state y) "playing")
              (side-scroll (get-sprites y))
              (let ((new-sprites (foldr (lambda (x y) (cond ((eq? (car x) "kill-me") y)
                                                            ((eq? (car x) "pop-me") (append (cdr x) y))
                                                            (else (cons x y)))) 
                                        nil
                                        (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-sprites y)))))
                (if (enemy-collision new-sprites)
                    (make-world (get-state y) (get-map y) new-sprites (get-bg y) (get-map-offset y) (get-tiles-on-left y) (- (get-health y) 1) (get-inventory-sprites y))
                    (make-world (get-state y) (get-map y) new-sprites (get-bg y) (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-inventory-sprites y)))))
        ; TITLE SCREEN
        ((eq? (get-state y) "title screen")
         (let* ((new-sprites (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-sprites y)))
               (cursor (filter cursor? new-sprites)))
           (if (and z-button (is-state? (car cursor) "start"))
               (begin (set! z-button #f)
                      (make-world "playing" 1 sprite-list-two bg-1 (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-inventory-sprites y)))
               (change-world-sprites y new-sprites))))
        ((eq? (get-state y) "pause menu")
         (change-world-inventory-sprites y (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-inventory-sprites y))))
        (else y)))


(define (draw y)
  (cond ((eq? (get-state y) "playing")
         (let ((health-width (if (> (get-health y) 0) (get-health y) 0)))
           ; draw background, then draw map into the background, then draw sprites into map, then draw HUD onto the whole thing
           (place-image/align (place-image/align (place-image/align healthbar 0 0 'left 'top (empty-scene (* 1.37 health-width) 26))
                                                 103 16 'left 'top
                                                 HUD)
                              10 10 'left 'top
                              (draw-sprites (get-sprites y)
                                            (draw-map (choose-map (get-map y)) (get-bg y) (get-map-offset y)) (+ map-offset (* 64 tiles-on-left))))))
        ((eq? (get-state y) "pause menu")
         (place-image/align (text "A basic spell. Shoots a bubble to hurt enemies." 24 'white) 50 365 'left 'top 
                            (draw-sprites (get-inventory-sprites y) pause-menu 0)))
        ; else case is Title Screen
        (else (draw-sprites (get-sprites y) (get-bg y) 0))))

; KEY LIST
; should we do this another way?
(define left-button #f)
(define up-button #f)
(define right-button #f)
(define down-button #f)
(define z-button #f)
(define x-button #f)
(define shift-button #f)

; KEY DOWN EVENTS
(define (keys-down y ke)
  (cond ((key=? ke "left")  (set! left-button #t))
        ((key=? ke "up")  (set! up-button #t))
        ((key=? ke "right")  (set! right-button #t))
        ((key=? ke "down")  (set! down-button #t))
        ((key=? ke "z")  (set! z-button #t))
        ((key=? ke "x")  (set! x-button #t))
        ((key=? ke "shift") (set! shift-button #t)))
  y)

; KEY UP EVENTS
(define (key-release y ke)
  (cond ((key=? ke "left")  (set! left-button #f))
        ((key=? ke "up")  (set! up-button #f))
        ((key=? ke "right")  (set! right-button #f))
        ((key=? ke "down")  (set! down-button #f))
        ((key=? ke "z")  (set! z-button #f))
        ((key=? ke "x")  (set! x-button #f))
        ((key=? ke "shift")  (set! shift-button #f)))
  y)

(define title-screen-bg (bitmap "images/title.png"))
(define bg-1 (bitmap "images/Background-1.png"))
(define bg-2 (bitmap "images/Background-2.png"))
(define pause-menu (bitmap "images/menu/pause-menu.png"))
(define pause-cursor-1 (bitmap "images/menu/pause-cursor-1.png"))
(define pause-cursor-2 (bitmap "images/menu/pause-cursor-2.png"))

(define healthbar (bitmap "images/healthbar.png"))
(define HUD (bitmap "images/HUD.png"))

(define (pause-cursor-update sprite)
  (let ((new-sprite (if (>= (sprite-frame-counter sprite) 48)
                         (change-sprite-frame-counter sprite 0)
                         (change-sprite-frame-counter sprite (+ (sprite-frame-counter sprite) 1)))))
        (cond ((and up-button (is-state? sprite "position 1"))
               (set! up-button #f)
               (change-sprite-state (change-sprite-coords new-sprite 0 0) "save position"))
              ((and right-button (is-state? sprite "position 1"))
               (set! right-button #f)
               menu-cursor-pos-2)
              ((and left-button (is-state? sprite "position 1"))
               (set! left-button #f)
               menu-cursor-pos-4)
              ((and right-button (is-state? sprite "position 2"))
               (set! right-button #f)
               menu-cursor-pos-3)
              ((and left-button (is-state? sprite "position 2"))
               (set! left-button #f)
               menu-cursor-pos-1)
              ((and right-button (is-state? sprite "position 3"))
               (set! right-button #f)
               menu-cursor-pos-4)
              ((and left-button (is-state? sprite "position 3"))
               (set! left-button #f)
               menu-cursor-pos-2)
              ((and right-button (is-state? sprite "position 4"))
               (set! right-button #f)
               menu-cursor-pos-1)
              ((and left-button (is-state? sprite "position 4"))
               (set! left-button #f)
               menu-cursor-pos-3)
              (else new-sprite))))

(define (pause-cursor-draw sprite)
  (if (< (sprite-frame-counter sprite) 24)
      pause-cursor-1
      pause-cursor-2))

(define menu-cursor-pos-1 (make-sprite "cursor" pause-cursor-1 (list 50 230) "position 1" pause-cursor-update pause-cursor-draw 106 106 0)) 
(define menu-cursor-pos-2 (make-sprite "cursor" pause-cursor-1 (list 170 230) "position 2" pause-cursor-update pause-cursor-draw 106 106 0)) 
(define menu-cursor-pos-3 (make-sprite "cursor" pause-cursor-1 (list 300 230) "position 3" pause-cursor-update pause-cursor-draw 106 106 0)) 
(define menu-cursor-pos-4 (make-sprite "cursor" pause-cursor-1 (list 430 230) "position 4" pause-cursor-update pause-cursor-draw 106 106 0)) 

(main (make-world "title screen" 0 sprite-list-one title-screen-bg 0 0 100 (list menu-cursor-pos-1)))