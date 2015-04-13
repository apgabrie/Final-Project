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

(define (sprite-gridX sprite)
  (floor (/ (sprite-realX sprite) 64)))
(define (sprite-gridY sprite)
  (floor (/ (sprite-realY sprite) 64)))

; convenience functions
(define (change-sprite-state sprite state)
  (make-sprite (sprite-type sprite) (sprite-image sprite) (append (list (sprite-realX sprite) (sprite-realY sprite)) (sprite-more-variables sprite)) state (sprite-update sprite) (sprite-draw sprite)))

(define (change-sprite-coords sprite x y)
  (make-sprite (sprite-type sprite) (sprite-image sprite) (append (list x y) (sprite-more-variables sprite)) (sprite-state sprite) (sprite-update sprite) (sprite-draw sprite)))

(define (change-sprite-jumpspeed sprite jumpspeed)
  (make-sprite (sprite-type sprite) (sprite-image sprite) (list (sprite-realX sprite) (sprite-realY sprite) jumpspeed) (sprite-state sprite) (sprite-update sprite) (sprite-draw sprite)))

; sprite predicates
(define (player? sprite)
  (eq? (sprite-type sprite) "player"))

(define (enemy? sprite)
  (equal? (substring (sprite-type sprite) 0 5) "enemy"))

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
              (>= (+ (sprite-realX sprite) 64 speed) (* 64 (sprite-gridX sprite))))
         (change-sprite-coords sprite (* 64 (sprite-gridX sprite)) (sprite-realY sprite)))
        
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1)))
              (>= (+ (sprite-realX sprite) 64 speed) (* 64 (sprite-gridX sprite)))
              (> (sprite-realY sprite) (* 64 (sprite-gridY sprite))))
         (change-sprite-coords sprite (* 64 (sprite-gridX sprite)) (sprite-realY sprite)))
        
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 2) (sprite-gridY sprite)))
              (>= (+ (sprite-realX sprite) 64 speed) (* 64 (+ (sprite-gridX sprite) 2))))
         (change-sprite-coords sprite (* 64 (+ (sprite-gridX sprite) 1)) (sprite-realY sprite)))
        
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 2) (+ (sprite-gridY sprite) 1)))
              (>= (+ (sprite-realX sprite) 64 speed) (* 64 (+ (sprite-gridX sprite) 2))))
         (change-sprite-coords sprite (* 64 (+ (sprite-gridX sprite) 1)) (sprite-realY sprite)))
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
           (change-sprite-coords sprite (sprite-realX sprite) (* 64 (+ (sprite-gridY sprite) 1)))
           "stand-right")
          16))
        
        ((and (< (sprite-jumpspeed sprite) 0)
              (>= (- (sprite-realY sprite) (sprite-jumpspeed sprite)) (* (+ (sprite-gridY sprite) 1) 64))
              (= (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
              (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 2))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (* 64 (+ (sprite-gridY sprite) 1)))
           "stand-right")
          16))
        
        ((and (< (sprite-jumpspeed sprite) 0) 
              (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (* 64 (sprite-gridY sprite)))
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
                                          (< (sprite-realX (car player)) (+ (sprite-realX sprite) 64)) 
                                          (> (sprite-realY (car player)) (- (sprite-realY sprite) 64))
                                          (< (sprite-realY (car player)) (+ (sprite-realY sprite) 64)))
                                     #f))
                sprites))))

; PLAYER UPDATE PROCEDURE
(define (player-update-proc sprite)
  (let* ((new-sprite (left-or-right sprite)))
    (cond ; FALL
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
           (list "pop-me" 
                 (make-sprite "projectile" (circle 10 "solid" "red") (list (+ (sprite-realX sprite) 20) (sprite-realY sprite) 20) "start" projectile-1-and-up sprite-display-image)
                 (make-sprite "projectile" (circle 10 "solid" "blue") (list (+ (sprite-realX sprite) 20) (+ (sprite-realY sprite) 20) 20) "start" projectile-1 sprite-display-image)
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

(define (sprite-decay-update sprite)
  (if (<= (sprite-jumpspeed sprite) 0)
      (list "kill-me")
      (change-sprite-jumpspeed sprite (- (sprite-jumpspeed sprite) 1))))

(define (projectile-1 sprite)
  (sprite-decay-update (move-right sprite 4)))
(define (projectile-1-and-up sprite)
  (sprite-decay-update (move-right (change-sprite-coords sprite (sprite-realX sprite) (- (sprite-realY sprite) .5)) 4)))
(define (projectile-2 sprite)
  (sprite-decay-update (move-left sprite 4)))

; ENEMY UPDATE PROCEDUREs
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
                         (list (sprite-realX sprite) (sprite-realY sprite) 100) "move-left" projectile-2 sprite-display-image))
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
        (else (sprite-image sprite))))

(define (sprite-display-image sprite)
  (sprite-image sprite))

(define arrow-image (bitmap "images/sprites/arrow.png"))
(define player-stand-right (bitmap "images/sprites/player1.png"))
(define player-jump-right-1 (bitmap "images/sprites/player-jump-right-1.png"))
(define player-jump-right-2 (bitmap "images/sprites/player-jump-right-2.png"))
;(define player-sprites (list player-stand-right player-stand-left))

(define sprite-list-one   (list (make-sprite "arrow" arrow-image '(210 225) "start" sprite-null-update sprite-display-image)))
(define sprite-list-two   (list (make-sprite "arrow" arrow-image '(210 257) "continue" sprite-null-update sprite-display-image)))
(define sprite-list-three (list (make-sprite "player" player-stand-right '(320 256 16) "stand-right" player-update-proc player-draw-proc)
                                (make-sprite "enemy-1" (rectangle 45 45 "solid" "red") '(590 30) "fly-up" enemy-one-update-proc sprite-display-image)
                                (make-sprite "enemy-2" (rectangle 45 45 "solid" "blue") '(320 30) "fly-left" enemy-two-update-proc sprite-display-image)
                                (make-sprite "enemy-3" (rectangle 64 64 "solid" "purple") '(256 320) "walk-left" enemy-three-update-proc sprite-display-image)
                                (make-sprite "enemy-4" (rectangle 64 64 "solid" "white") '(576 192 120) "walk-left" enemy-four-update-proc sprite-display-image)
                                ))


(define (draw-sprites sprite-list img)
  (foldr (lambda (x y) (place-image/align ((sprite-draw x) x) (sprite-realX x) (sprite-realY x) 'left 'top y)) 
         img
         sprite-list))

;******************************************************


;******************** MAP STUFF ***********************

(define map-one (list (list w1 ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai)
                      (list w1 ai b1 ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai d2 ai ai ai)
                      (list w1 ai ai ai ai ai b1 ai ai b1)
                      (list w1 ai ai d1 d1 b1 ai ai d2 ai)
                      (list g2 g1 g1 g1 g1 g1 g1 g1 g1 g1)))

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
  (cond ((= map-num 1) map-one)
        ((= map-num 2) map-two)
        ((= map-num 3) map-three)
        (else map-one)))

(define (draw-map my-map img tile-offset)
  (define (draw-map-help the-map x-max y-max result)
    (cond ((null? the-map) result)
          ((null? (car the-map)) (draw-map-help (cdr the-map) x-max y-max result))
          (else (draw-map-help (append (list (cdr (car the-map))) (cdr the-map)) x-max y-max 
                      (place-image/align (draw-tile (car (car the-map)))
                                         (- (* (- x-max (length (car the-map))) 64) tile-offset)
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
(define get-tile-offset fifth)
(define get-health sixth)

(define (change-world-sprites y sprites)
  (make-world (get-state y) (get-map y) sprites (get-bg y) (get-tile-offset y) (get-health y)))

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
            [on-key key-down]
            [on-release key-release]))

(define (update y)
  ; update all sprites
  (if (eq? (get-state y) "pause-menu")
      ; PAUSE (we'll need to save sprite list somehow)
      (make-world "pause-menu" (get-map y) (get-sprites y) (get-bg y) (get-tile-offset y) (get-health y))
      (let ((new-sprites (foldr (lambda (x y) (cond ((eq? (car x) "kill-me") y)
                                                    ((eq? (car x) "pop-me") (append (cdr x) y))
                                                    (else (cons x y)))) 
                                nil
                                (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-sprites y)))))
        (if (enemy-collision new-sprites)
          (make-world (get-state y) (get-map y) new-sprites (get-bg y) 0 (- (get-health y) 1))
          (make-world (get-state y) (get-map y) new-sprites (get-bg y) 0 (get-health y))))))


(define (draw y)
  (cond ((eq? (get-state y) "playing")
         (let ((health-width (if (> (get-health y) 0) (get-health y) 0)))
           ; draw background, then draw map into the background, then draw sprites into map, then draw healthbar onto the whole thing
           (place-image/align (rectangle (* health-width 2) 20 "solid" "red") 20 20 'left 'top
                              (draw-sprites (get-sprites y)
                                            (draw-map (choose-map (get-map y)) (get-bg y) (get-tile-offset y))))))
        ((eq? (get-state y) "pause-menu")
         pause-menu)
        ; else case is Title Screen
        (else (draw-sprites (get-sprites y) (get-bg y)))))

; KEY LIST
; should we do this another way?
(define left-button #f)
(define up-button #f)
(define right-button #f)
(define down-button #f)
(define z-button #f)
(define x-button #f)
(define rshift-button #f)

; KEY DOWN EVENTS
(define (key-down y ke)
  (cond ((eq? (get-state y) "playing") (playing-keys-down y ke))
        ((eq? (get-state y) "pause-menu") (pause-menu-keys y ke))
        (else (title-screen-keys-down y ke))))

(define (playing-keys-down y ke)
  (cond ((key=? ke "left")  (set! left-button #t))
        ((key=? ke "up")  (set! up-button #t))
        ((key=? ke "right")  (set! right-button #t))
        ((key=? ke "down")  (set! down-button #t))
        ((key=? ke "z")  (set! z-button #t))
        ((key=? ke "x")  (set! x-button #t))
        ((key=? ke "rshift") (set! rshift-button #t)))
  y)

(define (title-screen-keys-down y ke)
  (cond ((and (or (key=? ke "down") (key=? ke "up")) (memf (lambda (x) (is-state? x "start")) (get-sprites y)))
         (make-world (get-state y) (get-map y) sprite-list-two (get-bg y) (get-tile-offset y) (get-health y)))
        ((and (or (key=? ke "down") (key=? ke "up")) (memf (lambda (x) (is-state? x "continue")) (get-sprites y)))
         (make-world (get-state y) (get-map y) sprite-list-one (get-bg y) (get-tile-offset y) (get-health y)))
        ((and (key=? ke "z") (memf (lambda (x) (is-state? x "start")) (get-sprites y)))
         (make-world "playing" 1 sprite-list-three bg-1 (get-tile-offset y) (get-health y)))
        (else y)))

(define (pause-menu-keys y ke)
  (if (key=? ke "rshift")
      (make-world "playing" (get-map y) (get-sprites y) bg-1 (get-tile-offset y) (get-health y))
      y))

; KEY UP EVENTS
(define (key-release y ke)
  (cond ((key=? ke "left")  (set! left-button #f))
        ((key=? ke "up")  (set! up-button #f))
        ((key=? ke "right")  (set! right-button #f))
        ((key=? ke "down")  (set! down-button #f))
        ((key=? ke "z")  (set! z-button #f))
        ((key=? ke "x")  (set! x-button #f))
        ((key=? ke "rshift")  (set! rshift-button #f)))
  y)

(define title-screen-bg (bitmap "images/title.png"))
(define bg-1 (bitmap "images/Background-1.png"))
(define pause-menu (bitmap "images/pause-menu.png"))

(main (make-world "title screen" 0 sprite-list-one title-screen-bg 0 100))