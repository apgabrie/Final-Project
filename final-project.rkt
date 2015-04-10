#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(include "tile-map.rkt")

(define nil '())

;****************** SPRITE STUFF **********************

(define make-sprite list)
(define sprite-type first)
(define sprite-image second)
(define (sprite-realX sprite) (car (third sprite)))
(define (sprite-realY sprite) (cadr (third sprite)))
(define (sprite-more-variables sprite) (cddr (third sprite)))
(define (sprite-jumpspeed sprite) (car (sprite-more-variables sprite)))
(define sprite-state fourth)
(define sprite-update fifth)
(define sprite-draw sixth)

(define (sprite-gridX sprite)
  (floor (/ (sprite-realX sprite) 64)))
(define (sprite-gridY sprite)
  (floor (/ (sprite-realY sprite) 64)))

; For detecting collision with walls
(define (move-left sprite)
  (cond ((= 1 (tile-at-xy current-map (sprite-gridX sprite) (sprite-gridY sprite)))
         sprite)
        ((and (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1)))
              (> (sprite-realY sprite) (* 64 (sprite-gridY sprite))))
         sprite)
        (else (change-sprite-coords sprite (- (sprite-realX sprite) 6) (sprite-realY sprite)))))
(define (move-right sprite)
  (cond ((= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (sprite-gridY sprite)))
         sprite)
        ((and (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1)))
              (> (sprite-realY sprite) (* 64 (sprite-gridY sprite))))
         sprite)
        (else (change-sprite-coords sprite (+ (sprite-realX sprite) 6) (sprite-realY sprite)))))
(define (fall sprite)
  (change-sprite-coords sprite (sprite-realX sprite) (- (sprite-realY sprite) (sprite-jumpspeed sprite))))

; convenience functions
(define (change-sprite-state sprite state)
  (make-sprite (sprite-type sprite) (sprite-image sprite) (append (list (sprite-realX sprite) (sprite-realY sprite)) (sprite-more-variables sprite)) state (sprite-update sprite)))

(define (change-sprite-coords sprite x y)
  (make-sprite (sprite-type sprite) (sprite-image sprite) (append (list x y) (sprite-more-variables sprite)) (sprite-state sprite) (sprite-update sprite)))

(define (change-sprite-jumpspeed sprite jumpspeed)
  (make-sprite (sprite-type sprite) (sprite-image sprite) (list (sprite-realX sprite) (sprite-realY sprite) jumpspeed) (sprite-state sprite) (sprite-update sprite)))

; sprite predicates
(define (player? sprite)
  (eq? (sprite-type sprite) "player"))

(define (is-type? the-type sprite)
  (eq? the-type (sprite-type sprite)))

(define (is-state? sprite the-state)
  (eq? the-state (sprite-state sprite)))

(define (sprite-null-update sprite) sprite)

; PLAYER UPDATE PROCEDURE
(define (player-update-proc sprite)
  (cond ; JUMP BUTTON PRESS
        ((and z-button
              (or (eq? (sprite-state sprite) "stand-right") (eq? (sprite-state sprite) "stand-left") 
                  (eq? (sprite-state sprite) "walk-right") (eq? (sprite-state sprite) "walk-left")))
         (change-sprite-state sprite "jump"))
        ; JUMP
        ((and (< (sprite-jumpspeed sprite) 0 )
              (or (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1)))
                  (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1))))
              (eq? (sprite-state sprite) "jump"))
         (change-sprite-coords (change-sprite-jumpspeed (change-sprite-state sprite "stand-right") 16) (sprite-realX sprite) (* 64 (sprite-gridY sprite))))
        ((and (not left-button) (not right-button) (eq? (sprite-state sprite) "jump"))
         (change-sprite-coords (change-sprite-jumpspeed sprite (- (sprite-jumpspeed sprite) 1))
                               (sprite-realX sprite) (- (sprite-realY sprite) (sprite-jumpspeed sprite))))
        ((and left-button (not right-button) (eq? (sprite-state sprite) "jump"))
         (move-left (fall (change-sprite-jumpspeed sprite (- (sprite-jumpspeed sprite) 1)))))
        ((and (not left-button) right-button (eq? (sprite-state sprite) "jump"))
         (move-right (fall (change-sprite-jumpspeed sprite (- (sprite-jumpspeed sprite) 1)))))
        ; FALL
        ((and (= 0 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1)))
              (= 0 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1)))
              (or (eq? (sprite-state sprite) "walk-right") (eq? (sprite-state sprite) "walk-left")))
         (change-sprite-jumpspeed (change-sprite-state sprite "jump") -2))
        ; WALK RIGHT BUTTON PRESS
        ((and right-button
              (or (eq? (sprite-state sprite) "stand-right") (eq? (sprite-state sprite) "stand-left") (eq? (sprite-state sprite) "walk-left")))
         (change-sprite-state sprite "walk-right"))
        ; WALK RIGHT
        ((and right-button (eq? (sprite-state sprite) "walk-right"))
         (move-right sprite))
        ; WALK LEFT BUTTON PRESS
        ((and left-button
              (or (eq? (sprite-state sprite) "stand-right") (eq? (sprite-state sprite) "stand-left") (eq? (sprite-state sprite) "walk-right")))
         (change-sprite-state sprite "walk-left"))
        ; WALK LEFT
        ((and left-button (eq? (sprite-state sprite) "walk-left"))
         (move-left sprite))
        (else sprite)))

; SPRITE UPDATE FUNCTIONS
(define (enemy-one-update-proc sprite)
  sprite)

(define arrow-image (bitmap "images/sprites/arrow.png"))
(define player-stand-right (bitmap "images/sprites/player1.png"))
(define player-stand-left (bitmap "images/sprites/player2.png"))
(define player-sprites (list player-stand-right player-stand-left))

(define sprite-list-one   (list (make-sprite "arrow" arrow-image '(210 225) "start" sprite-null-update)))
(define sprite-list-two   (list (make-sprite "arrow" arrow-image '(210 257) "continue" sprite-null-update)))
(define sprite-list-three (list (make-sprite "player" player-stand-right '(320 320 16) "stand-right" player-update-proc)
                                (make-sprite "enemy-1" (rectangle 45 45 "solid" "red") '(20 30) "fly-right" enemy-one-update-proc)))


(define (draw-sprites sprite-list img)
  (foldr (lambda (x y) (place-image/align (sprite-image x) (sprite-realX x) (sprite-realY x) 'left 'top y)) 
         img
         sprite-list))

;******************************************************


;******************** MAP STUFF ***********************

(define map-one (list (list w1 ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai d1 d1 ai ai d2 ai ai ai)
                      (list g2 g1 g1 g1 g1 g1 g1 g1 g1 g1 g1)))

(define map-two (list (list ai ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai t1 ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai t1 ai)
                      (list t1 ai ai ai ai ai t1 ai ai ai ai)
                      (list t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1)))

(define map-three (list (list ai ai ai ai ai ai ai ai ai ai ai)
                        (list ai ai ai ai ai ai ai ai ai ai t1)
                        (list ai ai ai ai ai ai ai ai ai t1 t1)
                        (list ai ai ai ai ai ai ai ai t1 t1 t1)
                        (list ai ai ai ai ai ai ai t1 t1 t1 t1)
                        (list t1 ai ai ai ai ai t1 t1 t1 t1 t1)
                        (list t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1)))

(define current-map map-two)

(define (choose-map map-num)
  (cond ((= map-num 1) map-two)
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

(define (change-world-sprites y sprites)
  (make-world (get-state y) (get-map y) sprites (get-bg y) (get-tile-offset y)))

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
  (define (new-sprites sprites) (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-sprites y)))
      (make-world (get-state y) (get-map y) (new-sprites (get-sprites y)) (get-bg y) 0))

(define (draw y)
  (cond ((eq? (get-state y) "playing")
         ; draw background, then draw map into the background, then draw sprites into map
         (draw-sprites (get-sprites y)
                       (draw-map (choose-map (get-map y)) (get-bg y) (get-tile-offset y))))
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
        (else (title-screen-keys-down y ke))))

(define (playing-keys-down y ke)
  (cond ((key=? ke "left")  (set! left-button #t))
        ((key=? ke "up")  (set! up-button #t))
        ((key=? ke "right")  (set! right-button #t))
        ((key=? ke "down")  (set! down-button #t))
        ((key=? ke "z")  (set! z-button #t))
        ((key=? ke "x")  (set! x-button #t))
        ((key=? ke "rshift")  (set! rshift-button #t)))
  y)

(define (title-screen-keys-down y ke)
  (cond ((and (or (key=? ke "down") (key=? ke "up")) (memf (lambda (x) (is-state? x "start")) (get-sprites y)))
         (make-world (get-state y) (get-map y) sprite-list-two (get-bg y) (get-tile-offset y)))
        ((and (or (key=? ke "down") (key=? ke "up")) (memf (lambda (x) (is-state? x "continue")) (get-sprites y)))
         (make-world (get-state y) (get-map y) sprite-list-one (get-bg y) (get-tile-offset y)))
        ((and (key=? ke "z") (memf (lambda (x) (is-state? x "start")) (get-sprites y)))
         (make-world "playing" 1 sprite-list-three bg-1 (get-tile-offset y)))
        (else y)))

(define title-screen-bg (bitmap "images/title.png"))
(define bg-1 (bitmap "images/Background-1.png"))

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

(main (make-world "title screen" 0 sprite-list-one title-screen-bg 0))