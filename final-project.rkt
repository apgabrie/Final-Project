#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(include "keys.rkt")
(include "sprite.rkt")
(include "tile.rkt")
(include "map.rkt")

(define nil '())

(define (double-map proc items1 items2)
  (if (null? items1)
      nil
      (cons (proc (car items1) (car items2))
            (map proc (cdr items1) (cdr items2)))))

;******************************************************

; worldstate is a list in the form 
;    (state map-number sprite-list background-image map-offset tiles-on-left player-health player-experience inventory-sprite-list wait-counter)

(define make-world list)
(define get-state first)
(define get-map second)
(define get-sprites third)
(define get-bg fourth)
(define get-map-offset fifth)
(define get-tiles-on-left sixth)
(define get-health seventh)
(define get-exp eighth)
(define get-inventory-sprites ninth)
(define get-wait-counter tenth)

(define (change-world-sprites y sprites)
  (make-world (get-state y) (get-map y) sprites (get-bg y) (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y)))
(define (change-world-inventory-sprites y sprites)
  (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) sprites (get-wait-counter y)))

(define (main y)
  (big-bang y
            [on-tick update]
            [stop-when (lambda (y) (= (get-map y) 3))]
            [to-draw draw]
            [on-key keys-down]
            [on-release key-release]))

(define (side-scroll-going-right player map-offset tiles-on-left)
  (let ((new-map-offset (+ map-offset (sprite-velX player))))
    (cond ((and (>= new-map-offset 64) (< tiles-on-left (- (length (car current-map)) 11)))
           (list (- new-map-offset 64) (+ tiles-on-left 1)))
          ((and (= tiles-on-left (- (length (car current-map)) 11)) (< new-map-offset 64))
           (list new-map-offset tiles-on-left))
          ((and (= tiles-on-left (- (length (car current-map)) 11)) (>= new-map-offset 64))
           (list 64 tiles-on-left))
          ((< tiles-on-left (- (length (car current-map)) 11))
           (list new-map-offset tiles-on-left))
        (else (list map-offset tiles-on-left)))))

(define (side-scroll-going-left player map-offset tiles-on-left)
  (let ((new-map-offset (- map-offset (sprite-velX player))))
    (cond ((and (<= new-map-offset 0) (> tiles-on-left 0))
           (list (+ new-map-offset 64) (- tiles-on-left 1)))
          ((and (= tiles-on-left 0) (> new-map-offset 0))
           (list new-map-offset tiles-on-left))
          ((and (= tiles-on-left 0) (<= new-map-offset 0))
           (list 0 tiles-on-left))
          ((> tiles-on-left 0)
           (list new-map-offset tiles-on-left))
          (else (list map-offset tiles-on-left)))))

(define (side-scroll player map-offset tiles-on-left)
  (cond #|((>= (- (sprite-realX player) (+ map-offset (* 64 tiles-on-left))) 352)
         (side-scroll-going-right player map-offset tiles-on-left))
        ((< (- (sprite-realX player) (+ map-offset (* 64 tiles-on-left))) 288)
         (side-scroll-going-left player map-offset tiles-on-left))|#
        ((and (is-dir? player "right") (>= (- (sprite-realX player) (+ map-offset (* 64 tiles-on-left))) 252))
         (side-scroll-going-right player map-offset tiles-on-left))
        ((and (is-dir? player "right") (< (- (sprite-realX player) (+ map-offset (* 64 tiles-on-left))) 188))
         (side-scroll-going-left player map-offset tiles-on-left))
        ((and (is-dir? player "left") (>= (- (sprite-realX player) (+ map-offset (* 64 tiles-on-left))) 452))
         (side-scroll-going-right player map-offset tiles-on-left))
        ((and (is-dir? player "left") (< (- (sprite-realX player) (+ map-offset (* 64 tiles-on-left))) 388))
         (side-scroll-going-left player map-offset tiles-on-left))
        (else (list map-offset tiles-on-left))))

(define (update y)
  (cond ; NEXT STAGE WAIT
        ((and (eq? (get-state y) "next stage wait") (> (get-wait-counter y) 0))
         (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) (get-inventory-sprites y) (- (get-wait-counter y) 1)))
        ((and (eq? (get-state y) "next stage wait") (<= (get-wait-counter y) 0))
         (set! current-map map-two)
         (make-world "playing" (+ (get-map y) 1) sprite-list-three (get-bg y) 0 0 (get-health y) (get-exp y) (get-inventory-sprites y) 0))
        ; PAUSE BUTTON PRESS
        ((and shift-button (eq? (get-state y) "playing"))
         (set! shift-button #f)
         (make-world "pause menu" (get-map y) (get-sprites y) pause-menu (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y)))
        ((and shift-button (eq? (get-state y) "pause menu"))
         (set! shift-button #f)
         (make-world "playing" (get-map y) (get-sprites y) bg-1 (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y)))
        ; PAUSE MENU
        ((eq? (get-state y) "pause menu")
         (let* ((new-sprites (map (lambda (sprite) ((sprite-update sprite) sprite))(get-inventory-sprites y)))
               (cursor (filter cursor? new-sprites)))
           (cond ((and z-button (is-state? (car cursor) "save position"))
                  (set! z-button #f)
                  (save-world y)
                  y)
                 (else (change-world-inventory-sprites y new-sprites)))))
        ; PLAYING
        ((eq? (get-state y) "playing")
         (let* ((new-sprites (foldr (lambda (x y) (cond ((eq? (car x) "kill-me") y)
                                                        ((eq? (car x) "pop-me") (append (cdr x) y))
                                                        (else (cons x y)))) 
                                    nil
                                    (map (lambda (sprite) (if (and (> (- (sprite-realX sprite) (get-map-offset y) (* 64 (get-tiles-on-left y))) -64)
                                                                   (< (- (sprite-realX sprite) (get-map-offset y) (* 64 (get-tiles-on-left y))) 704))
                                                              ((sprite-update sprite) sprite) 
                                                              sprite))
                                           (get-sprites y))))
                (player (car (filter player? new-sprites)))
                (enemies-colliding-w-player (enemy-collision new-sprites))
                (damage (if (or (is-state? player "get hurt") (null? enemies-colliding-w-player)) 0 (sprite-damage (car enemies-colliding-w-player))))
                (player-maybe-hurt (if (null? enemies-colliding-w-player) 
                                       player 
                                       (change-sprite-velY (change-sprite-frame-counter (change-sprite-state player "get hurt") 100) 10)))
                (new-player (cond ((is-state? player-maybe-hurt "get hurt") player-maybe-hurt)
                                  ((and (not (is-state? player "jump")) (left-key-not-right-key)) (change-sprite-state (change-sprite-direction player "left") "walk"))
                                  ((and (not (is-state? player "jump")) (right-key-not-left-key)) (change-sprite-state (change-sprite-direction player "right") "walk"))
                                  ((not (is-state? player "jump")) (change-sprite-state player "stand"))
                                  (else player)))
                
                ; prevents projectiles being shoot too close together
                (new-wait-timer (cond ((and x-button (= (get-wait-counter y) 0))
                                       2)
                                      ((> (get-wait-counter y) 0)
                                       (- (get-wait-counter y) 1))
                                      (else 0)))
                
                (enemies (filter enemy? new-sprites))
                (projectiles (filter projectile? new-sprites))
                (projectiles-not-touching-enemies (filter (lambda (x) (not (sprite-collides-over-list? x enemies))) projectiles))
                (new-projectiles (if (and x-button (= (get-wait-counter y) 0))
                                     (append projectiles-not-touching-enemies (shoot-weapon player))
                                     projectiles-not-touching-enemies))
                
                (enemies-after-projectiles (if (null? projectiles)
                                               enemies
                                               (projectile-collisions projectiles enemies)))
                (items (filter item? new-sprites))
                (maybe-next-stage (filter (lambda (x) (sprites-collide? player x)) new-sprites))
                
                (side-scroll-values (side-scroll player (get-map-offset y) (get-tiles-on-left y)))
                (new-map-offset (first side-scroll-values))
                (new-tiles-on-left (second side-scroll-values)))
           (if (member "item-next-stage" maybe-next-stage (lambda (x y) (is-type? x y)))
               (make-world "next stage wait" (get-map y) new-sprites (get-bg y) new-map-offset new-tiles-on-left (get-health y) (get-exp y) (get-inventory-sprites y) 65)
               (make-world (get-state y) (get-map y) 
                           ; new sprite-list
                           (append enemies-after-projectiles new-projectiles (list new-player) items)
                           (get-bg y) new-map-offset new-tiles-on-left (- (get-health y) damage) (get-exp y) (get-inventory-sprites y) new-wait-timer))))
        ; TITLE SCREEN
        ((eq? (get-state y) "title screen")
         (let* ((new-sprites (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-sprites y)))
               (cursor (filter cursor? new-sprites)))
           (cond ((and z-button (is-state? (car cursor) "start"))
                  (set! z-button #f)
                  (make-world "playing" 1 sprite-list-two bg-1 (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y)))
                 ((and z-button (is-state? (car cursor) "continue"))
                  (set! z-button #f)
                  ;(make-world-from-save)
                  ;(continue)
                  (make-world "playing" (first continue) sprite-list-two bg-1 (get-map-offset y) (get-tiles-on-left y) (second continue) (third continue) (get-inventory-sprites y) (get-wait-counter y)))
                 (else (change-world-sprites y new-sprites)))))
        (else y)))


(define (draw y)
  (cond ; NEXT STAGE WAIT
        ((eq? (get-state y) "next stage wait")
         (let ((health-width (if (> (get-health y) 0) (get-health y) 0)))
           ; same as in playing but also go to next stage picture
           (place-image/align next-stage-picture
                              120 50 'left 'top
                              (draw-sprites (get-sprites y)
                                            (draw-map (choose-map (get-map y) (get-tiles-on-left y)) (get-bg y) (get-map-offset y)) (+ (get-map-offset y) (* 64 (get-tiles-on-left y)))))))
        ; PLAYING
        ((eq? (get-state y) "playing")
         (let ((health-width (if (> (get-health y) 0) (get-health y) 0)))
           ; draw background, then draw map into the background, then draw sprites into map, then draw HUD onto the whole thing
           (place-image/align (place-image/align (place-image/align expbar 0 0 'left 'top (empty-scene (* 1.37 (- 100 health-width)) 26))
                                                 103 50 'left 'top
                                                 (place-image/align (place-image/align healthbar 0 0 'left 'top (empty-scene (* 1.37 health-width) 26))
                                                                    103 16 'left 'top
                                                                    HUD))
                              10 10 'left 'top
                              (draw-sprites (get-sprites y)
                                            (draw-map (choose-map (get-map y) (get-tiles-on-left y)) (get-bg y) (get-map-offset y)) (+ (get-map-offset y) (* 64 (get-tiles-on-left y)))))))
        ; PAUSE MENU
        ((eq? (get-state y) "pause menu")
         (place-image/align (text "A basic spell. Shoots a bubble to hurt enemies." 24 'white) 50 365 'left 'top 
                            (draw-sprites (get-inventory-sprites y) pause-menu 0)))
        ; else case is Title Screen
        (else (draw-sprites (get-sprites y) (get-bg y) 0))))

(define title-screen-bg (bitmap "images/title.png"))
(define bg-1 (bitmap "images/Background-1.png"))
(define bg-2 (bitmap "images/Background-2.png"))
(define pause-menu (bitmap "images/menu/pause-menu.png"))

(define healthbar (bitmap "images/healthbar.png"))
(define expbar (bitmap "images/expbar.png"))
(define HUD (bitmap "images/HUD.png"))
(define next-stage-picture (bitmap "images/next-stage.png"))

(define (save-world y)
  (let ((map    (get-map y))
        (health (get-health y))
        (exp    (get-exp y)))
  (write-file "save.txt"
              (string-append (number->string map) "\n" (number->string health) "\n" (number->string exp) ))))

(define continue
    (map car (read-words-and-numbers/line "save.txt")))

(main (make-world "title screen" 0 sprite-list-one title-screen-bg 0 0 100 0 (list menu-cursor-pos-1) 0))


