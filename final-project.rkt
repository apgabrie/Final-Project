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
(define (get-weapon y)
  (caddr (cddddr (cddddr y))))

(define (change-world-sprites y sprites)
  (make-world (get-state y) (get-map y) sprites (get-bg y) (get-map-offset y) (get-tiles-on-left y) 
              (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y) (get-weapon y)))
(define (change-world-inventory-sprites y sprites)
  (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) 
              (get-health y) (get-exp y) sprites (get-wait-counter y) (get-weapon y)))
(define (change-world-wait-counter y count)
  (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) 
              (get-health y) (get-exp y) (get-inventory-sprites y) count (get-weapon y)))
(define (change-world-weapon y weapon)
  (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) 
              (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y) weapon))

(define (main y)
  (big-bang y
            [on-tick update]
            [stop-when (lambda (y) (or (= (get-map y) 5)
                                       (eq? (get-state y) "game over")))]
            [to-draw draw]
            [on-key keys-down]
            [on-release key-release]
            [name "Henriette the Witch"]))

(define (side-scroll-going-right player map-offset tiles-on-left)
  (let ((new-map-offset (+ map-offset 7)))
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
  (let ((new-map-offset (- map-offset 7)))
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
  (cond ; GAME OVER
        ((and (<= (get-health y) 0) (eq? (get-state y) "playing"))
         (make-world "game over" (get-map y) '() game-over (get-map-offset y) (get-tiles-on-left y) (get-health y) (get-exp y) (get-inventory-sprites y)
                     (get-wait-counter y) (get-weapon y)))
        ; NEXT STAGE WAIT
        ((and (eq? (get-state y) "next stage wait") (> (get-wait-counter y) 0))
         (make-world (get-state y) (get-map y) (get-sprites y) (get-bg y) (get-map-offset y) (get-tiles-on-left y) 
                     (get-health y) (get-exp y) (get-inventory-sprites y) (- (get-wait-counter y) 1) (get-weapon y)))
        ((and (eq? (get-state y) "next stage wait") (<= (get-wait-counter y) 0))
         (set! current-map (map-by-number (+ (get-map y) 1)))
         (let ((new-spell (cond ((= (get-map y) 1) (list the-burst-spell))
                                ((= (get-map y) 2) (set! have-dbl-jmp-item #t)
                                                   (list the-magic-feather))
                                (else nil))))
           (make-world "playing" (+ (get-map y) 1) (sprite-list-by-map-number (+ (get-map y) 1)) (get-bg y) 0 0 
                       (get-health y) (get-exp y) (append (get-inventory-sprites y) new-spell) 0 (get-weapon y))))
        ; PAUSE BUTTON PRESS
        ((and shift-button (eq? (get-state y) "playing"))
         (set! shift-button #f)
         (make-world "pause menu" (get-map y) (get-sprites y) pause-menu (get-map-offset y) (get-tiles-on-left y) 
                     (get-health y) (get-exp y) (get-inventory-sprites y) 0 (get-weapon y)))
        ((and shift-button (eq? (get-state y) "pause menu"))
         (set! shift-button #f)
         (make-world "playing" (get-map y) (get-sprites y) bg-1 (get-map-offset y) (get-tiles-on-left y) 
                     (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y) (get-weapon y)))
        ; PAUSE MENU UPDATE
        ((eq? (get-state y) "pause menu")
         (let* ((new-sprites (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-inventory-sprites y)))
               (cursor (filter cursor? new-sprites))
               (spells (filter spell? (get-inventory-sprites y))))
           (cond ((> (get-wait-counter y) 0)
                  (change-world-wait-counter y (- (get-wait-counter y) 1)))
                 ((and z-button (is-state? (car cursor) "save position"))
                  (set! z-button #f)
                  (save-world y)
                  (change-world-wait-counter y 40))
                 ((and z-button (is-state? (car cursor) "back position"))
                  (set! z-button #f)
                  (make-world "playing" (get-map y) (get-sprites y) bg-1 (get-map-offset y) (get-tiles-on-left y) 
                              (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y) (get-weapon y)))
                 ((and z-button (is-state? (car cursor) "position spell 1"))
                  (set! z-button #f)
                  (change-world-weapon y 1))
                 ((and z-button (>= (length spells) 2) (is-state? (car cursor) "position spell 2"))
                  (set! z-button #f)
                  (change-world-weapon y 2))
                 ((and z-button (>= (length spells) 3) (is-state? (car cursor) "position spell 3"))
                  (set! z-button #f)
                  (change-world-weapon y 3))
                 (else (change-world-inventory-sprites y new-sprites)))))
        ; PLAYING UPDATE
        ((eq? (get-state y) "playing")
         (let* ((new-sprites (foldr (lambda (x y) (cond ((is-type? "kill-me" x) y)
                                                        ((is-type? "pop-me" x) (append (cdr x) y))
                                                        (else (cons x y)))) 
                                    nil
                                    (map (lambda (sprite) (if (and (> (- (sprite-realX sprite) (get-map-offset y) (* 64 (get-tiles-on-left y))) -64)
                                                                   (< (- (sprite-realX sprite) (get-map-offset y) (* 64 (get-tiles-on-left y))) 704))
                                                              ((sprite-update sprite) sprite) 
                                                              sprite))
                                           (get-sprites y))))
                (player (car (filter player? new-sprites)))
                (enemies-colliding-w-player (enemy-collision new-sprites))
                (damage (cond ((is-state? player "place me") (null? enemies-colliding-w-player)
                               30)
                              ((or (is-state? player "get hurt") (null? enemies-colliding-w-player) (> (player-invincibility-counter player) 0)) 
                               0)
                              (else (sprite-damage (car enemies-colliding-w-player)))))
                (player-maybe-hurt (if (and (not (null? enemies-colliding-w-player)) (= (player-invincibility-counter player) 0))
                                       (change-player-invincibility-counter
                                        (change-sprite-velY (change-sprite-state player "get hurt") 5)
                                        80)
                                       player))
                (new-player (cond ((is-state? player-maybe-hurt "place me")
                                   (let ((check-point (find-checkpoint current-map (sprite-gridX player))))
                                     (change-player-invincibility-counter
                                      (change-sprite-state
                                       (change-sprite-coords player 
                                                             (* 64 (car check-point))
                                                             (* 64 (cadr check-point)))
                                       "stand")
                                      80)))
                                  ((is-state? player-maybe-hurt "get hurt") player-maybe-hurt)
                                  ((and (not (is-state? player "jump")) (left-key-not-right-key)) (change-sprite-state (change-sprite-direction player "left") "walk"))
                                  ((and (not (is-state? player "jump")) (right-key-not-left-key)) (change-sprite-state (change-sprite-direction player "right") "walk"))
                                  ((not (is-state? player "jump")) 
                                   (set! double-jumpable? #f)
                                   (change-sprite-state player "stand"))
                                  (else player)))
                
                ; prevents projectiles being shot too close together
                (new-wait-timer (cond ((and x-button (= (get-weapon y) 1) (= (get-wait-counter y) 0))
                                       3)
                                      ((and x-button (= (get-weapon y) 2) (= (get-wait-counter y) 0))
                                       60)
                                      ((> (get-wait-counter y) 0)
                                       (- (get-wait-counter y) 1))
                                      (else 0)))
                
                (enemies (filter enemy? new-sprites))
                (projectiles (filter projectile? new-sprites))
                (projectiles-after-collision (map (lambda (x) (if (and (not (= (sprite-damage x) 0)) (sprite-collides-over-list? x enemies))
                                                                  (change-sprite-frame-counter x 0)
                                                                  x))
                                                  projectiles))
                (new-projectiles (if (and x-button (= (get-wait-counter y) 0))
                                     (append projectiles-after-collision
                                             (shoot-weapon player (get-exp y) (get-weapon y)))
                                     projectiles-after-collision))
                
                (enemies-after-projectiles (if (null? projectiles)
                                               enemies
                                               (projectile-collisions projectiles enemies)))
                (items (filter item? new-sprites))
                (new-items (filter (lambda (x) (not (sprites-collide? x player))) items))
                (items-colliding-w-player (filter (lambda (x) (sprites-collide? x player)) items))
                (new-inventory-items (if (eq? nil items-colliding-w-player)
                                         nil
                                         (map (lambda (x) (change-sprite-coords
                                                           (change-sprite-type x (substring (sprite-type x) 5 (string-length (sprite-type x))))
                                                           (+ 90 (* 139 (length (filter (lambda (x) (not (or (spell? x) (cursor? x)))) (get-inventory-sprites y)))))
                                                           262))
                                              items-colliding-w-player)))
                
                (exp (filter exp? new-sprites))
                (new-exp-items (filter (lambda (x) (not (sprites-collide? x player))) exp))
                (new-exp (foldr (lambda (x y) (+ (sprite-damage x) y)) 0 (filter (lambda (x) (sprites-collide? x player)) exp)))
                
                (maybe-next-stage (filter (lambda (x) (sprites-collide? player x)) new-sprites))
                
                (side-scroll-values (side-scroll player (get-map-offset y) (get-tiles-on-left y)))
                (new-map-offset (first side-scroll-values))
                (new-tiles-on-left (second side-scroll-values)))
           (if (member "item-next-stage" maybe-next-stage (lambda (x y) (is-type? x y)))
               (make-world "next stage wait" (get-map y) new-sprites (get-bg y) new-map-offset new-tiles-on-left 
                           (get-health y) (get-exp y) (get-inventory-sprites y) 65 (get-weapon y))
               (make-world (get-state y) (get-map y) 
                           ; new sprite-list
                           (append enemies-after-projectiles new-projectiles (list new-player) new-items new-exp-items)
                           (get-bg y) new-map-offset new-tiles-on-left (- (get-health y) damage) 
                           (+ (get-exp y) new-exp) (append (get-inventory-sprites y) new-inventory-items) new-wait-timer (get-weapon y)))))
        ; TITLE SCREEN UPDATE
        ((eq? (get-state y) "title screen")
         (let* ((new-sprites (map (lambda (sprite) ((sprite-update sprite) sprite)) (get-sprites y)))
               (cursor (filter cursor? new-sprites)))
           (cond ((and z-button (is-state? (car cursor) "start"))
                  (set! z-button #f)
                  (make-world "playing" 1 sprite-list-one bg-1 (get-map-offset y) (get-tiles-on-left y) 
                              (get-health y) (get-exp y) (get-inventory-sprites y) (get-wait-counter y) (get-weapon y)))
                 ((and z-button (is-state? (car cursor) "continue"))
                  (set! z-button #f)
                  (set! current-map (map-by-number (second (continue y))))

                  (continue y))
                 (else (change-world-sprites y new-sprites)))))
        (else y)))


(define (draw y)
  (cond ; NEXT STAGE WAIT DRAW
        ((eq? (get-state y) "next stage wait")
         (let ((next-level-string-1 (cond ((= (get-map y) 1)
                                         "You got a new spell!")
                                        ((= (get-map y) 2)
                                         "You got magic feather!")))
               (next-level-string-2 (cond ((= (get-map y) 1)
                                         "Equip it in the pause menu.")
                                        ((= (get-map y) 2)
                                         "You can now double jump."))))
           ; same as in playing but also go to next stage picture
           (place-image/align 
            (text next-level-string-1 27 'white) 198 120 'left 'top
            (place-image/align (text next-level-string-2 27 'white) 150 180 'left 'top
                               (place-image/align next-stage-picture
                                                  120 50 'left 'top
                                                  (draw-sprites (get-sprites y)
                                                                (draw-map (choose-map (get-map y) (get-tiles-on-left y)) (get-bg y) (get-map-offset y)) (+ (get-map-offset y) (* 64 (get-tiles-on-left y)))))))))
        ; PLAYING DRAW
        ((eq? (get-state y) "playing")
         (let ((health-width (if (> (get-health y) 0) (get-health y) 0))
               (weapon-image (cond ((= (get-weapon y) 1)
                                    bubble-spell)
                                   ((= (get-weapon y) 2)
                                    burst-spell)
                                   (else (empty-scene 0 0)))))
           ; draw background, then draw map into the background, then draw sprites into map, then draw HUD onto the whole thing
           (place-image/align 
            weapon-image 32 32 'left 'top
            (place-image/align (make-HUD y health-width)
                               10 10 'left 'top
                               (draw-sprites (get-sprites y)
                                             (draw-map (choose-map (get-map y) (get-tiles-on-left y)) (get-bg y) (get-map-offset y)) (+ (get-map-offset y) (* 64 (get-tiles-on-left y))))))))
        ; PAUSE MENU DRAW
        ((eq? (get-state y) "pause menu")
         (let* ((cursor (car (filter cursor? (get-inventory-sprites y))))
                (spells (filter spell? (get-inventory-sprites y)))
                (items (filter (lambda (x) (not (or (spell? x) (cursor? x)))) (get-inventory-sprites y)))
                (name (item-name-from-cursor-state (sprite-state cursor) spells items))
                (item-text (item-text-from-cursor-state (sprite-state cursor) spells items))
                (game-saved-message (if (> (get-wait-counter y) 0)
                                        game-saved
                                        (empty-scene 0 0))))
           (place-image/align game-saved-message  105 167 'left 'top
                              (place-image/align equip-image (+ (* 139 (- (get-weapon y) 1)) 131) 157 'left 'top
                                                 (place-image/align (text name 20 'black) 85 360 'left 'top 
                                                                    (place-image/align (text item-text 20 'black) 60 390 'left 'top 
                                                                                       (draw-sprites (get-inventory-sprites y) pause-menu 0)))))))
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
(define game-saved (bitmap "images/menu/game-saved.png"))
(define game-over (bitmap "images/game-over.png"))

(define (make-HUD y health-width)
  (place-image/align 
   (text (number->string (floor (/ (get-exp y) 60))) 17 (color 57 0 29 255))
   87 52 'left 'top
   (place-image/align (place-image/align expbar 0 0 'left 'top (empty-scene (* 1.72 (modulo (get-exp y) 60)) 24))
                                        115 49 'left 'top
                                        (place-image/align (place-image/align healthbar 0 0 'left 'top (empty-scene (* 1.03 health-width) 24))
                                                           115 16 'left 'top
                                                           HUD))))

(define (save-world y)
  (write-file "save.txt"
              (string-append 
               (number->string (get-map y)) "\n" 
               (number->string (get-health y)) "\n" 
               (number->string (get-exp y)) "\n"
               (string-append* (cdr (append* (map (lambda (x) (list "\n" x))
                                     (map car (get-inventory-sprites y)))))))))

(define (continue y)
  (let* ((saved-variables (read-lines "save.txt"))
         (map (string->number (first saved-variables)))
         (health (string->number (second saved-variables)))
         (exp (string->number (third saved-variables)))
         (inventory-items (drop saved-variables 3))
         (inventory-sprite-list-init (list menu-cursor-pos-1
                                           (make-sprite "Bubble" bubble-spell '(91 119) "A simple spell. Shoots a bubble to hurt enemies." sprite-null-update sprite-display-image 64 64 0 "left"))) )
    
    (define (inventory-sprite-list-final inv-items ac)
      ;;; check for items later...
      (cond ((not (eq? (findf (lambda (x) (equal? x "Burst")) inv-items) #f))
             (inventory-sprite-list-final (filter (lambda (x) (not (equal? x "Burst"))) inv-items) (append ac
                                                                                                              (list (make-sprite "Burst" burst-spell '(230 119) "Shoots a sphere that burst in all directions." sprite-null-update sprite-display-image 64 64 0 "left")))))
            #|
            ((not (eq? (findf (lambda (x) (equal? x "Weapon 3")) inv-items) #f))
             (inventory-sprite-list-final (filter (lambda (x) (not (equal? x "Weapon 3"))) inv-items) (append ac
                                                                                                              (list (make-sprite "Weapon 3" bubble-spell '(369 119) "The third weapon." sprite-null-update sprite-display-image 64 64 0 "left")))))|#
            (else ac)))
    
           (make-world "playing" map (sprite-list-by-map-number map) bg-1 (get-map-offset y) (get-tiles-on-left y) health exp (inventory-sprite-list-final inventory-items inventory-sprite-list-init) (get-wait-counter y) (get-weapon y))))


(main (make-world "title screen" 0 title-sprite-list title-screen-bg 0 0 100 0 inventory-list-one 0 1))


