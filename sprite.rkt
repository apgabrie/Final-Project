
;****************** SPRITE STUFF **********************

(define make-sprite list)
(define sprite-type first)
(define sprite-image second)
(define (sprite-realX sprite) (car (third sprite)))  ;X
(define (sprite-realY sprite) (cadr (third sprite))) ;Y
(define (sprite-more-variables sprite) (cddr (third sprite))) 
(define (sprite-velX sprite) (first (sprite-more-variables sprite)))
(define (sprite-velY sprite) (second (sprite-more-variables sprite)))
(define (sprite-damage sprite) (third (sprite-more-variables sprite)))
(define (sprite-health sprite) (fourth (sprite-more-variables sprite)))
(define sprite-state fourth)
(define sprite-update fifth)
(define sprite-draw sixth)
(define sprite-width seventh)
(define sprite-height eighth)
(define sprite-frame-counter ninth)
(define sprite-direction tenth)

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
               (sprite-frame-counter sprite)
               (sprite-direction sprite)))

(define (change-sprite-coords sprite x y)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list x y) (sprite-more-variables sprite)) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)
               (sprite-direction sprite)))

(define (change-sprite-velX sprite speed)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite) speed) (cdr (sprite-more-variables sprite))) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)
               (sprite-direction sprite)))

(define (change-sprite-jumpspeed sprite jumpspeed)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite) (sprite-velX sprite) jumpspeed) (cddr (sprite-more-variables sprite))) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)
               (sprite-direction sprite)))

(define (change-sprite-frame-counter sprite count)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite)) (sprite-more-variables sprite)) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               count
               (sprite-direction sprite)))

(define (change-sprite-health sprite new-health)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite) (sprite-velX sprite) (sprite-velY sprite) (sprite-damage sprite) new-health) (cddddr (sprite-more-variables sprite))) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)
               (sprite-direction sprite)))

(define (change-sprite-direction sprite direction)
  (make-sprite (sprite-type sprite) 
               (sprite-image sprite) 
               (append (list (sprite-realX sprite) (sprite-realY sprite)) (sprite-more-variables sprite)) 
               (sprite-state sprite) 
               (sprite-update sprite) 
               (sprite-draw sprite)
               (sprite-width sprite)
               (sprite-height sprite)
               (sprite-frame-counter sprite)
               direction))

; sprite predicates
(define (player? sprite)
  (is-type? "player" sprite))

(define (enemy? sprite)
  (equal? (substring (sprite-type sprite) 0 5) "enemy"))

(define (item? sprite)
  (equal? (substring (sprite-type sprite) 0 4) "item"))

(define (projectile? sprite)
  (is-type? "projectile" sprite))

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
        ((and (> (sprite-velY sprite) 0) 
              (<= (- (sprite-realY sprite) (sprite-velY sprite)) (* (sprite-gridY sprite) 64))
              (or (= 1 (tile-at-xy current-map (sprite-gridX sprite) (- (sprite-gridY sprite) 1)))
                  (and (> (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
                       (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (- (sprite-gridY sprite) 1))))))
         (change-sprite-jumpspeed 
          (change-sprite-coords sprite (sprite-realX sprite) (* 64 (sprite-gridY sprite)))
          0))
        ; HITTING FLOOR
        ((and (< (sprite-velY sprite) 0)
              (>= (- (sprite-realY sprite) (sprite-velY sprite)) (* (+ (sprite-gridY sprite) 1) 64))
              (> (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
              (or (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 2)))
                  (= 1 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 2)))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (- (* 64 (+ (sprite-gridY sprite) 2)) (sprite-height sprite)))
           "stand-right")
          16))
        
        ((and (< (sprite-velY sprite) 0)
              (>= (- (sprite-realY sprite) (sprite-velY sprite)) (* (+ (sprite-gridY sprite) 1) 64))
              (= (sprite-realX sprite) (* 64 (sprite-gridX sprite)))
              (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 2))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (- (* 64 (+ (sprite-gridY sprite) 2)) (sprite-height sprite)))
           "stand-right")
          16))
        
        ((and (< (sprite-velY sprite) 0) 
              (= 1 (tile-at-xy current-map (sprite-gridX sprite) (+ (sprite-gridY sprite) 1))))
         (change-sprite-jumpspeed 
          (change-sprite-state 
           (change-sprite-coords sprite (sprite-realX sprite) (- (* 64 (+ (sprite-gridY sprite) 1)) (sprite-height sprite)))
           "stand-right")
          16))
        (else (change-sprite-jumpspeed 
               (change-sprite-coords sprite (sprite-realX sprite) (- (sprite-realY sprite) (sprite-velY sprite))) 
               (- (sprite-velY sprite) 1)))))

; SPRITE COLLISION
(define (sprites-collide? sprite1 sprite2)
  (and (> (sprite-realX sprite1) (- (sprite-realX sprite2) (sprite-width sprite1)))
       (< (sprite-realX sprite1) (+ (sprite-realX sprite2) (sprite-width sprite2))) 
       (> (sprite-realY sprite1) (- (sprite-realY sprite2) (sprite-height sprite1)))
       (< (sprite-realY sprite1) (+ (sprite-realY sprite2) (sprite-height sprite2)))))

(define (sprite-collides-over-list? sprite sprite-list)
  (foldr (lambda (x y) (or (sprites-collide? sprite x) y)) #f sprite-list))

; player collision with enemies
(define (enemy-collision sprites)
  (let ((player (filter player? sprites)))
    (filter list?
            (map (lambda (sprite) (if (and (enemy? sprite)
                                           (> (sprite-realX (car player)) (- (sprite-realX sprite) 64))
                                           (< (sprite-realX (car player)) (+ (sprite-realX sprite) (sprite-width sprite))) 
                                           (> (sprite-realY (car player)) (- (sprite-realY sprite) 64))
                                           (< (sprite-realY (car player)) (+ (sprite-realY sprite) (sprite-height sprite))))
                                      sprite
                                      #f))
                 sprites))))

(define (projectile-collisions the-projectiles target-sprites)
  (define (help-me the-projectile-list result)
    (if (null? the-projectile-list)
        result
        (help-me (cdr the-projectile-list) (double-map (lambda (x y) (if (sprites-collide? (car the-projectile-list) x)
                                                                         (+ (sprite-damage (car the-projectile-list)) y)
                                                                         (+ 0 y)))
                                                       target-sprites result))))
  (double-map (lambda (x y) (change-sprite-health y (- (sprite-health y) x))) 
              (help-me the-projectiles (make-list (length target-sprites) 0))
              target-sprites))


(define (left-or-right sprite)
  (cond ((and left-button (< (sprite-velX sprite) 7)) (change-sprite-velX (move-left sprite (sprite-velX sprite)) (+ (sprite-velX sprite) 1)))
        ((and left-button (>= (sprite-velX sprite) 7)) (move-left sprite (sprite-velX sprite)))
        ((and right-button (< (sprite-velX sprite) 7)) (change-sprite-velX (move-right sprite (sprite-velX sprite)) (+ (sprite-velX sprite) 1)))
        ((and right-button (>= (sprite-velX sprite) 7)) (move-right sprite (sprite-velX sprite)))
        ((> (sprite-velX sprite) 7) (change-sprite-velX sprite (- (sprite-velX sprite) 1)))
        (else sprite)))

; PLAYER UPDATE PROCEDURE
(define (player-update-proc sprite)
  (let* ((new-sprite-l-r (left-or-right sprite))
         (new-sprite (if (>= (sprite-frame-counter new-sprite-l-r) 48)
                         (change-sprite-frame-counter new-sprite-l-r 0)
                         (change-sprite-frame-counter new-sprite-l-r (+ (sprite-frame-counter new-sprite-l-r) 1)))))
    (cond ; GET HURT
          ((and (is-state? sprite "get hurt") (> (sprite-frame-counter sprite) 0))
           (change-sprite-frame-counter (jump (move-left sprite 7)) (- (sprite-frame-counter sprite) 1)))
          ((and (is-state? sprite "get hurt") (= (sprite-frame-counter sprite) 0))
           (change-sprite-jumpspeed (change-sprite-state sprite "stand-right") 16))
          ; LAND IN PIT
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
                 (make-sprite "projectile" bubble (list (+ (sprite-realX sprite) 20) (+ (sprite-realY sprite) 20) 0 0 1)
                              "start" weapon-one-update sprite-display-image 20 20 40 "left")
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
  (let* ((new-sprite (sprite-decay-update (move-right sprite 8))))
    (if (or (eq? (car new-sprite) "kill-me") (= (sprite-realX new-sprite) (sprite-realX sprite)))
        (make-sprite "projectile" bubble (list (sprite-realX sprite) (sprite-realY sprite)) "pop" sprite-decay-update weapon-one-pop-draw 20 20 6 "left")
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
  (cond ((<= (sprite-health sprite) 0)
         (list "pop-me"
               (make-sprite "enemy-1" (rectangle 32 64 "solid" "blue") (list (sprite-realX sprite) (sprite-realY sprite) 0 0 10 5) "walk-right" enemy-one-update-proc sprite-display-image 32 64 0 "left")
               (make-sprite "enemy-1" (rectangle 32 64  "solid" "green") (list (+ (sprite-realX sprite) (sprite-width sprite)) (sprite-realY sprite) 0 0 10 5) "walk-right" enemy-one-update-proc sprite-display-image 32 64 0 "left")))
        ((eq? (sprite-state sprite) "walk-left")
         (if (equal? (move-left sprite 2) sprite)
             (change-sprite-state sprite "walk-right")
             (move-left sprite 2)))
        (else
         (if (= 0 (tile-at-xy current-map (+ (sprite-gridX sprite) 1) (+ (sprite-gridY sprite) 1)))
             (change-sprite-state sprite "walk-left")
             (move-right sprite 2)))))

(define (enemy-two-update-proc sprite)
  (cond ((<= (sprite-health sprite) 0)
         (list "kill-me"))
        ((<= (sprite-realX sprite) 0)
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
  (cond ((<= (sprite-health sprite) 0)
         (list "kill-me"))
        ((eq? (sprite-state sprite) "walk-left")
         (if (equal? (move-left sprite 2) sprite)
             (change-sprite-state sprite "walk-right")
             (move-left sprite 2)))
        (else
         (if (equal? (move-right sprite 2) sprite)
             (change-sprite-state sprite "walk-left")
             (move-right sprite 2)))))

(define (enemy-four-update-proc sprite)
  (cond ((<= (sprite-health sprite) 0)
         (list "kill-me"))
        ((<= (sprite-velY sprite) 0)
         (list "pop-me"
               (change-sprite-jumpspeed sprite 120)
               (make-sprite "enemy-4-projectile" (rectangle 20 20 "solid" "white") 
                            (list (sprite-realX sprite) (sprite-realY sprite) 0 0 5 -1) "move-left" projectile-2 sprite-display-image 20 20 127 "left")))
        (else (change-sprite-jumpspeed sprite (- (sprite-velY sprite) 1)))))

(define (enemy-six-update-proc sprite)
  (cond ((<= (sprite-health sprite) 0)
         (list "kill-me"))
        ((<= (sprite-frame-counter sprite) 0)
         (list "pop-me"
               (change-sprite-frame-counter sprite 120)
               (make-sprite "enemy-6-projectile-1" (rectangle 20 20 "solid" "blue")
                            (list (sprite-realX sprite) (sprite-realY sprite) -10 0 10 -1) "move-left" projectile-update-proc sprite-display-image 20 20 40 "left")
               (make-sprite "enemy-6-projectile-2" (rectangle 20 20 "solid" "blue")
                            (list (sprite-realX sprite) (sprite-realY sprite) -10 4 10 -1) "move-left" projectile-update-proc sprite-display-image 20 20 40 "left")
               (make-sprite "enemy-6-projectile-3" (rectangle 20 20 "solid" "blue")
                            (list (sprite-realX sprite) (sprite-realY sprite) -10 -4 10 -1) "move-left" projectile-update-proc sprite-display-image 20 20 40 "left")))
        (else (change-sprite-frame-counter sprite (- (sprite-frame-counter sprite) 2)))))

(define (projectile-update-proc sprite)
  (let ((new-sprite (if (< (sprite-velX sprite) 0)
                        (move-left sprite (abs (sprite-velX sprite)))
                        (move-right sprite (sprite-velX sprite)))))
    (sprite-decay-update (change-sprite-coords new-sprite (sprite-realX new-sprite) (+ (sprite-realY sprite) (sprite-velY sprite))))))

; DRAW FUNCTIONS
(define (player-draw-proc sprite)
  (cond ((is-state? sprite "get hurt")
         (rectangle 64 64 "solid" (color 255 0 0 50)))
        ((and (is-state? sprite "jump") (> (sprite-velY sprite) 0))
         player-jump-right-1)
        ((and (is-state? sprite "jump") (<= (sprite-velY sprite) 0))
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

(define (draw-sprites sprite-list img offset)
  (foldr (lambda (x y) (place-image/align ((sprite-draw x) x) (- (sprite-realX x) offset) (sprite-realY x) 'left 'top y)) 
         img
         sprite-list))

;******************************************************

; Pause menu cursor 
(define (pause-cursor-update sprite)
  (let ((new-sprite (if (>= (sprite-frame-counter sprite) 48)
                         (change-sprite-frame-counter sprite 0)
                         (change-sprite-frame-counter sprite (+ (sprite-frame-counter sprite) 1)))))
        (cond ((and up-button (or (is-state? sprite "position 1") (is-state? sprite "position 2") (is-state? sprite "position 3") (is-state? sprite "position 4")))
               (set! up-button #f)
               (change-sprite-frame-counter menu-cursor-pos-save (sprite-frame-counter new-sprite)))
              ((and down-button (is-state? sprite "save position"))
               (set! down-button #f)
               (change-sprite-frame-counter menu-cursor-pos-1 (sprite-frame-counter new-sprite)))
              ((and right-button (is-state? sprite "position 1"))
               (set! right-button #f)
               (change-sprite-frame-counter menu-cursor-pos-2 (sprite-frame-counter new-sprite)))
              ((and left-button (is-state? sprite "position 1"))
               (set! left-button #f)
               (change-sprite-frame-counter menu-cursor-pos-4 (sprite-frame-counter new-sprite)))
              ((and right-button (is-state? sprite "position 2"))
               (set! right-button #f)
               (change-sprite-frame-counter menu-cursor-pos-3 (sprite-frame-counter new-sprite)))
              ((and left-button (is-state? sprite "position 2"))
               (set! left-button #f)
               (change-sprite-frame-counter menu-cursor-pos-1 (sprite-frame-counter new-sprite)))
              ((and right-button (is-state? sprite "position 3"))
               (set! right-button #f)
               (change-sprite-frame-counter menu-cursor-pos-4 (sprite-frame-counter new-sprite)))
              ((and left-button (is-state? sprite "position 3"))
               (set! left-button #f)
               (change-sprite-frame-counter menu-cursor-pos-2 (sprite-frame-counter new-sprite)))
              ((and right-button (is-state? sprite "position 4"))
               (set! right-button #f)
               (change-sprite-frame-counter menu-cursor-pos-1 (sprite-frame-counter new-sprite)))
              ((and left-button (is-state? sprite "position 4"))
               (set! left-button #f)
               (change-sprite-frame-counter menu-cursor-pos-3 (sprite-frame-counter new-sprite)))
              (else new-sprite))))

(define (pause-cursor-draw sprite)
  (if (< (sprite-frame-counter sprite) 24)
      pause-cursor-1
      pause-cursor-2))



; ALL SPRITES AND SPRITE-LISTS
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

(define pause-cursor-1 (bitmap "images/menu/pause-cursor-1.png"))
(define pause-cursor-2 (bitmap "images/menu/pause-cursor-2.png"))

(define menu-cursor-pos-save (make-sprite "cursor" arrow-image (list 452 84) "save position" pause-cursor-update sprite-display-image 64 64 0 "left")) 
(define menu-cursor-pos-1 (make-sprite "cursor" pause-cursor-1 (list 50 230) "position 1" pause-cursor-update pause-cursor-draw 106 106 0 "left")) 
(define menu-cursor-pos-2 (make-sprite "cursor" pause-cursor-1 (list 170 230) "position 2" pause-cursor-update pause-cursor-draw 106 106 0 "left")) 
(define menu-cursor-pos-3 (make-sprite "cursor" pause-cursor-1 (list 300 230) "position 3" pause-cursor-update pause-cursor-draw 106 106 0 "left")) 
(define menu-cursor-pos-4 (make-sprite "cursor" pause-cursor-1 (list 430 230) "position 4" pause-cursor-update pause-cursor-draw 106 106 0 "left")) 

(define sprite-list-one (list (make-sprite "cursor" arrow-image '(210 225) "start" title-screen-cursor-update sprite-display-image 64 64 0 "left")))
(define sprite-list-two (list (make-sprite "player" player-stand-right (list 64 320 3 16) "stand-right" player-update-proc player-draw-proc 64 64 0 "right")
                              (make-sprite "enemy-1" (rectangle 64 64 "solid" "red") '(1024 320 0 0 10 5) "walk-right" enemy-one-update-proc sprite-display-image 64 64 0 "left")
                              (make-sprite "enemy-3" (rectangle 20 64 "solid" "purple") '(256 320 0 0 10 2) "walk-left" enemy-three-update-proc sprite-display-image 20 64 0 "left")
                              (make-sprite "enemy-4" (rectangle 64 64 "solid" "white") '(576 192 0 120 20 3) "shoot" enemy-four-update-proc sprite-display-image 64 64 0 "left")
                              (make-sprite "enemy-6" (rectangle 64 64 "solid" "blue") '(1984 64 0 0 20 5) "stand" enemy-six-update-proc sprite-display-image 64 64 0 "left")
                              (make-sprite "item-next-stage" (rectangle 64 64 "solid" "yellow") '(2240 192 0 0) "float" sprite-null-update sprite-display-image 64 64 0 "left")))
(define sprite-list-three (list (make-sprite "player" player-stand-right (list 64 320 0 16) "stand-right" player-update-proc player-draw-proc 64 64 0 "left")))