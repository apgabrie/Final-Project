(define (tile-at-xy my-map x y)
  (cond ((or (>= x (length (car my-map)))
             (< x 0))
         1)
        ((>= y (length my-map))
         3)
        ((< y 0)
         0)
        (else (tile-type (list-ref (list-ref my-map y) x)))))

(define (sub-list lst start end)
  (drop (take lst end) start))

(define (sub-map my-map start end)
  (map (lambda (x) (sub-list x start end)) my-map))

(define (find-checkpoint the-map x-coord)
  (define (find-checkpoint-help the-x-coord the-y-coord)
    (cond ((= (tile-at-xy the-map the-x-coord the-y-coord) 2)
           (list the-x-coord the-y-coord))
          ((<= the-y-coord (length the-map))
           (find-checkpoint-help the-x-coord (+ the-y-coord 1)))
          ((<= the-x-coord 0)
           (list 0 0))
          (else (find-checkpoint-help (- the-x-coord 1) 0))))
  (find-checkpoint-help (- x-coord 1) 0))

;******************** MAP STUFF ***********************

(define map-one (list (list ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai w4)
                      (list ai ai ai ai ai ai ai ai ai ai ai ai d1 ai ai ai ai ai ai ai ai ai ai ai w4 w3 b1 ai ai ai ai ai ai ai ai ai ai w2)
                      (list ai ai ai ai ai d1 ai ai ai ai ai ai w4 w3 d1 ai ai ai ai ai ai ai w4 g1 g3 g2 w3 ai ai ai ai b1 b1 ai ai ai ai w2)
                      (list d1 ai ai ai ai w4 w3 d2 ai ai ai w4 g3 g2 w3 ai d2 d1 ai d2 w4 g1 g3 g4 g4 g4 w1 b1 d2 ai d1 ai ai ai ai ai ai w2)
                      (list g1 g1 g1 g1 g1 g3 g2 g1 g1 g1 g1 g3 g4 g4 g2 g1 g1 g1 g1 g1 g3 g4 g4 g4 g4 g4 g2 g1 g1 g1 g1 g1 g1 g1 g1 g1 g1 g3)))

(define map-two (list (list w1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai b1 ai ai ai ai ai ai ai b1 ai ai ai ai ai ai ai ai ai ai w4 g1 g1 w3 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai b1 ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai b1 d2 ai ck ai ai ai ai b1 ck ai ai ai ai ai ai b1 ai ai w2 g4 g4 g2 w3 ai ai ai ai ai ai ai b1 ai ai ck ai ai ai ai ai b1 ai ai b1 ck ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai d1 ai ai b1 b1 w4 g1 w3 ai ai ai w4 g1 w3 ai ai ai ai ai ai ai ai ai w2 g4 g4 g4 g2 g1 w3 ai ai ai ai ai w4 g1 g1 w3 ai ai ai ai b1 w4 g1 g1 w3 b1 ai ai ai ai ai ai)
                      (list w1 d1 d1 ai ai ai w4 g1 g1 g1 w3 b1 w2 g4 w1 ai ai w4 g3 g4 w1 ai ai ai ck ai ai ai ai w4 g3 g4 g4 g4 g4 g4 w1 ck ai ai ai b1 w2 g4 g4 w1 ai ai ai ai ai w2 g4 g4 w1 ai ai ai ai ai ai ai)
                      (list g2 g1 g1 g1 g1 g1 g3 g4 g4 g4 g2 g1 g3 g4 w1 ai ai w2 g4 g4 w1 ai ai b1 b1 ai ai w4 g1 g3 g4 g4 g4 g4 g4 g4 g2 w3 ai ai ai ai w2 g4 g4 w1 b1 ai ai ai ai w2 g4 g4 w1 ai ai ai ai ai ai ai)))

(define map-three 
                (list (list ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai w2) 
                      (list ai ai ai ai ai ai ai ai ai ai ai b1 b1 ai ai b1 b1 ai ai w4 w3 ai ai ai ai ai b1 b1 b1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai w2)
                      (list ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai w2 w1 ck ai ai ai ai ai ai ai ai ai ai ai ai ai ai b1 b1 ai ai ai ai ai ai w4 g1 w3 ai ai ai ai ai ck ai ai ai ai b1 b1 ai ai w2)
                      (list ai ai ai ai ai ai ck ai ai ai ai ai ai ai ai ai ai ai ai w2 g2 w3 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai w4 g3 g4 g2 w3 ai ai w4 g1 w3 ai ai ai ai ai ai ai ai w2)
                      (list ai ai ai ai ai w4 w3 ai ai ai ai ai ai b1 b1 b1 b1 ai ai w2 g4 w1 ai ai ai ai ai ai ai d2 ai ai d1 ai ai ai ai ai ai b1 b1 b1 ai w2 g4 g4 g4 g2 g1 g1 g3 g4 w1 ai ai ai ai ai ai ai ai w2)
                      (list d1 ai ai ai w4 g3 w1 ai ai ai ai ai ai ai ai ai ai d1 ai w2 g4 w1 ai ai ai ai ai w4 g1 g1 g1 g1 g1 w3 ai d1 ai ck ai ai ai ai ai w2 g4 g4 g4 g4 g4 g4 g4 g4 w1 ai ai ai w4 w3 ai ai ai w2)
                      (list g1 g1 g1 g1 g3 g4 w1 b1 ai ai ai ai ai ai w4 g1 g1 g1 g1 g3 g4 w1 ai ai ai ai ai w2 g4 g4 g4 g4 g4 g2 g1 g1 g1 w3 ai ai ai ai ai w2 g4 g4 g4 g4 g4 g4 g4 g4 w1 ai ai ai w2 g2 g1 g1 g1 g3)))
;                           0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61
(define current-map map-one)

(define (choose-map map-num tiles-on-left)
  (cond ((= map-num 1) (sub-map map-one tiles-on-left (+ tiles-on-left 11)))
        ((= map-num 2) (sub-map map-two tiles-on-left (+ tiles-on-left 11)))
        ((= map-num 3) (sub-map map-three tiles-on-left (+ tiles-on-left 11)))
        (else map-one)))

(define (map-by-number map-num)
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