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

;******************** MAP STUFF ***********************

(define map-one (list (list w1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list w1 ai b1 ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai d1 ai ai ai ai ai b1 ai ai ai ai ai)
                      (list w1 ai ai ai ai ai d2 ai ai ai ai ai ai d2 ai d1 ai ai ai ai ai ai ai ai ai w4 w3 ai ai ai ai ai b1 ai ai ai ai)
                      (list w1 ai ai ai ai ai b1 ai ai b1 ai ai w4 g1 g1 w3 ai ai ai ai ai ai ai w4 g1 g3 g2 w3 ai ai ai ai ai ai ai ai ai)
                      (list w1 ai b1 d1 d1 b1 ai ai d2 ai ai w4 g3 g4 g4 w1 ai ai ai ai ai ai b1 w2 g4 g4 g4 w1 d1 ai ai ai ai d1 d1 ai ai)
                      (list g2 g1 g1 g1 g1 g1 g1 g1 g1 g1 g1 g3 g4 g4 g4 g2 g1 g1 g1 w3 ai ai ai w2 g4 g4 g4 g2 g1 g1 g1 g1 g1 g1 g1 g1 g1)))

(define map-two (list (list ai ai ai ai ai t1 t1 t1 ai t1 t1 ai ai ai t1 t1 ai t1 t1 t1 ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai t1 ai ai t1 ai ai ai t1 ai ai ai ai t1 ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai t1 ai ai t1 t1 ai ai ai t1 ai ai ai t1 ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai t1 ai ai t1 ai ai ai ai ai t1 ai ai t1 ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai t1 ai ai t1 t1 ai ai t1 t1 ai ai ai t1 ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai ai)
                      (list t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1)))

(define map-three 
                (list (list ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai ai ai ai ai)
                      (list ai ai t1 ai ai ai ai ai ai ai)
                      (list ai t1 t1 t1 ai ai ai ai ai ai)
                      (list ai ai ai ai ai ai t1 ai ai t1)
                      (list t1 t1 ai ai ai ai t1 ai ai ai)
                      (list t1 t1 t1 t1 t1 t1 t1 ai t1 t1)))

(define current-map map-one)

(define (choose-map map-num tiles-on-left)
  (cond ((= map-num 1) (sub-map map-one tiles-on-left (+ tiles-on-left 11)))
        ((= map-num 2) (sub-map map-two tiles-on-left (+ tiles-on-left 11)))
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