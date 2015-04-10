(define (make-tile type image) (cons type image))
(define tile-type car)
(define tile-image cdr)

(define air (empty-scene 0 0))
(define wall-tile-1 (bitmap "images/tiles/wall-tile-1.png"))
(define ground-tile-1 (bitmap "images/tiles/ground-tile-1.png"))
(define ground-tile-2 (bitmap "images/tiles/ground-tile-2.png"))
(define grass-1 (bitmap "images/tiles/grass-1.png"))
(define mushroom-1 (bitmap "images/tiles/mushroom-1.png"))

(define test1 (rectangle 64 64 "solid" "black"))
(define t1 (make-tile 1 test1))
(define test2 (rectangle 64 64 "solid" "blue"))

(define ai (make-tile 0 (empty-scene 0 0)))
(define w1 (make-tile 1 wall-tile-1))
(define g1 (make-tile 1 ground-tile-1))
(define g2 (make-tile 1 ground-tile-2))
(define d1 (make-tile 0 grass-1))
(define d2 (make-tile 0 mushroom-1))

(define (item-at-num lst num)
  (if (> num 0)
      (item-at-num (cdr lst) (- num 1))
      (car lst)))

(define (tile-at-xy my-map x y)
  (if (or (> x (length (car my-map)))
          (< x 0)
          (>= y (length my-map))
          (< y 0))
          1
          (tile-type (item-at-num (item-at-num my-map y) x))))