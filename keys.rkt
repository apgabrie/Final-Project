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