#lang racket

(require slideshow)
(require slideshow/text)
(require pict)

(provide current-mono-font tm stm
	 block cblock
	 rect-bullet subsubitem
	 url surl)

(define current-mono-font (make-parameter "Nimbus Mono L"))
(define (tm s) (text s (cons 'bold (current-mono-font)) (current-font-size)))
(define (stm s) (small (tm s)))

(define (block render . l)
  (apply vl-append (current-line-sep) (map render l)))

(define (cblock render . l)
  (apply vl-append (map render l)))

(define rect-bullet (baseless
		  (cc-superimpose (rectangle (/ gap-size 2) (/ gap-size 2))
				  (blank 0 gap-size))))

(define (subsubitem #:gap-size [a-gap-size (current-gap-size)]
		    #:bullet [bullet (if (eq? gap-size a-gap-size)
					 rect-bullet
					 (scale rect-bullet (/ a-gap-size gap-size)))]
		    #:width [width (current-para-width)]
		    #:align [align 'left]
		    #:fill? [fill? #t]
		    #:decode? [decode? #t]
		    . s)
  (inset (htl-append (/ a-gap-size 2)
		     bullet
		     (para #:width (- width
				      (* 4 a-gap-size)
				      (pict-width bullet)
				      (/ a-gap-size 2))
			   #:align align
			   #:fill? fill?
			   #:decode? decode?
			   s))
	 (* 4 a-gap-size) 0 0 0))

(define (url a-url)
  (hyperlinkize (if (pict? a-url) a-url (t a-url))))

(define (surl a-url)
  (scale (url a-url) 0.5))
