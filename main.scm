(pk "* start of main.scm")

(define container (document-query-selector "#root"))

(define make-position cons)

(define (make-object kind ore electricity science)
  `((kind . ,kind)
    (ore . ,ore) ;; production per turn
    (electricity . ,electricity) ;; production per turn
    (science . ,science)))  ;; production per turn

(define object->image `((ore . "ore.png")
                        (planet . "planet.png")
                        (asteroid . "asteroid.png")
                        (star . "star.png")))

(define (object-image object)
  (string-append "static/" (pk (ref object->image (pk (ref object 'kind))))))

(define (object-acquired? model position)
  (pk 'acquired (ref* model 'owned position)))

(define (render-object-class model position)
  (if (object-acquired? model position)
      "space acquired"
      "space"))

(define (object-clicked position)
  (lambda (model spawn)
    (lambda (event)
      (set* model 'preview position))))

(define (render-object model mc position)
  (let ((object (ref* model 'universe position)))
    (if object
        `(div (@ (class . ,(render-object-class model position))
                 (on . ((click . ,(mc (object-clicked position))))))
              (img (@ (src . ,(object-image object)))))
        `(div (@ (class . "space"))))))

(define (render-object-description object)
  (string-append "This is a " (symbol->string (ref object 'kind)) ". "
                 "It produce "
                 (number->string (ref object 'ore)) " ore, "
                 (number->string (ref object 'electricity)) " electricity, "
                 (number->string (ref object 'science)) " science."))

(define (render-object-preview model)
  (let ((position (ref model 'preview)))
    (if position
        (let ((object (ref* model 'universe position)))
          `(div (@ (id . "preview"))
                (img (@ (src . ,(object-image object))))
                ,(render-object-description object)))
        '(div ""))))

(define (view model mc)
  (map pk model)
  `(div (@ (id . "root"))
        (div (@ (id . "sidebar"))
             (h1 "culturia " (small "⋅ space exploration"))
             ,(render-object-preview model)
             (p ,(ref model 'message))
             (button "next turn"))
        (div (@ (id . "board"))
             ,(map (lambda (x) `(div (@ (class . "line"))
                                   ,(map (lambda (y) (render-object model mc (make-position x y))) (iota 13)))) (iota 13)))))

(define universe `(((0 . 0) . ,(make-object 'planet 10 10 10))
                   ((2 . 2) . ,(make-object 'star 0 100 0))))

(define (init)
  `((message . "Héllo dear Administer! The planet at the top left is yours!")
    (universe . ,universe)
    (owned . (((0 . 0) . #t)))))

(mount container init view)
