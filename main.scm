(pk "* start of main.scm")

(define container (document-query-selector "#root"))

(define make-position cons)

(define (make-object kind ore electricity science ore* electricity* science*)
  `((kind . ,kind)
    (ore . ,ore) ;; production per turn
    (electricity . ,electricity) ;; production per turn
    (science . ,science)  ;; production per turn
    (ore* . ,ore*) ;; required ressources to acquire
    (electricity* . ,electricity*)
    (science* . ,science*)))

(define (make-star)
  (make-object 'star 0 10 0 100 100 100))

(define (make-planet)
  (make-object 'planet 5 5 5 50 50 50))

(define (make-asteroid)
  (make-object 'asteroid 10 0 0 10 50 10))

(define object->image `((ore . "ore.png")
                        (planet . "planet.png")
                        (asteroid . "asteroid.png")
                        (star . "star.png")))

(define (object-image object)
  (string-append "static/" (ref object->image (ref object 'kind))))

(define (object-acquired? model position)
  (ref* model 'owned position))

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

(define (render-object-description position object)
  (string-append "(" (number->string (car position)) ", " (number->string (cdr position)) ")"
                 " is a " (symbol->string (ref object 'kind)) ". "
                 "It produce "
                 (number->string (ref object 'ore)) " ore, "
                 (number->string (ref object 'electricity)) " electricity, "
                 (number->string (ref object 'science)) " science."
                 " To join this " (symbol->string (ref object 'kind)) " you need "
                 (number->string (ref object 'ore*)) " ore, "
                 (number->string (ref object 'electricity*)) " electricity, "
                 (number->string (ref object 'science*)) " science."))

(define (distance position other)
  (expt (+ (expt (- (car other) (car position)) 2)
           (expt (- (cdr other) (cdr position)) 2))
        0.5))

(define (object-near? position model)
  (let ((positions (map car (ref model 'owned))))
    (memq #t (map (lambda (other)
                    (< (distance position other) 3)) positions))))

(define (acquire model position)
  (let ((object (ref* model 'universe position)))
    (pk object)
    (let ((electricity* (ref object 'electricity*))
          (ore* (ref object 'ore*))
          (science* (ref object 'science*)))
      (set
       (set
        (set model 'science (- (ref model 'science) science*))
        'ore 
        (- (ref model 'ore) ore*))
       'electricity
       (- (ref model 'electricity) electricity*)))))

(define (acquire-clicked position)
  (lambda (model spawn)
    (lambda (event)
      (set* (produce (acquire model position)) 'owned position #t))))

(define (object-affordable? object model)
  (and (< (ref object 'science*) (ref model 'science))
       (< (ref object 'ore*) (ref model 'ore))
       (< (ref object 'electricity*) (ref model 'electricity))))

(define (render-object-preview model mc)
  (let ((position (ref model 'preview)))
    (if position
        (let ((object (ref* model 'universe position)))
          `(div (@ (id . "preview"))
                (div
                 (img (@ (src . ,(object-image object))))
                 ,(render-object-description position object))
                ,(if (member position (map car (ref model 'owned)))
                     `(p "It's part of culturia!")
                     (if (object-near? position model)
                         (if (object-affordable? object model)
                             `(button (@ (on . ((click . ,(mc (acquire-clicked position)))))) "join")
                             `(p "We do not have enough ressources"))
                         `(p "It's too far away, continue exploring")))))
        '(div ""))))

(define (render-production model)
  `(div (@ (id . "production"))
        (div (p ,(ref model 'ore))
             (img (@ (src . "static/ore.png"))))

        (div (p ,(ref model 'electricity))
             (img (@ (src . "static/electricity.png"))))
        (div (p ,(ref model 'science))
             (img (@ (src . "static/science.png"))))))

(define (turn-ressource name)
  (lambda (model)
    (let ((positions (map car (ref model 'owned))))
      (apply + (ref model name)
             (map (lambda (position)
                    (ref (ref* model 'universe position) name)) positions)))))

(define turn-electricity (turn-ressource 'electricity))
(define turn-ore (turn-ressource 'ore))
(define turn-science (turn-ressource 'science))

(define (produce model)
    (let ((electricity (turn-electricity model))
          (ore (turn-ore model))
          (science (turn-science model)))
      (set (set (set model 'electricity electricity) 'ore ore) 'science science)))

(define (next-turn model spawn)
  (lambda (event)
    (produce model)))

(define (view model mc)
  ;; (map pk model)
  `(div (@ (id . "root"))
        (div (@ (id . "sidebar"))
             (h1 "culturia " (small "⋅ space exploration clicker"))
             (p ,(ref model 'message))
             ,(render-production model)
             ,(render-object-preview model mc)
             (button (@ (on . ((click . ,(mc next-turn))))) "next turn"))
        (div (@ (id . "board"))
             ,(map (lambda (x) `(div (@ (class . "line"))
                                   ,(map (lambda (y) (render-object model mc (make-position x y))) (iota 13)))) (iota 13)))))

(define universe `(((0 . 0) . ,(make-planet))
                   ((2 . 2) . ,(make-star))
                   ((1 . 2) . ,(make-asteroid))
                   ((2 . 3) . ,(make-planet))                   
                   ((1 . 5) . ,(make-asteroid))
                   ((1 . 8) . ,(make-asteroid))
                   ((2 . 9) . ,(make-asteroid))
                   ((4 . 4) . ,(make-star))
                   ((5 . 5) . ,(make-star))
                   ((3 . 6) . ,(make-planet))                   
                   ))

(define (init)
  `((message . "Héllo dear Administer!")
    (universe . ,universe)
    (owned . (((0 . 0) . #t)))
    (ore . 0)
    (electricity . 0)
    (science . 0)))

(mount container init view)
