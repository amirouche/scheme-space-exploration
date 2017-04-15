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
      (set* model 'game 'preview position))))

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
                 (number->string (ref object 'science)) " science."))

(define (distance position other)
  (expt (+ (expt (- (car other) (car position)) 2)
           (expt (- (cdr other) (cdr position)) 2))
        0.5))

(define (object-near? position model)
  (let ((positions (map car (ref model 'owned))))
    (memq #t (map (lambda (other)
                    (< (distance position other) 3)) positions))))

(define (acquire model position)
  (let ((object (ref* model 'game 'universe position)))
    (pk object)
    (let ((electricity* (ref object 'electricity*))
          (ore* (ref object 'ore*))
          (science* (ref object 'science*)))
      (set*
       (set*
        (set* model 'game 'science (- (ref* model 'game 'science) science*))
        'game 'ore
        (- (ref* model 'game 'ore) ore*))
       'game 'electricity
       (- (ref* model 'game 'electricity) electricity*)))))

(define (acquire-clicked position)
  (lambda (model spawn)
    (lambda (event)
      (set* (produce (acquire model position)) 'game 'owned position #t))))

(define (object-affordable? object model)
  (and (< (ref object 'science*) (ref model 'science))
       (< (ref object 'ore*) (ref model 'ore))
       (< (ref object 'electricity*) (ref model 'electricity))))

(define (render-preview model mc)
  (let ((position (ref model 'preview)))
    (if position
        (let ((object (ref* model 'universe position)))
          `(div
            (div
             (img (@ (src . ,(object-image object))))
             ,(render-object-description position object))
            ,(if (member position (map car (ref model 'owned)))
                 `(p "It's part of culturia!")
                 (if (object-near? position model)
                     (if (object-affordable? object model)
                         `(button (@ (on . ((click . ,(mc (acquire-clicked position)))))) "join")
                         `((p ,(string-append " To join this " (symbol->string (ref object 'kind)) " you need "
                                              (number->string (ref object 'ore*)) " ore, "
                                              (number->string (ref object 'electricity*)) " electricity, "
                                              (number->string (ref object 'science*)) " science."))
                           (p "We do not have enough ressources")))
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
    (let ((positions (map car (ref* model 'game 'owned))))
      (apply + (ref* model 'game name)
             (map (lambda (position)
                    (ref (ref* model 'game 'universe position) name)) positions)))))

(define turn-electricity (turn-ressource 'electricity))
(define turn-ore (turn-ressource 'ore))
(define turn-science (turn-ressource 'science))

(define (produce model)
    (let ((electricity (turn-electricity model))
          (ore (turn-ore model))
          (science (turn-science model)))x
      (set* (set* (set* model 'game 'electricity electricity) 'game 'ore ore) 'game 'science science)))

(define (next-turn model spawn)
  (lambda (event)
    (produce model)))

(define (view/game-board model mc)
  `(div (@ (id . "root") (class . "game"))
        (div (@ (id . "sidebar"))
             (h1 "culturia " (small "⋅ space exploration"))
             (p ,(ref* model 'game 'message))
             ,(render-production (ref model 'game))
             (button (@ (on . ((click . ,(mc next-turn))))) "next turn"))
        (div (@ (id . "board"))
             ,(map (lambda (x)
                     `(div (@ (class . "line"))
                           ,(map (lambda (y) (render-object (ref model 'game) mc (make-position x y))) (iota 13))))
                   (iota 13)))
        (div (@ (id . "preview"))
             ,(render-preview (ref model 'game) mc))))

(define universe `(((0 . 0) . ,(make-planet))
                   ((2 . 2) . ,(make-star))
                   ((1 . 2) . ,(make-asteroid))
                   ((2 . 3) . ,(make-planet))
                   ((1 . 5) . ,(make-asteroid))
                   ((1 . 8) . ,(make-asteroid))
                   ((2 . 9) . ,(make-asteroid))
                   ((0 . 10) . ,(make-star))
                   ((1 . 12) . ,(make-planet))
                   ((4 . 4) . ,(make-star))
                   ((5 . 5) . ,(make-star))
                   ((3 . 6) . ,(make-planet))
                   ))

(define (new-game model)
  (set model 'game
       `((message . "Héllo dear Administer!")
         (universe . ,universe)
         (owned . (((0 . 0) . #t)))
         (ore . 0)
         (electricity . 0)
         (science . 0))))

(define (new-game-clicked model spawn)
  (lambda (event)
    (history-append "/game/board")
    (resolve (new-game model) spawn)))

(define (view/index model mc)
  `(div (@ (id . "root"))
        (div (@ (id . "menu"))
             (h1 "culturia" (small "⋅ space exploration"))
             (ul
              (li (button (@ (on . ((click . ,(mc new-game-clicked))))) "new game"))
              (li (button "load saved game"))
              (li ,(link mc "/help" '(button "help")))
              (li ,(link mc "/credits" '(button "credits")))))))

(define (view/credits model mc)
  `(div (@ (id . "root"))
        (h1 "culturia" (small " ⋅ space exploration"))
        (h2 "credits")
        (p "icons are from the noun project (FIXME expand credits later)")))

(define (view/help model mc)
  `(div (@ (id . "root"))
        (h1 "culturia" (small " ⋅ space exploration"))
        (h2 "help")
        (p "culturia goal is to learn scheme language while exploring the universe")
        (p "You start with a single planet that produce 10 ore, 10 electricity and 10 science.
To increase your production you have the choice between exploring the universe and building factories.
Joining a new space object requires ressources and to already have an outpost
near it. Otherwise you can build facilities on the space objects you already have to increase the
production of electricity, ore and science. Planets are the most versatile. You can build
on them whatever you want but the ressources are limited. You can extract a lot of ore from
asteroid and can harness star energy to produce a lot of electricity.")
        (p "Building facility mostly cost ore, but you will need to program the facility
so that the facility actually produce something. Evaluating a program cost electricity.")
        (p "Each turn new ressources are produced by your factories. You consume those
ressources by reaching new space objects, programming factories and using the administer
codex. Ore and electricity is limited by the storage facility you have. Science
is not limited. That said science production is slow, you can build labs to increase science
production.")
        (p "Science allows you to discover new scheme constructs which populates the administer
codex. Using the administer codex cost electricity")))


(define routes `(("/" ,identity-controller ,view/index)
                 ("/game/board" ,identity-controller ,view/game-board)
                 ("/help" ,identity-controller ,view/help)
                 ("/credits" ,identity-controller ,view/credits)))

(define route->view (make-views routes))

(define (view model mc)
  (let ((route (pk 'route (ref* model 'location 'route))))
    (if (eq? route 'unknown)
        `(h1 "Error 404: Unknown route " ,(document-location-pathname))
        (let ((view (ref route->view route)))
          (if view
              (view model mc)
              '(p "no route defined yet for " route))))))

(create-app* container (lambda () '()) view (make-routes routes))
