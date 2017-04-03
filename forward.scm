;; scheme helpers

(define (pk . args)
  (apply console-log (cons ";;;" args))
  (car (reverse args)))

(define (acons a b alist)
  (cons (cons a b) alist))

(define (rm alist key)
  (let loop ((alist alist)
             (out '()))
    (if (null? alist)
        out
        (if (equal? (caar alist) key)
            (loop (cdr alist) out)
            (loop (cdr alist) (cons (car alist) out))))))

(define (set alist key value)
  (acons key value (rm alist key)))

(define (ref alist key)
  (let loop ((alist alist))
    (cond
     ((null? alist) #f)
     ((equal? (caar alist) key) (cdar alist))
     (else (loop (cdr alist))))))

(define (ref* assoc . keys)
  (let loop ((keys keys)
             (assoc assoc))
    (cond
     ((eq? assoc #f) #f)
     ((null? keys) assoc)
     (else (loop (cdr keys) (ref assoc (car keys)))))))

(define (set* assoc . args)
  (let* ((args* (reverse args))
         (value (car args*))
         (keys (reverse (cdr args*))))
    (let loop ((keys keys)
               (assoc assoc))
      (if (null? keys)
          value
          (set assoc (car keys) (loop (cdr keys) (or (ref assoc (car keys)) '())))))))

(define (rm* assoc . keys)
  (set* assoc (append keys (list #f))))

(define (string-prefix? prefix string)
  (let ((l (string-length prefix)))
    (if (< (string-length string) l)
        #f
        (let ((other (substring string 0 l)))
          (equal? other prefix)))))

;;; async ajax bindings

(define $ (js-eval "jQuery"))

(define (ajax url settings k)
  (let ((promise (js-invoke $ "ajax" url (alist->js-obj settings))))
    (js-invoke promise "always" (js-closure k))))

;;; localStorage bindings

(define local-storage (js-eval "localStorage"))

(define (local-storage-set! key value)
  (js-invoke local-storage "setItem" key value))

(define (local-storage-ref key)
  (js-invoke local-storage "getItem" key))

;;; snabbdom bindings

(define %window (js-eval "window"))
(define %document (js-eval "document"))

(define (document-query-selector selector)
  (js-invoke %document "querySelector" selector))

(define (patch old new)
  (js-invoke %window "patch" old new)
  new)

(define (events->js-obj make-action events)
  (alist->js-obj (map (lambda (name+proc)
                        (cons (car name+proc) (js-closure (make-action (cdr name+proc)))))
                      events)))

(define (html tag events children)
  (js-invoke %window "h" tag events (list->js-array children)))

;;; navigo bindings

(define (make-navigo root)
  (if (null? root)
      (js-new "Navigo")
      (js-new "Navigo" root)))

(define (navigo-on navigo pattern proc)
  (js-invoke navigo "on" pattern (js-closure (lambda (param) (proc (js-obj->alist param))))))

(define (navigo-default navigo proc)
  (js-invoke navigo "on" (js-closure proc)))

(define (navigo-not-found navigo proc)
  (js-invoke navigo "notFound" (js-closure proc)))

(define (navigo-navigate navigo path)
  (js-invoke navigo "navigate" path))

(define (navigo-resolve navigo)
  (js-invoke navigo "resolve"))

;;; FIXME: use biwascheme bindings

(define (event-target event)
  (js-ref event "target"))

(define (event-target-value event)
  (js-ref (event-target event) "value"))

(define (event-target-checked event)
  (js-ref (event-target event) "checked"))

(define (event-key event)
  (js-ref event "key"))

(define (event-delta-y event)
  (js-ref event "deltaY"))

(define (event-prevent-default event)
  (js-invoke event "preventDefault"))

(define (set-timeout proc timeout)
  (js-invoke %window "setTimeout" (js-closure proc) timeout))
  
(define (attrs->js-obj make-action attrs)
  (let loop ((attrs attrs)
             (on '())
             (out '()))
    (if (null? attrs)
        (if (ref out "key")
            (alist->js-obj `(("on" . ,(events->js-obj make-action on))
                             ("attrs" . ,(alist->js-obj out))
                             ("key" . ,(ref out "key"))))
            (alist->js-obj `(("on" . ,(events->js-obj make-action on))
                             ("attrs" . ,(alist->js-obj out)))))
        (let ((name (symbol->string (caar attrs)))
              (value (cdar attrs)))
          (if (string-prefix? "on-" name)
              (loop (cdr attrs)
                    (cons (cons (substring name 3 (string-length name)) value)
                          on)
                    out)
              (loop (cdr attrs)
                    on
                    (cons (cons name value) out)))))))

(define (flatten lst)
  (let loop ((lst lst)
             (out '()))
    (cond
     ((null? lst) (reverse out))
     ((and (pair? (car lst)) (not (symbol? (caar lst)))) (loop (append (car lst) (cdr lst)) out))
     (else (loop (cdr lst) (cons (car lst) out))))))

(define (sxml->h* make-action)
  ;; FIXME: improve this function
  (lambda (element)
    (cond
     ((null? element) '())
     ((string? element) element)
     (else
      (let ((tag (symbol->string (car element))))
        (let ((attrs (cadr element)))
          (if (and (pair? attrs) (eq? (car attrs) '@))
              (html tag
                    (attrs->js-obj make-action (cdr attrs))
                    (map (sxml->h* make-action) (flatten (cddr element))))
              (html tag
                    (alist->js-obj '())
                    (map (sxml->h* make-action) (flatten (cdr element)))))))))))

(define (mount container init view)
  "Mount in node from the DOM named CONTAINER, the result of the state
returned by INIT passed to VIEW. VIEW must return pseudo sxml where
\"on-fu\" attributes (where fu is DOM event name) are associated with
action lambdas. An action looks like the following:

   (define (button-clicked state spawn)
     (lambda (event)
       (+ 1 state)))

In the above STATE is the current state. SPAWN allows to create
a new green thread. When the action returns the new state, the
VIEW procedure is called again with the new state.

A minimal VIEW procedure looks like the following:

   (define (view state)
     `(button (@ (on-click . ,button-clicked)) ,state))

A minimal INIT procedure looks like the following:

   (define (init) 1)

That's all folks!
"
  (let ((state (init)))  ;; init state
    ;; create a procedure that allows to create new green threads
    (letrec ((spawn (lambda (timeout proc args)
                      (set-timeout (lambda () (apply (proc state spawn) args)) timeout)))
             ;; lambda used to wrap event callback
             (make-action (lambda (action)
                            (lambda args
                              (let ((new (apply (action state spawn) args)))
                                (set! state new)
                                (render)))))
             ;; rendering pipeline
             (render (lambda ()
                       (let ((sxml (view state)))
                         (set! container (patch container
                                                ((sxml->h* make-action) sxml)))))))
      (render)

      (lambda (proc)
        (set! state (proc state)) ;; set new state
        (render))))) ;; render the new state
