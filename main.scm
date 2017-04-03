(pk "start of main.scm")

(define container (document-query-selector "#root"))

(define (init)
  "")

(define (text-changed state spawn)
  (lambda (event)
    (event-target-value event)))

(define (view state)
  (pk state)
  `(div (@ (id . "root"))
        (div (@ (id . "container"))
             (h1 "forward.scm " (small "⋅ frontend framework for Scheme"))
             (p "forward.scm is a library that allows to code
                dynamic website using the scheme language.")
             (p "This website is written using forward.scm")
             (p "To quickly get started clone the git repository:")
             (pre "> git clone https://github.com/amirouche/forward.scm")
             (p "To know more continue reading")
             (h2 "Kesako ?!")
             (p (code "forward.scm") " is frontend framework that follows
                the " (a (@ (href . "https://en.wikipedia.org/wiki/Model-view-controller"))
                         "Model-View-Controller pattern")
                " (MVC) that use Scheme (I hope you already understood that
                (anyway if you are not a schemers there is little chance
                you will be reading this (otherwise welcome!)))")
             (p "MVC is well known pattern used in graphical user interface
                 where basically a graphical application is split into 3 
                 pieces:")
             (ul (li "Model is the data. In forward.scm there is single 
                      immutable model datastructure that is built out of
                      lists (more likely association lists). It's a global
                      or fluid per se, it's passed around and through the
                      magic of closures (you do no need to understand what
                      closures are to use forward.scm)")
                 (li "View is... well the user interface per se. The piece
                      of code that represent the object you see. In forward.scm
                      this is coded using a sxml (a scheme DSL for writting 
                      html or xml (where attributes are pure association list)).
                      In forward, there is a single view that in turns can use
                      sub views. At the very core, there is a single view.")
                 (li "Controllers are the procedure where the logic of the 
                      application lives. Otherwise said everything that is not
                      view or model. Most of the time in graphical user interfaces,
                      controllers are event handlers. It is not always the case
                      but it's the most common kind of controller"))
             (p "With this small introduction to MVC you already know a lot about
                 forward.scm. First there is single model, there is a single view
                 which use sxml to represent the user interface and there are
                 controllers which are most of the time event handlers.")
             (p "(Most advanced MVC users might wonder whether it's possible to 
                 compose hierarchically components, the answer is yes!)"))))

(mount container init view)
