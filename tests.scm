(pk "* forward.js unit tests")

(define number-of-failed-tests 0)

(define-macro (test-check test-name expr expected)
  `(begin
     (pk (string-append "** test " ,test-name))
     (let ((expr* ,expr)
           (expected* ,expected))
       (if (equal? expr* expected*)
           (pk "*** PASS :)")
           (begin
             (pk "*** FAILED :(")
             (pk "**** expected:" expected*)
             (pk "**** found:" expr*)
             (set! number-of-failed-tests (+ 1 number-of-failed-tests)))))))

(test-check "rm 1"
  (rm '((a  . b) (c . d)) 'a)
  '((c  . d)))
     
(test-check "rm 2"
  (rm '((a  . b) (c . d)) 'c)
  '((a  . b)))
     
(test-check "rm 3"
  (rm '((a  . b) (c . d) (e . f)) 'c)
  '((a  . b) (e . f)))

(test-check "set 1"
  (set '() 'a 'b)
  '((a . b)))

(test-check "set 2"
  (set  '((a . b)) 'a 'c)
  '((a . c)))

(test-check "ref 1"
  (ref '() 'a)
  #f)

(test-check "ref 2"
  (ref '((a . b)) 'a)
  'b)

(test-check "rm* 1"
  (rm* '() 'a)
  '())

(test-check "rm* 2"
  (rm* '((a . b)) 'a)
  '())

(test-check "rm* nested 1"
  (rm* '((a . a) (b . ((c . d)))) 'b 'c)
  '((a . a)))

(test-check "rm* nested 2"
  (rm* '((b . ((c . d))) (a . a)) 'b 'c)
  '((a . a)))

(test-check "rm* nested 3"
  (rm* '((b . ((c . d)))) 'b 'c)
  '())

(pk "** number failed tests:" number-of-failed-tests)
