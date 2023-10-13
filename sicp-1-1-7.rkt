#|

takes a guess and number we want to find square root of if "good enough" returns
guess otherwise runs the function again with a better guess (improve guess x)

requires defining "good enough" and "improve"

improve :: average guess with x/guess
good enough :: x/guess within +- 0.00001 of guess

|#

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x))) 

(define (improve guess x)
  (average guess (/ x guess))) 

(define (average x y)
  (/ (+ x y) 2)) 

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.00001)) 

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x)) 

#|
flexible version - for trying out different thresholds
|#
(define (sqrt-iter-flex guess x threshold)
  (if (good-enough-flex? threshold guess x)
    guess
    (sqrt-iter-flex (improve guess x) x threshold))) 

(define (good-enough-flex? threshold guess x)
  (< (abs (- (square guess) x)) threshold)) 

(define (sqrt-flex x threshold)
  (let ([guess 1.0]) (sqrt-iter-flex guess x threshold)))
   

#|
version for 1.7
|#
(define (good-enough-imp? guess x)
  (let ([new-guess (improve guess x)])
    (< (abs (- 1 (/ new-guess guess))) 0.001)))


(define (sqrt-iter-imp guess x)
  (if (good-enough-imp? guess x)
    guess
    (sqrt-iter-imp (improve guess x) x))) 

(define (sqrt-imp x)
  (sqrt-iter-imp 1.0 x)) 

