;; 2.4 Multiple Representations for Abstract Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Two strategies for creating generic programs
;;  1) Type tags
;;  2) Data-directed programming

;; For now we're going to use these to construct barriers between
;; representations of one data type.

;; In 2.5, we'll look at how to use type tags / data-directed
;; programming in order to do generic arithmetic (add, mul, etc).


;; 2.4.1 Representations for Complex Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Two possible forms:
;;   1) rectangular: (real . imaginary)
;;   2) polar: (magnitude . angle)
;;
;;     z = x + iy = r e^(iA)

;; Assume that the operations on complex numbers are implemented in
;; terms of four selectors: real-part, imag-part, magnitude, and
;; angle.

;; Also assume we have make-from-real-imag make-from-mag-ang where
;; given a complex number, z, we get z back from either:
;;
;;     (make-from-real-img (real-part z) (imag-part z))
;;     (make-from-mag-ang (magnitude z) (angle z))

;; With just these, we can define addition, subtraction,
;; multiplication, division.

(define (square x) (* x x))
(define real-part car)
(define imag-part cdr)
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-img x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (add-complex z1 z2)
  (make-from-real-img (+ (real-part z1) (real-part z2))
                      (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-img (- (real-part z1) (real-part z2))
                      (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; The above can work with any representation of complex numbers that
;; provides the appropriate interface. Above we use a fundamentally
;; rectangular representation, but we could use a fundamentally polar.


;; 2.4.2 Tagged data
;;;;;;;;;;;;;;;;;;;;

;; We can include a type tag, either 'polar or 'rectangular

;; A general format for attaching and looking at type tags:

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; From here we can build rectangular? and polar?

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Now we can get different representations to coexist in the same
;; program.

;; Rectangular representation
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

;; Note the type tagging here:
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-angle-rectangular x y)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; Polar representation
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

;; Again, note the type tagging here:
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;; Now we can dispatch to the correct method based on the tag.
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else "Unknown type -- REAL PART" z)))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
         (else (error "Unknown type -- ANGLE" z))))


;; The above technique is called "Dispatching on type"
;;
;; This implementation has some weaknesses
;;   1) Our generic api must know the underlying details of types
;;   2) We must modify multiple places to add new types
;;   3) We require globally unique names for representations
;;   4) All of the above stem from this being "not additive"

;; We can fix this with "Data directed programming"

;; Data directed programming stems from the idea that we have a table
;; of operations by types. Then finding the appropriate procedure call
;; is as simple as looking up the appropriate cell.
;;
;;     (put <op> <type> <item>)  ;; Installs the procedure into the table
;;     (get <op> <type>)         ;; Looks up the procedure in the table

;; I guess we'll just assume we have `put` and `get` for now...

;; Given this table exists, we can use a function, `apply-generic` to
;; make a single call to the appropriate function

;; NB: I'm just defining this here so the file will load
(define (get) (lambda (x . args) x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC "
           (list op type-tags))))))


;; Message Passing
;;;;;;;;;;;;;;;;;;

;; Before writing apply-generic, we had intelligent operations that
;; dispatched on type. What if we took the opposite approach and had
;; intelligent data that dispatched on operation?

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; Since our data objects are procedures, we apply an operation to
;; them by calling the procedure with the operation as an argument
(define (message-pass-apply-generic op arg) (arg op))

;; This style of programming is called message passing.

;; Message passing gets its name from the fact that a data object is
;; an entity that recieves the request for an operation as a "message"
;; containing the name of the operation to perform.
