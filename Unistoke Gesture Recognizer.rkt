;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Unistoke Gesture Recognizer|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************
;; Dylan Thai
;; Date: 10/20/2020
;; Unistroke Gesture Recognizer
;; (CS 135 Assignment 4 Fall 2020)
;; *****************************
;;

;;
;; Problem 2 Part a
;;

;; (euclidean-norm v) Produces the Euclidean Norm of the vector v
;; Examples:
(check-within (euclidean-norm (list 3 4)) 5 0.01)
(check-within (euclidean-norm (list 3 4 5)) 7.07 0.01)

;; euclidean-norm: (listof Num) -> Num
(define (euclidean-norm v)
  (cond [(empty? v) 0]
        [else (sqrt (sum-of-sqr-vect-comp v))]))

;; Tests:
(check-expect (euclidean-norm (list 0 0 0)) 0)
(check-expect (euclidean-norm empty) 0)


;; (sum-of-sqr-vect-comp v) Produces the sum of the squares of each
;;    component in the vector v
;; Examples:
(check-expect (sum-of-sqr-vect-comp (list 0)) 0)
(check-expect (sum-of-sqr-vect-comp empty) 0)
(check-expect (sum-of-sqr-vect-comp (list 3 4)) 25)
(check-expect (sum-of-sqr-vect-comp (list 3 4 5)) 50)

;; sum-of-sqr-vect-comp: (listof Num) -> Num
(define (sum-of-sqr-vect-comp v)
  (cond [(empty? v) 0]
        [else (+ (sqr (first v)) (sum-of-sqr-vect-comp (rest v)))]))



;;
;; Problem 2 Part b
;;

;; (unit-vector v) Produces an aligned unit vector of v
;; Examples:
(check-within (unit-vector (list 3 4)) (list 0.6 0.8) 0.01)
(check-within (unit-vector (list 3 4 5)) (list 0.42 0.57 0.71) 0.01)


;; unit-vector: (listof Num) -> (listof Num)
;; Requires the elements in v to be postive
(define (unit-vector v)
  (cond [(empty? v) (list 0 0 0)]
        [else (calc-unit-vector v (euclidean-norm v))]))

;; Tests:
(check-expect (unit-vector empty) (list 0 0 0))
(check-within (unit-vector (list 39 48 57))
              (list 0.46 0.57 0.68) 0.01)


;; (calc-unit-vector v) Does the computation for the function
;;    vector

;; calc-unit-vector: (listof Num) -> (listof Num)
(define (calc-unit-vector v euclidean-norm)
  (cond [(empty? v) v]
        [else (cons (/ (first v) euclidean-norm)
                    (calc-unit-vector (rest v) euclidean-norm))]))



;;
;; Problem 2 Part c
;;

;; (cos-between a b) Produces the cosine of the angle between vectors
;;    a and b
;; Examples:
(check-within (cos-between (list 3 4) (list 0 6)) 0.8 0.1)
(check-within (cos-between (list 2 3 4) (list 7 6 5)) 0.92 0.01)

;; cos-between: (listof Num) (listof Num) -> Num
;; Requires:
;;   vector-A to be non-zero and length > 0
;;   vector-B to be non-zero and length > 0
;;   vector-A and vector-B to be the same degree

(define (cos-between a b)
  (cond [(empty? a) 0]
        [else (compute-cos-between (unit-vector a)
                                   (unit-vector b))]))

;; Tests:
(check-expect (cos-between (list 3 0) (list -4 0)) -1)
(check-expect (cos-between (list 3 0) (list 0 4)) 0)
(check-expect (cos-between (list 3 0) (list 6 0)) 1)



;; (compute-cos-between a b) does the computation for the function
;;    cos-between
;; Examples:
(check-within (compute-cos-between (list 0.6 0.8) (list 0 1))
              0.8 0.01)
(check-within (compute-cos-between (list 0.3714 0.5571 0.7428)
                                   (list 0.6674 0.5721 0.4767))
              0.92 0.01)

;; compute-cos-between (listof Num) (listof Num) -> Num
;; Requires:
;;   unit-vector-a to be non-zero and length > 0
;;   unit-vector-b to be non-zero and length > 0
;;   unit-vector-a and unit-vector-b to be the same degree

(define (compute-cos-between unit-vector-a unit-vector-b)
  (cond [(empty? unit-vector-a) 0]
        [else (+ (* (first unit-vector-a) (first unit-vector-b))
                 (compute-cos-between (rest unit-vector-a)
                                      (rest unit-vector-b)))]))