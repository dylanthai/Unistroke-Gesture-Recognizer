;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Unistoke Gesture Recognizer|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *****************************
;; Dylan Thai (20898721)
;; CS 135 Fall 2020
;; Assignment 04, Problem 3
;; *****************************
;;

;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal

;; Constants for Examples and Testing
;; line segement
(define test-gesture-1 (list (list 1 2) (list -3 4)))

;; Quadralateral
(define test-gesture-2
  (list (list 100 0) (list 200 100) (list 100 200) (list 0 100)))

;; Triangle
(define test-gesture-3
  (list (list 1 2) (list 3 2) (list 2 1)))

;; Singular Point
(define test-gesture-4
  (list (list 5 6)))


;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;; i.

;; (get-x point) produces the x-coordinate of point
;; Examples:
(check-expect (get-x (list 3 4)) 3)
(check-expect (get-x (list -5 2)) -5)

;; get-x: Point -> Num
(define (get-x point)
  (first point))

;; (get-y point) produces the x-coordinate of point
;; Examples:
(check-expect (get-y (list 3 4)) 4)
(check-expect (get-y (list -5 -2)) -2)

;; get-y: Point -> Num
(define (get-y point)
  (first (rest point)))


;; ii.

;; (translate-gesture gesture x-offset y-offset) produces a new
;;    gesture with each point in gesture moved x-offset units 
;;    horizontally and each point in gesture y-offset unit vertically
;; Examples:
(check-expect (translate-gesture test-gesture-1 1 -5)
              (list (list 2 -3) (list -2 -1)))
(check-expect (translate-gesture test-gesture-2 -50 75)
              (list (list 50 75) (list 150 175) (list 50 275) (list -50 175)))

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture gesture x-offset y-offset)
  (cond [(empty? gesture) gesture]
        [else (cons (list (+ (get-x (first gesture)) x-offset)
                          (+ (get-y (first gesture)) y-offset))
                    (translate-gesture (rest gesture) x-offset y-offset))]))

;; iii.

;; (scale-gesture gesture x-scale y-scale) produces a new
;;    gesture with each point in gesture scaled x-scale times 
;;    horizontally and each point in gesture y-scale times vertically
;; Examples:
(check-expect (scale-gesture test-gesture-1 2 3)
              (list (list 2 6) (list -6 12)))
(check-expect (scale-gesture test-gesture-2 0.5 0.2)
              (list (list 50 0) (list 100 20) (list 50 40) (list 0 20)))

;; scale-gesture: Gesture Num Num -> (listof (list Num Num))
;; Requires:
;;   x-scale > 0
;;   y-scale > 0

(define (scale-gesture gesture x-scale y-scale)
  (cond [(empty? gesture) gesture]
        [else (cons (list (* (get-x (first gesture)) x-scale)
                          (* (get-y (first gesture)) y-scale))
                    (scale-gesture (rest gesture) x-scale y-scale))]))

;; iv

;; (get-b-box gesture) produces the gesture's bounding box
;; Examples:
(check-expect (get-b-box test-gesture-1) (list (list -3 2) (list 1 4)))
(check-expect (get-b-box test-gesture-2) (list (list 0 0) (list 200 200)))
(check-expect (get-b-box test-gesture-3) (list (list 1 1) (list 3 2)))

;; get-b-box: Gesture -> (list Point Point)
(define (get-b-box gesture)
  (list (list (find-min (create-lst-x-coord gesture))
              (find-min (create-lst-y-coord gesture)))
        (list (find-max (create-lst-x-coord gesture))
              (find-max (create-lst-y-coord gesture)))))


;; Helper functions:

;; (find-min lst-of-num) produces the minimum element in lst-of-num
;; Examples:
(check-expect (find-min (list 5 4 3 2 1)) 1)
(check-expect (find-min (list 5 4 3 -3 1)) -3)

;; find-min: (listof Num) -> Num
(define (find-min lst-of-num)
  (cond [(empty? (rest lst-of-num)) (first lst-of-num)]
        [(>= (first lst-of-num) (first (rest lst-of-num)))
         (find-min (rest lst-of-num))]
        [else (find-min (cons (first lst-of-num)
                              (rest (rest lst-of-num))))]))

;; (find-max lst-of-num) produces the maximum element in lst-of-num
;; Examples:
(check-expect (find-max (list 5 4 3 20 1)) 20)
(check-expect (find-max (list 5 42 3 -3 11)) 42)

;; find-max: (listof Num) -> Num
(define (find-max lst-of-num)
  (cond [(empty? (rest lst-of-num)) (first lst-of-num)]
        [(<= (first lst-of-num) (first (rest lst-of-num)))
         (find-max (rest lst-of-num))]
        [else (find-max (cons (first lst-of-num)
                              (rest (rest lst-of-num))))]))

;; (create-lst-x-coord lst) produces a list of all the x-coordinates
;;    of the points in lst
;; Examples:
(check-expect (create-lst-x-coord (list (list 1 2) (list -3 4)))
              (list 1 -3))
(check-expect (create-lst-x-coord (list (list 100 0) (list 200 100) (list 100 200) (list 0 100)))
              (list 100 200 100 0))

;; create-lst-x-coord: (listof Point) -> (listof Num)
(define (create-lst-x-coord lst)
  (cond [(empty? lst) lst]
        [else (cons (first (first lst)) (create-lst-x-coord (rest lst)))]))

;; (create-lst-y-coord lst) produces a list of all the y-coordinates
;;    of the points in lst
;; Examples:
(check-expect (create-lst-y-coord (list (list 1 2) (list -3 4)))
              (list 2 4))
(check-expect (create-lst-y-coord (list (list 100 0) (list 200 100) (list 100 200) (list 0 100)))
              (list 0 100 200 100))

;; create-lst-y-coord: (listof (list Num Num)) -> (listof Num)
(define (create-lst-y-coord lst)
  (cond [(empty? lst) lst]
        [else (cons (first (rest (first lst))) (create-lst-y-coord (rest lst)))]))

;; 3b)
;; Full design recipe required.

;; i.

;; (gesture-length gesture) produces the length of a gesture
;; Examples:
(check-within (gesture-length test-gesture-1) (* 2 (sqrt 5)) 0.01)
(check-within (gesture-length test-gesture-2) (* 400 (sqrt 2)) 0.01)
(check-within (gesture-length test-gesture-3) (+ 2 (* 2 (sqrt 2))) 0.01)
  
;; gesture-length: Gesture -> Num
(define (gesture-length gesture)
  (cond [(or (= (length gesture) 0)
             (= (length gesture) 1)) 0]
        [(= (length gesture) 2)
         (calc-distance-2-points (first gesture)
                                 (first (rest gesture)))]
        [else (calc-gesture-length gesture gesture)]))

;; Tests
(check-expect (gesture-length test-gesture-4) 0) ;; singular point
(check-expect (gesture-length empty) 0)
(check-within (gesture-length (list (list -9 4) (list 12 7) (list 3 8)
                                    (list 21 -13) (list -5 -6))) 95.62 0.01)



;; (calc-gesture-length gesture) calculates the length of gesture
;; Examples:
(check-within (calc-gesture-length test-gesture-2 test-gesture-2)
              (* 400 (sqrt 2)) 0.01)
(check-within (calc-gesture-length test-gesture-3 test-gesture-3)
              (+ 2 (* 2 (sqrt 2))) 0.01)

;; calc-gesture-length: Gesture -> Num
(define (calc-gesture-length gesture original-gesture)
  (cond [(= (length gesture) 1) (calc-distance-2-points
                                 (first original-gesture)
                                 (first gesture))]
        [else (+ (calc-distance-2-points (first gesture)
                                         (first (rest gesture)))
                 (calc-gesture-length (rest gesture) original-gesture))]))




;; (calc-distance-2-points 1st-point 2nd-point) produces the distance
;;    between 1st-point and 2nd-point
;; Examples:
(check-within (calc-distance-2-points (list 1 1) (list 2 2)) (sqrt 2) 0.01)
(check-expect (calc-distance-2-points (list 2 2) (list 5 6)) 5)
(check-within (calc-distance-2-points (list 1 2) (list -3 4))
              (* 2 (sqrt 5)) 0.01)

;; calc-distance-2-points: Point Point -> Num
(define (calc-distance-2-points 1st-point 2nd-point)
  (sqrt (+ (sqr (- (first  1st-point) (first 2nd-point)))
           (sqr (- (first (rest 1st-point)) (first (rest 2nd-point)))))))


;; 3b)

;; ii.

;; Constants for 3b ii.

(define mygest (list (list 100 0) (list 200 100) (list 100 200)
                     (list 0 100) (list 100 50)))

;; (get-points g list-of-nats) Produces a new gesture where each
;;    Point un the produced gesture is index by one element of the
;;    of Nat comsumed
;; Examples:
(check-expect (get-points mygest (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200)
                    (list 100 50) (list 100 50)))

(check-expect (get-points mygest (list 0 0 1 2 3 3 3 4 4))
              (list (list 100 0) (list 100 0) (list 200 100)
                    (list 100 200) (list 0 100) (list 0 100)
                    (list 0 100) (list 100 50) (list 100 50)))

;; Requires:
;;   lst-of-nats to be in non-decreasing order
;;   largest natural number in lst-of-nats can be at most (n-1)
;;   where n is the length of g  (# of points in g)

;; get-points: Gesture (listof Nat) -> Gesture
(define (get-points g lst-of-nat)
  (cond [(not (empty? lst-of-nat))
         (cond [(= (first lst-of-nat) 0)
                (cons (first g) (get-points g (rest lst-of-nat)))]
               [else (get-points (rest g) (sub1-each-ele lst-of-nat))])]
        [else empty]))

;; (sub1-each-ele lst-of-nat) produces a list of natural numbers
;;    almost idential to lst-of-nat but each element is reduced by 1
;; Examples:
(check-expect (sub1-each-ele (list 1 2 2 3 3 3 4 8))
              (list 0 1 1 2 2 2 3 7))

;; sub1-each-ele: (listof Nat) -> (listof Nat)
(define (sub1-each-ele lst-of-nat)
  (cond [(empty? lst-of-nat) lst-of-nat]
        [else (cons (sub1 (first lst-of-nat))
                    (sub1-each-ele (rest lst-of-nat)))]))


;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample g)
  (get-points g (list 0
                      (floor (* 0.25 (length g)))
                      (floor (* 0.50 (length g)))
                      (floor (* 0.75 (length g)))
                      (sub1 (length g)))))


;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))



;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0

(define (move-and-scale gesture x-scale y-scale)
  (cond [(and (= (first (first (get-b-box gesture))) 0)
              (= (second (first (get-b-box gesture))) 0))
         (scale-gesture gesture x-scale y-scale)]
        [else (move-and-scale (translate-to-origin gesture) x-scale y-scale)]))

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))



;; (translate-to-origin gesture) produces a new gesture that is
;;    almost the same as gesture but just translated to the origin

;; translate-to-origin: gesture -> gesture
(define (translate-to-origin gesture)
  (translate-gesture gesture
                     (* (find-min (create-lst-x-coord gesture)) -1)
                     (* (find-min (create-lst-y-coord gesture)) -1)))


;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
 (check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 30 0) (list 100 20)))
              (list (list 0 0) (list 200 20)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty

(define (normalize-gesture gesture)
  (cond [(< (max-change-in-x gesture) 30)
         (move-and-scale gesture 1 (/ norm-size (max-change-in-y gesture)))]
        [(< (max-change-in-y gesture) 30)
         (move-and-scale gesture (/ norm-size (max-change-in-x gesture)) 1)]
        [else
         (move-and-scale gesture
                         (/ norm-size (max-change-in-x gesture))
                         (/ norm-size (max-change-in-y gesture)))]))

;;(move-and-scale gesture x-scale y-scale)

;; Tests:
(check-within (get-b-box (normalize-gesture testa))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (get-b-box (normalize-gesture test-gesture-2))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (get-b-box (normalize-gesture testb))
              (list (list 0 0) (list 200 200)) 0.01)


;; (max-change-in-x gesture) Produces the maximum horizontal distance
;;    of the gesture
;; Examples:
(check-expect (max-change-in-x test-gesture-1) 4)
(check-expect (max-change-in-x test-gesture-2) 200)

;; max-change-in-x: Gesture -> Num
(define (max-change-in-x gesture)
  (- (find-max (create-lst-x-coord gesture))
     (find-min (create-lst-x-coord gesture))))



;; (max-change-in-y gesture) Produces the maximum vertical distance
;;    of the gesture
;; Examples:
(check-expect (max-change-in-y test-gesture-1) 2)
(check-expect (max-change-in-y test-gesture-2) 200)

;; max-change-in-x: Gesture -> Num
(define (max-change-in-y gesture)
  (- (find-max (create-lst-y-coord gesture))
     (find-min (create-lst-y-coord gesture))))



;; 3civ)

;; Constant for 3civ
;; Since we normalized a gesture of 5 sample points, will always 
;;    have exactly 5 points, so we define k to always equal 5:
(define k-5 5)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

(check-within (geometric-5match
               (second (fourth templates))
               (second (fourth templates))) 0 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal

(define (geometric-5match gesture1 gesture2)
  (/ (calc-total-distance (normalize-gesture (five-sample gesture1))
                          (normalize-gesture (five-sample gesture2)))
     k-5))

;; Tests:
(check-expect (geometric-5match
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))) 0)
(check-within (geometric-5match test-gesture-2 test-gesture-3) 149.04 0.01)

;; (calc-total-distance norm-gest-1 norm-gest-2) produces the total
;;    distance between the points of the normalized gesture of k 
;;    points

;; calc-total-distance: Gesture Gesture -> Num
(define (calc-total-distance norm-gest-1 norm-gest-2)
  (cond [(zero? (length norm-gest-1)) 0]
        [else (+ (calc-distance-2-points (first norm-gest-1)
                                         (first norm-gest-2))
                 (calc-total-distance (rest norm-gest-1)
                                      (rest norm-gest-2)))]))


;; 3cv)

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal

(define (five-point-rec gesture templates)
  (determine-symbol (find-min (create-lst-geo-5match gesture templates))
                    (create-lst-geo-5match gesture templates)
                    templates))


;; Tests
(check-expect (five-point-rec testa templates) 'a)
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testi templates) 'i)
(check-expect (five-point-rec testi-2 templates) 'i)
(check-expect (five-point-rec testk templates) 'k)
;; (check-expect (five-point-rec testo templates) 'o)
;; (check-expect (five-point-rec testq templates) 'q)
;; (check-expect (five-point-rec testr templates) 'r)
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testt templates) 't)
(check-expect (five-point-rec testw templates) 'w)
(check-expect (five-point-rec testy templates) 'y)
(check-expect (five-point-rec testz templates) 'z)

;;

;; (determine-symbol par1 par2 ...) produces the symbol in
;;    template-library closest to candidate

;; determine-symbol: Num (listof Num) TL -> Sym
(define (determine-symbol expected-geo-5match lst-geo-5match templates)
  (cond [(empty? (rest templates)) (first (first templates))]
        [(= expected-geo-5match (first lst-geo-5match))
         (first (first templates))]
        [else (determine-symbol expected-geo-5match
                           (rest lst-geo-5match)
                           (rest templates))]))
         

;; (create-lst-geo-5match gesture templates) creates a list of
;;    geometric match distance bewteen the gesture and each template

;; create-lst-geo-5match: Gesture TL -> Num
(define (create-lst-geo-5match gesture templates)
  (cond [(empty? templates) empty]
        [else (cons (geometric-5match gesture (second (first templates)))
                    (create-lst-geo-5match gesture (rest templates)))]))


;; part d

;;(sub-sample gesture k) produces a sampling of gesture k points

;; Examples:
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 5)
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)) 5)
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))


;; sub-sample: Gesture Nat -> Gesture
;; requires:
;;    gesture is non-empty
;;    k > 2

(define (sub-sample g k)
  (get-points g (create-lst-nat g 0 k)))

(define (create-lst-nat g n k)
  (cond [(= n (sub1 k)) (cons (sub1 (length g)) empty)]
        [else (cons (floor (* (*(/ 1 (- k 1)) n) (length g)))
                    (create-lst-nat g (add1 n) k))]))
;; Tests:
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 5)
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (sub-sample (list (list 1 1)) 5)
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)) 5)
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))



;;(geometric-match gesture1 gesture2 k) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))
               5)
               16.16 0.01)

(check-within (geometric-match
               (second (fourth templates))
               (second (fourth templates)) 5) 0 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
;; requires:
;;   gesture1 and gesture2 are each not both vertical and horizontal
;;   k > 2

(define (geometric-match gesture1 gesture2 k)
  (/ (calc-total-distance (normalize-gesture (sub-sample gesture1 k))
                          (normalize-gesture (sub-sample gesture2 k)))
     k))

;; Tests:
(check-expect (geometric-match
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))
               5) 0)
(check-within (geometric-match test-gesture-2 test-gesture-3 5) 149.04 0.01)



;;(k-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec testd templates 10) 'd)
(check-expect (k-point-rec testk templates 9) 'k)


;; k-point-rec Gesture TL Nat -> Sym
;; requires:
;;    candidate is not both vertical and horizontal
;;    k > 2

(define (k-point-rec gesture templates k)
  (determine-symbol-new (find-min (create-lst-geo-kmatch gesture templates k))
                        (create-lst-geo-kmatch gesture templates k)
                        templates))
;; Tests
(check-expect (k-point-rec testa templates 5) 'a)
(check-expect (k-point-rec testc templates 9) 'c)
(check-expect (k-point-rec testd templates 6) 'd)
(check-expect (k-point-rec teste templates 9) 'e)
(check-expect (k-point-rec testf templates 11) 'f)
(check-expect (k-point-rec testh templates 10) 'h)
(check-expect (k-point-rec testi templates 7) 'i)
(check-expect (k-point-rec testi-2 templates 8) 'i)
(check-expect (k-point-rec testj templates 9) 'j)
(check-expect (k-point-rec testk templates 9) 'k)
(check-expect (k-point-rec testl templates 10) 'l)
(check-expect (k-point-rec testn templates 13) 'n)
(check-expect (k-point-rec testp templates 8) 'p)
(check-expect (k-point-rec testr templates 20) 'r)
(check-expect (k-point-rec tests templates 10) 's)
(check-expect (k-point-rec testt templates 7) 't)
(check-expect (k-point-rec testu templates 8) 'u)
(check-expect (k-point-rec testv templates 9) 'v)
(check-expect (k-point-rec testw templates 6) 'w)
(check-expect (k-point-rec testx templates 9) 'x)
(check-expect (k-point-rec testy templates 7) 'y)
(check-expect (k-point-rec testz templates 8) 'z)



;; (create-lst-geo-kmatch gesture templates) creates a list of
;;    geometric match distance bewteen the gesture and each template

;; create-lst-geo-kmatch: Gesture TL -> Num
(define (create-lst-geo-kmatch gesture templates k)
  (cond [(empty? templates) empty]
        [else (cons (geometric-match gesture (second (first templates)) k)
                    (create-lst-geo-kmatch gesture (rest templates) k))]))




;; determine-symbol: Num (listof Num) TL -> Sym
(define (determine-symbol-new expected-geo-kmatch lst-geo-kmatch templates)
  (cond [(empty? (rest templates)) (first (first templates))]
        [(= expected-geo-kmatch (first lst-geo-kmatch))
         (first (first templates))]
        [else (determine-symbol-new expected-geo-kmatch
                           (rest lst-geo-kmatch)
                           (rest templates))]))
         