(define f
   (lambda (x)
   (cond
     [(= x 0) 1]
      [else (* (f (- x 1)) x)])))

(define m
  (lambda (i type)
    (length (filter (lambda (x) (= i x)) type))))

(define n
  (lambda (type)
    (cond
      [(equal? '() type) 0]
      [else (+ (car type) (n (cdr type)))])))

(define build-list
  (lambda (m g)
    (cond
      [(= 0 m) '()]
      [else (cons (g m) (build-list (- m 1) g))])))

(define total
  (lambda (type)
     (/ (f (n type))
        (apply * (map (lambda (i)
                          (* (expt i (m i type))
                          (f (m i type))))
                      (build-list (n type)
                                  identity))))))

(define bad
  (lambda (type)
    (/ (* (f (- (n type) 2))
          (apply + (map (lambda (i)
                           (* (m i type) i (- i 1)))
                        (build-list (n type)
                                  identity))))
       (apply * (map (lambda (i)
                          (* (expt i (m i type))
                          (f (m i type))))
                      (build-list (n type)
                                  identity))))))

(define good
  (lambda (type)
    (- (total type)
       (bad type))))

(define thelist
 '((12)
   (10 1 1)
   (9 2 1)
   (8 3 1)
   (8 2 2)
   (8 1 1 1 1)
   (7 4 1)
   (7 3 2)
   (7 2 1 1 1)
   (6 5 1)
   (6 4 2)
   (6 3 3)
   (6 3 1 1 1)
   (6 2 2 1 1)
   (6 1 1 1 1 1 1)
   (5 5 2)
   (5 4 3)
   (5 4 1 1 1)
   (5 3 2 1 1)
   (5 2 2 2 1)
   (5 2 1 1 1 1 1)
   (4 4 4)
   (4 4 2 1 1)
   (4 3 3 1 1)
   (4 3 2 2 1)
   (4 3 1 1 1 1 1)
   (4 2 2 2 2)
   (4 2 2 1 1 1 1)
   (4 1 1 1 1 1 1 1 1)
   (3 3 3 2 1)
   (3 3 2 2 2)
   (3 3 2 1 1 1 1)
   (3 2 2 2 1 1 1)
   (3 2 1 1 1 1 1 1 1)
   (2 2 2 2 2 1 1)
   (2 2 2 1 1 1 1 1 1)
   (2 1 1 1 1 1 1 1 1 1 1)))

;;careful
(define theanswer
  (- (* (apply + (map good thelist))
        (expt 2 11))
     (/ (* (f 11) (expt 2 10)) 2)))

;(/ (f 12) 2)
;(apply + (map total thelist))
;(* (expt 2 11) (apply + (map total thelist)))
;theanswer
(/ theanswer (* (expt 3 7) (apply + (map total thelist))))