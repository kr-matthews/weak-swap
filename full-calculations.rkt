#lang racket
(require math/number-theory)

;; multiplicity of i in a cycle type
(define m
  (λ (i type)
    (length (filter (curry = i) type))))

;; how many pieces a cycle type acts on
(define n
  (curry foldr + 0))

;; cycle structures acting on n pieces with cycles of size at most m
;; CODE ASSUMES THESE ARE EACH WEAKLY DECREASING
(define cycle-types
  (λ (n m)
    (cond
      [(zero? n) '(())]
      [else
       (apply append
              (map (λ (i)
                     (map (curry cons i)
                          (cycle-types (- n i) i)))
                   (build-list (min m n) add1)))])))

;; cycle structures for edges/corners
(define all-types
  (λ (x)
    (match x
      ['edges (cycle-types 12 12)]
      ['corners (cycle-types 8 8)])))

;; ways to overcount a particular permutation of given cycle type
(define overcount
  (λ (type)
    (foldr *
           1
           (map (λ (i)
                  (* (expt i (m i type))
                     (factorial (m i type))))
                (build-list (n type) add1)))))

;; amount of permutations of given cycle type
(define amount
  (λ (type)
    (/ (factorial (n type))
       (overcount type))))

;; does cycle type have parity?
(define parity?
  (λ (type)
    (odd? (length (filter even? type)))))

;; how many trivial cycles the cycle type has
(define trivials
  (λ (type)
    (length (filter (curry = 1) type))))

;; how many non-trivial cycles the cycle type has
(define non-trivials
  (λ (type)
    (length (filter (compose not (curry = 1)) type))))

;; amount of edge/corner configurations (not just permutations)
(define total
  (λ (x)
    (match x
      ['edges (* (expt 2 11) (factorial 12))]
      ['corners (* (expt 3 7) (factorial 8))])))

;; amount of edge/corner configurations with(out) parity (not just permutations)
(define with<out>-parity-total
  (λ (x)
    (/ (total x) 2)))

;; possible orientations for a piece type
(define orientations
  (λ (x)
    (match x
      ['edges 2]
      ['corners 3])))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; amount of permutations of given cycle type with buffer directly after helper in the single non-trivial cycle
(define amount-of-one-nontrivial-cycle-with-buffer-ending-on-helper
  (λ (type)
    (cond
      [(= 1 (non-trivials type))
       (/ (* (first type) ;; choices for buffer in non-trivial cycle
             1            ;; helper must be right before it
             (factorial (- (n type) 2)))
          (overcount type))]
      [else
       0])))

;; configurations with parity where you end with UR/UBR (not RU/RUB/BRU): if you avoid breaking to helper
(define parity-UR/UBR-finish:avoid-UR/UBR
  (λ (x)
    (* (match x
         ['edges (expt 2 10)]   ;; 10, not 11; half the time you will end with RU and not UR
         ['corners (expt 3 6)]) ;; 6, not 7; two thirds of the time you will end with BUR/RUB
       (foldr +
              0
              (map amount-of-one-nontrivial-cycle-with-buffer-ending-on-helper
                   (filter parity?
                           (all-types x)))))))


;; amount of permutations of given cycle type with buffer and helper in different cycles,
;; at most 2 non-trivial cycles, helper in non-trivial cycle, buffer in other (if possible)
(define amount-of-two-nontrivial-cycles-with-buffer-and-helper-in-one-each
  (λ (type)
    (cond
      [(= 2 (non-trivials type))
       (/ (* 2 ;; which goes in which cycle
             (first type) ;; where 1st goes in 1st cycle
             (second type) ;; where other goes in 2nd cycle
             (factorial (- (n type) 2))) ;; rest of the pieces
          (overcount type))]
      [(= 1 (non-trivials type))
       (/ (* (first type) ;; choices for helper
             (- (n type) (first type)) ;; choices for buffer (in place)
             (factorial (- (n type) 2))) ;; rest of the pieces
          (overcount type))]
      [else
       0])))

;; configurations with parity where you end with UR/UBR (not RU/RUB/BRU): if you always break to helper
(define parity-UR/UBR-finish:break-UR/UBR-first
  (λ (x)
    (+ (parity-UR/UBR-finish:avoid-UR/UBR x) ;; all these still count, plus the new (at-most-)2-non-trivial-cycle cases next
       (* (match x
            ['edges (expt 2 10)]   ;; 10, not 11; half the time you will end with RU and not UR
            ['corners (expt 3 6)]) ;; 6, not 7; two thirds of the time you will end with BUR/RUB
          (foldr +
                 0
                 (map amount-of-two-nontrivial-cycles-with-buffer-and-helper-in-one-each
                      (filter parity?
                              (all-types x))))))))

;; configurations with parity where you end with UR/UBR (not RU/RUB/BRU): if you orient buffer into helper
(define parity-UR/UBR-finish:orient-buffer-into-UR/UBR-first-cycle-break
  (λ (x)
    (+ (parity-UR/UBR-finish:avoid-UR/UBR x) ;; all these still count, plus the new 2-non-trivial-cycle cases next
       (* (match x
            ['edges (expt 2 11)]
            ['corners (expt 3 7)])
          (foldr +
                 0
                 (map amount-of-two-nontrivial-cycles-with-buffer-and-helper-in-one-each
                      (filter parity?
                              (all-types x))))))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ These not actually used; replaced with better functions ~~~~~~~~~~~~~~~~~~~

;;; amount of permutations of given cycle type with buffer and helper in different cycles, helper in non-trivial cycle
;;; basically amount of permutations where you see buffer before helper gets solved
;;; this is generalized by amount-with-any-of-k-in-non-trivial-cycle-without-buffer (k=1)
;(define amount-with-helper-in-non-trivial-without-buffer
;  (λ (type)
;    (/ (foldr +
;              0
;              (map (λ (i)
;                     (* (* (m i type) i) ;; ways to put helper in an i-cycle
;                        (- (n type) i) ;; places to put buffer in a different cycle
;                        (factorial (- (n type) 2)))) ;; ways to place remaining pieces
;                   (build-list (sub1 (n type)) (compose add1 add1)))) ;; do this for each 2 =< i =< n (n will never work though)
;       (overcount type))))
;
;;; incorrect; doesn't account for cases where buffer is (oriented) in UR
;;; configurations with parity where you end with UR/UBR (not RU/RUB/BRU): if you weak swap with UR
;(define parity-UR/UBR-finish:weak-swap-with-UR/UBR
;  (λ (x)
;    (+ (parity-UR/UBR-finish:avoid-UR/UBR x) ;; all these still count; weak swap doesn't apply but you happen to finish with UR
;       (* (match x
;            ['edges (expt 2 11)]
;            ['corners (expt 3 7)])
;          (foldr +
;                 0
;                 (map amount-with-helper-in-non-trivial-without-buffer
;                      (filter parity?
;                              (all-types x)))))
;       (* (match x ;; cases where helper is in place but not oriented
;            ['edges (* (* 1 (expt 2 10)) ;; as below
;                       (factorial 11))]
;            ['corners (* (* 2 (expt 3 6)) ;; 2 choices for helper in place but misoriented, 3 for the rest except last
;                         (factorial 7))])
;          1/2)))) ;; only want parity scrambles

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; fraction of _parity_ scrambles with UR/UBR finish if... (also change 'edges to 'corners)
;; use exact->inexact to get a decimal (is there a better way?)
(/ (parity-UR/UBR-finish:avoid-UR/UBR 'edges)
   (with<out>-parity-total 'edges))
(/ (parity-UR/UBR-finish:break-UR/UBR-first 'edges)
   (with<out>-parity-total 'edges))
(/ (parity-UR/UBR-finish:orient-buffer-into-UR/UBR-first-cycle-break 'edges)
   (with<out>-parity-total 'edges))
;(/ (parity-UR/UBR-finish:weak-swap-with-UR/UBR 'edges)
;   (with<out>-parity-total 'edges))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ weak swap calculations ~~~~~~~~~~~~~~~~~~~~~~

;; We break the situations where you can weak swap (with any of k helpers on k distinct pieces) into 3 cases:
;;  A: There is a helper in a non-trivial cycle without the buffer.
;;   (We can weak swap in all these cases.)
;;   We will count the completement: All k helpers are in either a trivial cycle (fixed in place) or the same cycle as the buffer.
;;  B: Not A, and the buffer is not in a helper piece spot.
;;   We have subcases for 0 \leq l \leq k.
;;    B_l: l of the k helpers are in trivial cycles and k-l of the k helpers are in the cycle of the buffer.
;;       (We can weak swap if at least one of the l helpers are misoriented.)
;;  C: Not A, and the buffer is in a helper piece spot
;;   We have subcases for 0 \leq l \leq k-1 (k isn't possible)
;;    C_l: l of the k helpers are in trivial cycles and k-l of the k helpers are in the cycle of the buffer
;;       (We can weak swap if at least on of the l helpers is misoriented or the buffer is oriented.)

;; configurations with parity where you can weak swap with any of k helpers (on k distinct pieces)
;; useful if you know multiple edge-parity sets
(define parity-can-weak-swap-with-any-of-k-helpers
  (λ (k x)
    (apply + ;; add up the following
           (map (λ (type) ;; for a cycle structure type
                  (apply +
                         (map (λ (i) ;; and for the buffer being in a cycle of length i
                                (/ (+
                                    ;; all possible cases (when we (potentially) can weak swap)
                                    (* (* (m i type) i) ;; choices for buffer
                                       (factorial (sub1 (n type))) ;; choices for all n pieces
                                       (expt (orientations x) (sub1 (n type)))) ;; possible choices of orientation (last one no choice)
                                    ;; Case A: substract those where all k helpers are in either a trivial cycle
                                    ;; (fixed in place) or the same cycle as the buffer (when we (potentially) can't weak swap)
                                    (- (* (* (m i type) i) ;; choices for the buffer position
                                          (if (= 1 i) ;; choices for the k helpers
                                              (apply * ;; if i=1 then there are just the _other_ trivial cycles for them 
                                                     (build-list k
                                                                 (λ (j)
                                                                   (- (sub1 (trivials type)) j)))) ;; this many choices for the jth helper (indexed from 0)
                                              (apply * ;; if i>1 then they can go in trivials or the same cycle
                                                     (build-list k
                                                                 (λ (j)
                                                                   (- (+ (trivials type)
                                                                         (sub1 i))
                                                                      j)))))
                                          (factorial (- (n type) (add1 k))) ;; choices for the other n-(k+1) pieces
                                          (expt (orientations x) (sub1 (n type))))) ;; possible orientations for each piece (last one no choice)
                                    ;; Case B: add back (some of) the things we shouldn't have subtracted in case A (when we can weak swap)
                                    (apply + ;; add up
                                           (map (λ (l) ;; for each l (l helpers in trivial cycles, k-l in buffer cycle)
                                                  (* (* (m i type) i) ;; choices for the buffer
                                                     (binomial k l) ;; choices for which l helpers to be in trivial cycles
                                                     (apply * ;; choices to put these l helpers down
                                                            (build-list l
                                                                        (λ (j)
                                                                          (- (if (= i 1)
                                                                                 (sub1 (trivials type))
                                                                                 (trivials type))
                                                                             j))))
                                                     (apply * ;; choices to put the other k-l helpers in the buffer cycle
                                                            ;; but not on the right of the buffer (it's forbidden)
                                                            (build-list (- k l)
                                                                        (λ (j)
                                                                          (- (sub1 (sub1 i))
                                                                             j)))) ;; buffer takes a spot and the spot on its right must be empty
                                                     (factorial (- (n type) (add1 k))) ;; choices for the other n-(k+1) pieces
                                                     (sub1 (expt (orientations x) l)) ;; possible orientations for the l helpers (can't all be correct)
                                                     (expt (orientations x) (- (sub1 (n type)) l)))) ;; and for the remaning pieces (except the last)
                                                (build-list (add1 k)
                                                            identity)))
                                    ;; Case C: add back (the rest of) the things we shouldn't have subtracted in case A
                                    (apply + ;; add up
                                           (map (λ (l) ;; for each l (l helpers in trivial cycles, k-l in buffer cycle)
                                                  (* (* (m i type) i) ;; choices for the buffer
                                                     (binomial k l) ;; choices for which l helpers to be in trivial cycles
                                                     (apply * ;; choices to put these l helpers down
                                                            (build-list l
                                                                        (λ (j)
                                                                          (- (if (= i 1)
                                                                                 (sub1 (trivials type))
                                                                                 (trivials type))
                                                                             j))))
                                                     (- k l) ;; choices for which helper to put to the right of buffer (buffer is in helper's spot)
                                                     (apply * ;; choices to put the other k-l-1 helpers in the buffer cycle
                                                            ;; but not on the right of the buffer (it's already taken)
                                                            (build-list (max (sub1 (- k l)) 0)
                                                                        (λ (j)
                                                                          (- (sub1 (sub1 i))
                                                                             j)))) ;; buffer takes a spot and the spot on its right is taken already
                                                     (factorial (- (n type) (add1 k))) ;; choices for the other n-(k+1) pieces
                                                     (+ (expt (orientations x) l) ;; if the buffer is oriented then the l helpers can have any orientation
                                                            (* (sub1 (orientations x)) ;; if the buffer is not oriented...
                                                               (sub1 (expt (orientations x) l)))) ;;...then the l helpers can't all be correct
                                                     (expt (orientations x) (- (sub1 (n type)) (add1 l))))) ;; orientation of the other pieces
                                                (build-list (add1 k)
                                                            identity))))
                                   (overcount type))) ;; we counted the ways to fill in the diagram, so divide by how many times we counted each result
                              (build-list (n type) ;; over all possible cycle lengths the buffer could be in
                                          add1)))) ;; (list 1 ... n)
                (filter parity? ;; over all parity cycle structure types
                        (all-types x))))))

(exact->inexact (/ (parity-can-weak-swap-with-any-of-k-helpers 1 'edges)
                     (with<out>-parity-total 'edges)))