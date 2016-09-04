(ns sicp-clojure.c1-1)

;;;; 1.1 The Elements of Programming

;;; 1.1.4 Compound Procedures

;; A procedure definition is a compound operation that can be given a name and
;; then referred to as a unit.

(defn square
  "Return square of a number."
  [x]
  (* x x))

(defn sum-of-squares
  "Return sum of squares"
  [x y]
  (apply + (map square [x y])))

;; Now we can use `sum-of-squares' as a building block in constructing
;; future procedures.
(defn higher-order-function
  "Example Higher Order Function"
  [x]
  (sum-of-squares (+ x 1) (* x 2)))

(defn abs
  "Return absolute value of a number."
  [x]
  (if (< x 0)
    (- x)
    x))

(defn abs-with-cond
  [x]
  (cond
    (< x 0) (- x)
    (= x 0) 0
    :else x))

;;; 1.1.7 Square Roots by Newton's Method

;; The contrast betwen function and procedure reflects a general distinction
;; between declarative and imperative knowledge. How can you compute a square
;; root? One way is through successive approximations. Whenever we have a guess
;; `y' as the square root of `x', we can get a better approximation by
;; averaging y and x/y.
(defn sqrt1
  ""
  [x]
  (letfn [(average [a b] (/ (+ a b) 2.0))
          (improve [guess] (average guess (/ x (double guess))))
          (good-enough? [guess] (< (abs (- (square guess) x)) 0.001))]
    (loop [guess 1]
      (if (good-enough? guess)
        guess
        (recur (improve guess))))))

;; Exercise 1.6: Alyssa P. Hacker doesn't see why `if' nees to be provided as a
;; special form, and she defines a new version of if:

(defn new-if
  "A new way to do if"
  [predicate then-clause else-clause]
  (cond
    predicate then-clause
    :else else-clause))

;; What happens if she uses `new-if' to rewrite the square-root program?

;; You will have infinite recursion. A special form guarantees that the
;; then-clause will not be evaluated if the predicate is true, but the `new-if'
;; function will evaluate both parts, leading to infinite recursion.


;; Exercise 1.7: The `good-enough?' test used above will not be very effective
;; for finding the square roots of very small numbers. Also, in real computers,
;; arithmetic operations are almost always performed with limited
;; precision. This makes our test inadequate for very large numbers. Explain
;; these statements, with examples showing how the test fails for small and
;; large numbers. An alternative strategy for implementing `good-enough?' is to
;; watch how `guess' changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. Design a square-root-procedure
;; that uses this kind of end test.

(defn average
  "Find the average of some numbers"
  [& numbers]
  (let [len (count numbers)
        total (reduce + numbers)]
    (/ total (double len))))

(defn sqrt-improve
  "Improve the guess of the number"
  [guess number]
  (average guess (/ number (double guess))))

(defn good-enough?
  "Return true if the guess is good enough"
  [y0 y1]
  (let [target 0.00001
        delta (abs (- y0 y1))]
    (< (/ delta y1) target)))

(defn sqrt-recur
  "Loops through guesses until currect one is arrived at"
  [number guess1 guess2]
  (if (good-enough? guess1 guess2)
    guess2
    (recur number guess2 (sqrt-improve guess2 number))))

(defn sqrt2
  "finds the square root of a number"
  [x]
  (sqrt-recur x 0 1))



;; Exercise 1.8: Newton's method for cube roots is based on the fact that if y
;; is an approximation to the cube root of x, then a bettconsidereder approximation is
;; given by the value
;;
;; x/y^2 + 2y
;; ----------
;;     3
;;
;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure.

(defn cube-root
  "Modified square root program"
  [x]
  (letfn [(improve [guess]
            (let [num1 (/ x (square guess))
                  num2 (* 2 guess)]
              (/ (+ num1 num2) 3.0)))
          (good-enough? [guess0 guess1]
            (let [target 0.00001
                  delta (abs (- guess0 guess1))]
              (< (/ delta guess1) target)))]
    (loop [guess0 0
           guess1 1]
      (if (good-enough? guess0 guess1)
        guess1
        (recur guess1 (improve guess1))))))

;;; 1.1.8 Procedure as Black-Box Abstractions

;; A decomposition strategy allows a task to be divided into subtasks. Given
;; that each sub-task can be assigned to a procedure, the user of the library
;; doesn't need to know the details. Because the implementation details can be
;; hidden, we say that it is a "procedural abstraction", because at that level,
;; any implementation that computes the result is equally good.
;;
;; Therefore, a procedure definition should be able to suppress detail.

;; Names that are local to the sub-task procedures should not matter. A variable
;; that is a format parameter of a procedure is a "bound variable", and it does
;; not matter if the variable is given a different name. If the variable is in
;; scope, but not local to the procedure, then it is considered a "free"
;; variable.

;;Besides having variables as local scope, it is also possible to hvae
;;procedures as local scope.

(defn sqrt
  "Modified square root program"
  [x]
  (letfn [(average [a b] (/ (+ a b) 2.0))
          (improve [guess] (average guess (/ x (double guess))))
          (good-enough? [guess0 guess1]
            (let [target 0.00001
                  delta (abs (- guess0 guess1))]
              (< (/ delta guess1) target)))
          ]
    (loop [guess0 0
           guess1 1]
      (if (good-enough? guess0 guess1)
        guess1
        (recur guess1 (improve guess1))))))

