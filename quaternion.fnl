(local Quaternion {})
(set Quaternion.mt {})

(macro defn [name args ...]
  {:binding-form? true
   :body-form? true}
  (fn symdot [a b]
    (sym (.. (tostring a) "." (tostring b))))
  `(set ,(symdot 'Quaternion name)
    (fn ,name ,args ,...)))

(fn Quaternion.mt.__eq [[t x y z] [T X Y Z]]
  (and (= t T) (= x X) (= y Y) (= z Z)))
(defn == [u v]
  (let [u (if (number? u) (quat u) u)
        v (if (number? v) (quat v) v)]
    (= u v)))
(fn with-mt [q mt]
  (setmetatable q mt)
  q)
(fn Quaternion.new [[t x y z]]
  (with-mt [t x y z] Quaternion.mt))
(defn quat [t x y z]
  (let [t (or t 0)
        x (or x 0)
        y (or y 0)
        z (or z 0)]
    (Quaternion.new [t x y z])))
(assert (= (quat) (quat 0 0 0 0)))

(defn quat? [z] (= (getmetatable z) Quaternion.mt))
(assert (quat? (quat 1 0 0 0)))
(defn ->quat [z]
  (if (quat? z) z
      (Quaternion.new [z 0 0 0])))
(assert (= (->quat (quat 1 0 0 0))
           (quat 1 0 0 0)))

(defn vec [x y z] (quat 0 x y z))
(defn vec? [q]
  (and (quat? q)
       (let [[r _ _ _] q]
         (= r 0))))
(defn complex [x y] (quat x y))

(fn Quaternion.mt.__add [a b]
  (let [[t x y z] (->quat a)
        [T X Y Z] (->quat b)]
    (quat (+ t T) (+ x X) (+ y Y) (+ z Z))))

(assert (= (+ (quat 1 0 0 0)
              (quat 0 1 0 0))
           ;;----------------
           (quat    1 1 0 0)))

(fn Quaternion.mt.__unm [[t x y z]]
  (quat (- t) (- x) (- y) (- z)))
(fn q-plus-neg-q-is-0 [q]
  (assert (= (+ q (- q)) (quat))))
(q-plus-neg-q-is-0 (quat 1))
(q-plus-neg-q-is-0 (quat 1 2 3 4))

(fn number? [x] (= (type x) :number))
(fn Quaternion.mt.__sub [a b] (+ (->quat a) (->quat (- b))))

(defn scale [k q]
  (let [[t x y z] (->quat q)]
    (quat (* k t) (* k x) (* k y) (* k z))))
(fn Quaternion.mt.__mul [q Q]
  (if (number? q) (scale q Q)
      (number? Q) (scale Q q)
      (let [[t x y z] q
            [T X Y Z] Q]
        (quat
         ;; r R      - v · V
         (- (* t T) (* x X) (* y Y) (* z Z))
         ;;---- tV + vT ----+--------------- v × V --------------+
         ;;                 |                  jk          kj    |
         (+ (* t X) (* x T)                  (* y Z)  (- (* z Y)))
         ;;                 |        ik                    ki
         (+ (* t Y) (* y T)   (- (* x Z))                (* z X) )
         ;;                 |        ij        ji
         (+ (* t Z) (* z T)      (* x Y)  (- (* y X))            )))))
(defn conj [q]
  (let [[t x y z] (->quat q)]
    (quat t (- x) (- y) (- z))))
(assert (= (conj (quat 0 1)) (quat 0 -1)))
(assert (= (conj (quat 0 1 2 3)) (quat 0 -1 -2 -3)))
(assert (= (conj (quat 4 1 2 3)) (quat 4 -1 -2 -3)))
(assert (= (conj (quat 1)) (quat 1)))

(fn sqr [x] (* x x))
(defn abs2 [q]
  (let [[t x y z] (->quat q)]
    (+ (sqr t) (sqr x) (sqr y) (sqr z))))
(defn abs [q] (math.sqrt (abs2 q)))

(fn Quaternion.inverse [z]
  ;; q (q^*/qq^*) = 1
  ;; q (q^*/abs2(q)) =1
  (let [R (abs2 z)]
    (scale (/ R) (conj z))))
(fn Quaternion.mt.__div [u v]
  (if (number? v) (scale (/ v) u)
      (* (->quat u) (Quaternion.inverse (->quat v)))))

(fn q-times-inv-q-is-1 [q]
  (assert (= (* q (/ q)) (quat 1))))
(q-times-inv-q-is-1 (quat 1))
(q-times-inv-q-is-1 (quat 1 2 3 4))

(set Quaternion.i (quat 0 1))
(set Quaternion.j (quat 0 0 1))
(set Quaternion.k (quat 0 0 0 1))

(assert (= (/ (quat 0 1))     (quat 0 -1)))
(assert (= (/ (quat 0 0 1))   (quat 0  0 -1)))
(assert (= (/ (quat 0 0 0 1)) (quat 0  0  0 -1)))

(assert (= (/ (quat 2)) (quat .5)))

(assert (= (* Quaternion.i Quaternion.i) (quat -1)))
(assert (= (* Quaternion.j Quaternion.j) (quat -1)))
(assert (= (* Quaternion.k Quaternion.k) (quat -1)))
(assert (= (* Quaternion.i Quaternion.j Quaternion.k)
           (quat -1)))

(defn ->scalar [q]
  (let [[t _ _ _] (->quat q)]
    (quat t)))
(defn ->vector [q]
  (let [[_ x y z] (->quat q)]
    (quat 0 x y z)))

;; TODO how do i make keyword args?
(defn exp [q terms]
  (let [terms (or terms 30)]
    ((fn loop [sum n qn n!]
       (if (= n terms) sum
           (loop
            (+ (/ qn n!) sum)
            (+ n 1)
            (* q qn)
            (* (+ n 1) n!))))
     0 0 1 1)))

(fn ≈ [x y]
  (< (abs2 (- x y)) .000001))
(assert (= (exp (quat 0))
           (quat 1)))
(assert (≈
         (exp (quat 0 math.pi))
         (quat -1)))

Quaternion
