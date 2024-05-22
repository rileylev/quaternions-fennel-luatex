(local Quaternion {})
(set Quaternion.mt {})

(macro defn [name args ...]
  {:binding-form? true
   :body-form? true}
  (fn symdot [a b]
    (sym (.. (tostring a) "." (tostring b))))
  `(set ,(symdot 'Quaternion name)
    (fn ,name ,args ,...)))

(fn test [name ...] ...)

(fn number? [x] (= (type x) :number))

(fn Quaternion.mt.__eq [[t x y z] [T X Y Z]]
  (and (= t T) (= x X) (= y Y) (= z Z)))

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
(test "The nullary quat is the zero quaterinon"
      (assert (= (quat) (quat 0 0 0 0))))

(defn quat? [z] (= (getmetatable z) Quaternion.mt))
(test "quat constructs quaternions (quat?=true)"
      (assert (quat? (quat 1 0 0 0))))
(test "scalars are not quaternions"
      (assert (not (quat? 0))))
(test "strings are not quaternions"
      (assert (not (quat? "x"))))

(defn ->quat [z]
  (if (quat? z) z
      (number? z) (Quaternion.new [z 0 0 0])
                  (error ":C")))
(test "->quat does nothing if the argument is already a quaternion"
      (assert (= (->quat (quat 1 0 0 0))
                 (quat 1 0 0 0))))

(defn == [u v]
  (let [u (if (number? u) (quat u) u)
        v (if (number? v) (quat v) v)]
    (= u v)))
(test "a number is == to its image in the quaternions under inclusion"
      (assert (== 0 (quat)))
      (assert (== 1 (quat 1))))

(fn Quaternion.mt.__add [a b]
  (let [[t x y z] (->quat a)
        [T X Y Z] (->quat b)]
    (quat (+ t T) (+ x X) (+ y Y) (+ z Z))))

(test "quaternions add componentwise"
      (assert (= (+ (quat 1 0 0 0)
                    (quat 0 1 0 0))
                 ;;----------------
                 (quat    1 1 0 0))))
(test "addition promotes numbers to quaternions"
      (assert (= (quat 2)
                 (+ (quat 1)  1)
                 (+ 1         (quat 1)))))

(fn Quaternion.mt.__unm [[t x y z]]
  (quat (- t) (- x) (- y) (- z)))
(test "unary minus is the additive inverse"
      (fn q-plus-neg-q-is-0 [q]
        (assert (= (+ q (- q)) (quat))))
      (q-plus-neg-q-is-0 (quat 1))
      (q-plus-neg-q-is-0 (quat 1 2 3 4)))

(fn Quaternion.mt.__sub [a b] (+ (->quat a) (->quat (- b))))

(defn scale [k q]
  (let [[t x y z] (->quat q)]
    (quat (* k t) (* k x) (* k y) (* k z))))
(fn Quaternion.mt.__mul [q Q]
  (assert q)
  (assert Q)
  (if (number? q) (scale q Q)
      (number? Q) (scale Q q)
      (let [[t x y z] q
            [T X Y Z] Q]
        ;; (t + v⃗) (T + V⃗) = tT - v⃗ · V⃗  + tV⃗  + Tv⃗  + v⃗  × V⃗
        (quat
         ;; r R      - v⃗  · V⃗
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
(test "conjugation negates the vector part"
      (assert (= (conj (quat 0 1)) (quat 0 -1)))
      (assert (= (conj (quat 0 1 2 3)) (quat 0 -1 -2 -3)))
      (assert (= (conj (quat 4 1 2 3)) (quat 4 -1 -2 -3))))
(test "conjugation fixes 'real' quaternions"
      (assert (== (conj (quat 1)) 1)))

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

(test "(/ q) is the multiplicative inverse"
      (fn q-times-inv-q-is-1 [q]
        (assert (= (* q (/ q)) (quat 1))))
      (q-times-inv-q-is-1 (quat 1))
      (q-times-inv-q-is-1 (quat 1 2 3 4)))
(test "(/ i) = -i, (/ j)= -j, (/ k) = -k"
      (assert (= (/ (quat 0 1))     (quat 0 -1)))
      (assert (= (/ (quat 0 0 1))   (quat 0  0 -1)))
      (assert (= (/ (quat 0 0 0 1)) (quat 0  0  0 -1))))
(test "(/ _) is equivalent to ordinary inverse on 'real' quaternions"
      (assert (= (/ (quat 2)) (quat .5))))

(set Quaternion.i (quat 0 1))
(set Quaternion.j (quat 0 0 1))
(set Quaternion.k (quat 0 0 0 1))

(test "i²=j²=k²=ijk=-1"
      (assert (== (* Quaternion.i Quaternion.i)              -1))
      (assert (== (* Quaternion.j Quaternion.j)              -1))
      (assert (== (* Quaternion.k Quaternion.k)              -1))
      (assert (== (* Quaternion.i Quaternion.j Quaternion.k) -1)))

(defn ->scalar [q]
  (let [[t _ _ _] (->quat q)]
    (quat t)))
(defn ->vector [q]
  (let [[_ x y z] (->quat q)]
    (quat 0 x y z)))

(defn vec [x y z] (quat 0 x y z))
(defn realpart [[r _ _ _]] r)
(defn realquat? [q]
  (and (quat? q)
       (let [[t x y z] q]
         (= 0 x y z))))
(defn real? [q]
  (or (number? q) (realquat? q)))
(defn vec? [q]
  (and (quat? q) (= (realpart q) 0)))
(defn cross [u v] (->vector (* u v)))
(defn dot   [u v] (- (realpart (* u v))))
(defn complex [x y] (quat x y))

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
(test "e⁰ = 1"
      (assert (== (exp (quat 0)) 1)))
(test "exp(iπ)=-1"
      (assert (≈ (exp (quat 0 math.pi))
                 (quat -1))))

(defn hopf [q k]
  (let [k (or k Quaternion.k)]
    (* q k (/ q))))

(defn stereo1 [q ε]
  (let [ε         (or ε .001)
        q         (->quat q)
        [t x y z] q
        shrink    (/ (- (+ 1 ε) t))]
    (scale shrink (vec x y z))))
(defn stereok [q ε]
  (let [q         (->quat q)
        [t x y z] q]
    (stereo1 (quat z x y t) ε)))

(defn tikzprint [v]
  (let [[_ x y z] v]
    (tex.print (.. x ", " y "," z))))

(defn G [α]
  (fn [s t]
    (+ (* (math.cos α)
          (exp (* s Quaternion.k)))
       (* (math.sin α)
          (exp (- (* t Quaternion.k)))
          Quaternion.j))))

(defn draw [a s t]
  (tikzprint (stereok ((G a) s t))))
(defn draw2 [a u s t]
  (let [[t x y z] (* ((G a) (+ s) (- t))
                     (exp (* u Quaternion.k)))]
    (tikzprint (stereo1 (quat x t y z)))))

Quaternion
