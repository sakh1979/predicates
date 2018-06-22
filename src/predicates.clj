(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [k] (contains? a-set k)))

(defn pred-and [pred1 pred2]
  (fn [n] (and (pred1 n) (pred2 n))))

(defn pred-or [pred1 pred2]
  (fn [n] (or (pred1 n) (pred2 n))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? (seq string)))

(defn has-award? [book award]
  ((set->predicate (:awards book)) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? #(has-award? book %) awards))

(defn my-some [pred a-seq]
  (let [result (->> a-seq
                    (map pred)
                    (filter identity))]
    (if (empty? result)
      false
      (first result))))

(defn my-every? [pred a-seq]
  (let [result (->> a-seq
                    (map pred)
                    (filter (complement identity)))]
    (if (empty? result)
      true
      (first result))))

(defn prime? [n]
  (let [pred (fn [x] (= (mod n x) 0))]
    (not (some pred (range 2 n)))))
;^^
