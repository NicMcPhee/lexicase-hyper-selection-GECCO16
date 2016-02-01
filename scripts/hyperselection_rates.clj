(defn expt [x n]
  (cond
   (zero? n) 1
   (even? n) (expt (*' x x) (/ n 2))
   :else (*' x (expt x (dec n)))))

(defn probability-of-winning-tournament
  [population-size tournament-size rank-in-population]
  (/ (- (expt (+ population-size (- rank-in-population) 1.0) tournament-size)
        (expt (- population-size rank-in-population) tournament-size))
     (expt population-size tournament-size)))

(probability-of-winning-tournament 1000 7 1)

(map (partial probability-of-winning-tournament 1000 7) (range 1 10))

(def factorial
  (memoize (fn [n]
             (reduce *' (range 1 (inc n))))))

(def choose
  (memoize (fn [n k]
             (/ (factorial n)
                (* (factorial k) (factorial (- n k)))))))

(defn probability-of-hyperselection
  [population-size tournament-size num-selections percent-level rank-in-population]
  (let [target-selections (int (Math/ceil (* percent-level num-selections)))
        p (probability-of-winning-tournament population-size tournament-size rank-in-population)]
    (reduce
    +
    (remove #(Double/isNaN %)
     (map (fn [k] (* (choose num-selections k)
                     (expt p k)
                     (expt (- 1 p) (- num-selections k))))
          (range target-selections (inc num-selections)))))))

(time (reduce * (map #(- 1 %) (map (partial probability-of-hyperselection 1000 7 1700 0.01) (range 1 1000)))))
