(defn common [f] (fn [& args] (fn [m] (apply f (mapv (fn [x] (x m)) args)))))
(defn constant [v] (fn [m] v))
(defn variable [v] (fn [m] (get m v)))
(def add (common +))
(def subtract (common -))
(def multiply (common *))
(defn divide [a b] (fn [m] (/ (double (a m)) (double (b m)))))
(def negate (common -))
(def exp (common (fn [x] (Math/exp x))))
(def ln (common (fn [x] (Math/log (Math/abs x)))))

(defn op-map [arg]
  (cond (= '+ arg) add
        (= '- arg) subtract
        (= '* arg) multiply
        (= '/ arg) divide
        (= 'exp arg) exp
        (= 'ln arg) ln
        :else negate))

(defn parse [l]
  (cond (number?  l) (constant l)
        (symbol? l) (variable (name l))
        :else (apply (op-map (nth l 0)) (mapv parse (rest l)))))

(defn parseFunction [str] (let [expression (read-string str)]
                            (parse expression)))