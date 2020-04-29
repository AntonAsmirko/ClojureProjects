(defn operation [op a b]  (mapv op a b))
(defn minor [a b i j] (- (* (nth a i) (nth b j)) (* (nth a j) (nth b i))))

(defn v+ [a b] (operation + a b))
(defn v- [a b] (operation - a b))
(defn v* [a b] (operation * a b))
(defn vd [a b] (operation / a b))
(defn v*s [a b] (mapv (fn [element] (* b element)) a))
(defn scalar [a b] (reduce + (v* a b)))
(defn vect [a b] [(minor a b 1 2) (minor a b 2 0) (minor a b 0 1)])

(defn m+ [a b] (operation v+ a b))
(defn m- [a b] (operation v- a b))
(defn m* [a b] (operation v* a b))
(defn md [a b] (operation vd a b))
(defn m*s [a b] (mapv (fn [line] (v*s line b)) a))

(defn m*v [a b] (mapv (fn [line] (scalar line b)) a))

(defn transpose [m] (apply mapv vector m))

(defn m*m [a b] (mapv (partial m*v (transpose b)) a))

(defn operationShapeless[op] (fn evaluate [a b]
                               (if (number? a)
                                 (op a b)
                                 (mapv evaluate a b))))

(def s+ (operationShapeless +))
(def s- (operationShapeless -))
(def s* (operationShapeless *))

(defn operationCuboid [op] (fn [& arg] (apply mapv op arg)))

(def c+ (operationCuboid (operationCuboid (operationCuboid +))))
(def c- (operationCuboid (operationCuboid (operationCuboid -))))
(def c* (operationCuboid (operationCuboid (operationCuboid *))))
(def cd (operationCuboid (operationCuboid (operationCuboid /))))