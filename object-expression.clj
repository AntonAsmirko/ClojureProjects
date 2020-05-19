(require '[clojure.string :as string])

(declare Add)
(declare Multiply)
(declare Subtract)
(declare Divide)
(declare Negate)
(declare Exp)
(declare Ln)

(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn constructor [ctor prototype]
  (fn [& args] (apply ctor {:prototype prototype} args)))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(def diff (method :diff))
(def evaluate (method :evaluate))
(def toString (method :toString))

(defn placeholder-cons [this val]
  {
   :prototype this
   :val       val
   })

(def const-proto
  {
   :diff     (fn [this var] ((constructor placeholder-cons const-proto) 0))
   :evaluate (fn [this m] (this :val))
   :toString (fn [this] (format "%.1f" (this :val)))
   })

(def var-proto
  {
   :diff     (fn [this var] ((constructor placeholder-cons const-proto) (if (= var (this :val)) 1 0)))
   :evaluate (fn [this m] (m (this :val)))
   :toString (fn [this] (this :val))
   })

(defn Constant [val] (placeholder-cons const-proto val))
(defn Variable [val] (placeholder-cons var-proto val))

(defn add-sub-diff [f args]
  (fn [_ var]
    (apply f (mapv (fn [a] (diff a var)) args))))

(defn quotient-diff [_ args]
  (fn [_ var]
    (let [a (nth args 0) b (nth args 1)]
      (Divide
        (Subtract
          (Multiply b (diff a var))
          (Multiply a (diff b var)))
        (Multiply b b)))))

(defn mult-diff [_ args]
  (fn [_ var]
    (let [a (nth args 0) b (nth args 1)]
      (Add
        (Multiply (diff a var) b)
        (Multiply (diff b var) a)))))

(defn negate-diff [_ args]
  (fn [_ var]
    (let [arg (first args)]
      (Negate (diff arg var)))))

(defn op-op [arg]
  (cond (= "add" arg) Add
        (= "subtract" arg) Subtract
        (= "multiply" arg) Multiply
        (= "divide" arg) Divide
        (= "negate" arg) Negate
        (= "exp" arg) Exp
        (= "ln") Ln))

(defn op-cons [this f diff type args]
  {
   :prototype this
   :f         f
   :args      args
   :diff      (diff (op-op type) args)
   :type      type
   })

(defn op-str [op]
  (cond (= op "add") "+"
        (= op "subtract") "-"
        (= op "divide") "/"
        (= op "multiply") "*"
        (= op "negate") "negate"
        (= op "exp") "exp"
        (= op "ln") "ln"))

(defn safe-division [a b]
  (if (or (= b 0) (= b 0.0))
    (cond (> a 0) Double/POSITIVE_INFINITY
          (< a 0) Double/NEGATIVE_INFINITY
          (or (= a 0) (= a 0.0)) Double/NaN)
    (/ a b)))

(def op-proto
  {
   :evaluate (fn [this m]
               (apply (this :f) (mapv (fn [a] (evaluate a m)) (this :args))))
   :toString (fn [this] (str "(" (op-str (this :type)) " " (string/join " " (mapv toString (this :args))) ")"))
   })

(def divide-proto
  {
   :prototype op-proto
   :evaluate  (fn [this m] (apply safe-division (mapv (fn [a] (evaluate a m)) (this :args))))
   })

(def ln-proto
  {
   :prototype op-proto
   :evaluate  (fn [this m] (Math/log (Math/abs (evaluate (nth (this :args) 0) m))))
   })

(defn op-op [arg]
  (cond (= arg "add") Add
        (= arg "subtract") Subtract
        (= arg "multiply") Multiply
        (= arg "divide") Divide
        (= arg "negate") Negate
        (= arg "exp") Exp
        (= arg "ln") Ln
        :else nil))

(defn exp-diff [_ args]
  (fn [_ var]
    (Multiply (diff  (nth args 0) var) (Exp (nth args 0)))))

(defn log-diff [_ args]
  (fn [_ var]
    (Multiply (diff (nth args 0) var) (Divide (Constant 1) (nth args 0)))))

; it breaks evaluate
(defn Add [& args] (op-cons op-proto + add-sub-diff "add" args))
(defn Subtract [& args] (op-cons op-proto - add-sub-diff "subtract" args))
(defn Divide [& args] (op-cons divide-proto / quotient-diff "divide" args))
(defn Multiply [& args] (op-cons op-proto * mult-diff "multiply" args))
(defn Negate [arg] (op-cons op-proto - negate-diff "negate" [arg]))
(defn Exp [arg] (op-cons op-proto (fn [a] (Math/exp a)) exp-diff "exp" [arg]))
(defn Ln [arg] (op-cons ln-proto (fn [a] (Math/log a)) log-diff "ln" [arg]))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(defn op-map [arg]
  (cond (= '+ arg) Add
        (= '- arg) Subtract
        (= '* arg) Multiply
        (= '/ arg) Divide
        (= 'negate arg) Negate
        (= 'exp arg) Exp
        (= 'ln arg) Ln
        :else nil))

(defn parse [l]
  (cond (number? l) (Constant (double l))
        (symbol? l) (Variable (name l))
        :else (apply (op-map (first l)) (mapv parse (rest l)))))

(defn parseObject [str] (parse (read-string str)))