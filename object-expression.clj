(require '[clojure.string :as string])

(declare Add)
(declare Multiply)
(declare Subtract)
(declare Divide)
(declare Negate)
(declare Exp)
(declare Ln)
(declare Constant)

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
(def toStringSuffix (method :toStringSuffix))

(defn placeholder-cons [this val]
  {
   :prototype this
   :val       val
   })

;(declare const-proto)
;(def zero (placeholder-cons const-proto 0))
;(def one (placeholder-cons const-proto 1))

(def const-proto
  {
   :diff           (fn [this var] (Constant 0))
   :evaluate       (fn [this m] (this :val))
   :strRep         (fn [this] (format "%.1f" (this :val)))
   :toString       (fn [this] ((method :strRep) this))
   :toStringSuffix (fn [this] ((method :strRep) this))
   })

(def var-proto
  {
   :diff           (fn [this var] (if (= var (this :val)) (Constant 1) (Constant 0)))
   :evaluate       (fn [this m] (m (string/lower-case (first (this :val)))))
   :strRep         (fn [this] (this :val))
   :toString       (fn [this] ((method :strRep) this))
   :toStringSuffix (fn [this] ((method :strRep) this))
   })

(defn Constant [val] (placeholder-cons const-proto val))
(defn Variable [val] (placeholder-cons var-proto val))

(defn common-diff [args diff-args diff-f]
  (fn [_ var]
    (let [a (nth args 0)
          b (nth args 1)
          dif (diff-args var)
          ad (force (nth dif 0))
          bd (force (nth dif 1))]
      (diff-f a b ad bd))))


(defn add-diff [_ _ ad bd]
  (Add ad bd))

(defn sub-diff [_ _ ad bd]
  (Subtract ad bd))

(defn quotient-diff [a b ad bd]
  (Divide
    (Subtract
      (Multiply b ad)
      (Multiply a bd))
    (Multiply b b)))

(defn mult-diff [a b ad bd]
  (Add
    (Multiply ad b)
    (Multiply bd a)))

(defn negate-diff [ad]
  (fn [_ var]
    (Negate (force (nth (ad var) 0)))))

(defn op-map [arg]
  (let [arg-str (str arg)]
    (cond (= "+" arg-str) Add
          (= "-" arg-str) Subtract
          (= "*" arg-str) Multiply
          (= "/" arg-str) Divide
          (= "negate" arg-str) Negate
          (= "exp" arg-str) Exp
          (= "ln" arg-str) Ln
          :else nil)))

(defn op-cons [this f diff type args]
  {
   :prototype this
   :f         f
   :args      args
   :diff      diff
   :type      type
   })

(defn safe-division [a b]
  (if (or (= b 0) (= b 0.0))
    (cond (> a 0) Double/POSITIVE_INFINITY
          (< a 0) Double/NEGATIVE_INFINITY
          (or (= a 0) (= a 0.0)) Double/NaN)
    (/ a b)))

(def op-proto
  {
   :evaluate       (fn [this m]
                     (apply (this :f) (mapv (fn [a] (evaluate a m)) (this :args))))
   :toString       (fn [this] (str "(" (this :type) " " (string/join " " (mapv toString (this :args))) ")"))
   :toStringSuffix (fn [this] (str "(" (string/join " " (mapv toStringSuffix (this :args))) " " (this :type) ")"))
   })

(defn exp-diff [a ad]
  (fn [_ var]
    (Multiply (force (nth (ad var) 0)) (Exp a))))

(defn log-diff [a ad]
  (fn [_ var]
    (Multiply (force (nth (ad var) 0)) (Divide (Constant 1) a))))

(defn make-diff-map [args]
  (letfn [(get-diff [var] (mapv (fn [arg] (delay (diff arg var))) args))]
    {
     "x" (get-diff "x")
     "y" (get-diff "y")
     "z" (get-diff "z")
     }))

(defn diff-fnc [args f] (common-diff args (make-diff-map args) f))

(def operation-bones (partial op-cons op-proto))

(defn Add [& args] (operation-bones + (diff-fnc args add-diff) "+" args))
(defn Subtract [& args] (operation-bones - (diff-fnc args sub-diff) "-" args))
(defn Divide [& args] (operation-bones safe-division (diff-fnc args quotient-diff) "/" args))
(defn Multiply [& args] (operation-bones * (diff-fnc args mult-diff) "*" args))
(defn Negate [& arg] (operation-bones - (negate-diff (make-diff-map arg)) "negate" arg))
(defn Exp [& arg] (operation-bones #(Math/exp %) (exp-diff (nth arg 0) (make-diff-map arg)) "exp" arg))
(defn Ln [& arg] (operation-bones (fn [u] (Math/log (Math/abs u))) (log-diff (nth arg 0) (make-diff-map arg)) "ln" arg))

;parser

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
                       "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))

(defn _empty [value] (partial -return value))

(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

(defn _map [f result]
  (if (-valid? result)
    (-return (f (-value result)) (-tail result))))

(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        (_map (partial f (-value ar))
              ((force b) (-tail ar)))))))

(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn +char [chars] (_char (set chars)))

(defn +char-not [chars] (_char (comp not (set chars))))

(defn +map [f parser] (comp (partial _map f) parser))

(def +parser _parser)

(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce _either p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

; grammar

(def *digit (+char "0123456789.-"))
(def *number (+map read-string (+str (+plus *digit))))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(declare expression)
(def *letter (+char "xyzXYZ"))
(def *variable (+plus *letter))
(def bracket (+or (+ignore (+char "(")) (+ignore (+char ")"))))
(def operand (+or *number *variable (delay expression)))
(def operation (+or (+str (+seq (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e")))
                    (+char "-+*/")))
(def binary (+seq *ws bracket *ws operand *ws operand *ws operation *ws bracket))
(def unary (+seq *ws bracket *ws operand *ws operation *ws bracket))
(def bare (+seqn 0 *ws (+or *number *variable) *ws))
(def expression (+or unary binary bare))

(defn parse [suffix? l]
  (cond (number? l) (Constant (double l))
        ((if suffix? (fn [u] (= (type u) clojure.lang.Cons)) symbol?) l) (if suffix? (Variable (string/join "" (mapv (fn [u] u) l))) (Variable (str l)))
        :else (let [operation (if suffix? (last l) (first l))
                    operands (if suffix? (drop-last l) (rest l))]
                (apply (op-map operation) (mapv (partial parse suffix?) operands)))))

(defn parseObjectSuffix [str] (parse true ((expression str) :value)))
(defn parseObject [str] (parse false (read-string str)))
