(defn make-atom [value]
  {:type :atom :value value})

(defn make-list [children]
  {:type :list :value children})

(defn sexp-to-string [sexp]
  (cond (= (:type sexp) :atom) (:value sexp)
        (= (:type sexp) :list)
        (str "(" (apply str (map sexp-to-string (:value sexp))) ")")
        :else ""))

(defn tokenize [input]
  (-> input
      (clojure.string/replace #"(" " ( ")
      (clojure.string/replace #")" " ) ")
      (clojure.string/split #" ")
      (remove clojure.string/blank?)))

(defn parse-tokens [tokens]
  (if (empty? tokens)
    (throw (Exception. "Unexpected end of input"))
    (let [token (first tokens)
          rest (rest tokens)]
      (cond
        (= token "(")
        (let [[children remaining] (parse-list rest)]
          [(make-list children) remaining])
        (= token ")")
        (throw (Exception. "Unmatched )"))
        :else
        [(make-atom token) rest]))))

(defn parse-list [tokens]
  (cond
    (empty? tokens) (throw (Exception. "Unmatched ("))
    (= (first tokens) ")") [[] (rest tokens)]
    :else
    (let [[sexp remaining] (parse-tokens tokens)
          [sexps final-remaining] (parse-list remaining)]
      [(cons sexp sexps) final-remaining])))

(defn parse [input]
  (let [[sexp remaining] (parse-tokens (tokenize input))]
    (if (empty? remaining)
      sexp
      (throw (Exception. "Invalid S-expression")))))

(defn -main [& args]
  (let [input "(+ 1 (* 2 3))"]
    (println (sexp-to-string (parse input)))))
