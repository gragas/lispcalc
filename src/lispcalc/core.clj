(ns lispcalc.core
  (:gen-class))

(declare evaluate)
(declare str->exp)
(declare sexp-body->exp-list)

(defn evaluate
  [exp]
  (case (first exp)
    "EOFExp" ((println "Goodbye!") (System/exit 0))
    "IntExp" (second exp)
    "SExp" (case (first (first (second exp)))
             "IntOpExp" (reduce (second (first (second exp))) (map evaluate (rest (second exp))))
             (str "Invalid SExp " (second exp)))
    (str "Invalid Exp " exp)))

(defn str->exp
  [s]
  (case s
    nil ["EOFExp"]
    "+" ["IntOpExp", +]
    "-" ["IntOpExp", -]
    "*" ["IntOpExp", *]
    "/" ["IntOpExp", /]
    (if (number? (read-string s))
      ["IntExp", (read-string s)]
      (if (re-matches #"^\(.*\)$" s)
        ["SExp", (sexp-body->exp-list (drop-last 1 (rest s)))]
        ["InvalidExp", (str "Invalid expression: " s)]))))

(defn sexp-body->exp-list
  [s]
  ((fn
     [s cur count tokens]
     (if (> count 0)
       (if (= (first s) \))
         (if (= count 1)
           (recur (rest s) "" 0 (conj tokens (str->exp (str cur \)))))
           (recur (rest s) (str cur (first s)) (- count 1) tokens))
         (if (= (first s) \()
           (recur (rest s) (str cur \() (+ count 1) tokens)
           (recur (rest s) (str cur (first s)) count tokens)))
       (case (first s)
         nil    (if (empty? cur) tokens (conj tokens (str->exp cur)))
         \space (if (empty? cur)
                  (recur (rest s) cur count tokens)
                  (recur (rest s) "" count (conj tokens (str->exp cur))))
         \(     (recur (rest s) (str cur \() (+ count 1) tokens)
         (recur (rest s) (str cur (first s)) count tokens)))) s "" 0 []))

(defn interpret
  [s]
  (evaluate (str->exp s)))

(defn repl
  []
  (print "lispcalc> ")
  (flush)
  (println (interpret (read-line)))
  (repl))

(defn greet [] (println "Welcome to lispcalc!"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet)
  (repl))
