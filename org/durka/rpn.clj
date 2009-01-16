(ns org.durka.rpn
  (:import [java.io BufferedReader]))

(defn switch
  "Return a function that will call a given two-argument function with the arguments transposed."
  [f]
  #(f %2 %1))

(defn process
  "Turn an RPN expression into executable Clojure code. WARNING: no safety checking of any kind is done. Anything in the expression that doesn't look like a Number will be executed."
  [rpn]
  (concat
    '(let [stack (atom ())
           push #(swap! stack conj %)
           pull #(let [n (peek @stack)] 
                   (swap! stack pop)
                   n)])
    (map #(if (number? %)
            `(~'push ~%)
            `(~'push ((switch ~%) (~'pull) (~'pull))))
         rpn)))

(defn rpn-repl
  "Like the regular REPL, but for RPN expressions. Kill with EOF (ctrl-D)."
  []
  (doseq [expr (line-seq (BufferedReader. *in*))]
    (prn
      (peek
        (eval
          (process (read-string (str "(" expr ")"))))))))
