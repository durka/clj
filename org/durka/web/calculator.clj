(ns org.durka.web.calculator
  (:use [compojure jetty html])
  (:use [compojure.http servlet routes helpers]))

(defn parse-simple-infix
  [expr]
  (if-let [[_ lhs op rhs] (re-find #"([^ ]+) *([-\+\*\/]) *([^ ]+)" expr)]
    (eval (read-string (str "(" op " " lhs " " rhs ")")))
    (throw (Exception. "Invalid expression!"))))

(defservlet calc
            (GET "/"
                 (html [:h1 (params :expr)]
                       [:h2 (str (parse-simple-infix (params :expr)))]))
            (ANY "*"
                 (page-not-found)))

(defserver calculator
           {:port 8080}
           "/*" calc)

(start calculator)
