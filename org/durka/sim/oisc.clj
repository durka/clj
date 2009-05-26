(ns org.durka.sim.oisc
  (:use clojure.contrib.str-utils))

(defn subleq
  "Evalutes a subleq instruction, given the current state of the memory, the memory locations marked as special -in- and -out-, the registers (currently just an instruction pointer :ip), and (optionally) the arguments to subleq. Returns the new state of the memory and the registers."
  ([in out mem regs] ; in this case data and program memory are the same
   (apply subleq in out mem regs (subvec mem (:ip regs) (+ 3 (:ip regs)))))
  ([in out mem regs a b]
   (subleq in out mem regs a b
           (+ 3 (:ip regs)))) ; a SUBLEQ takes up three memory cells
  ([in out mem regs a b c]
   (let [a-ind (if (neg? a) (nth mem (- a)) a)
         b-ind (if (neg? b) (nth mem (- b)) b)
         c-ind (if (neg? c) (nth mem (- c)) c)

         mem (assoc mem b-ind
                    (- (mem b-ind) (mem a-ind)))]
     [mem (assoc regs
                 :ip (if (pos? (mem b-ind))
                       (+ 3 (:ip regs))
                       c-ind)
                 :in (= b-ind in)
                 :out (= b-ind out))])))

(defn drive-subleq
  "Runs an OISC SUBLEQ program. Given the program (as a list of integers) and the input (in the same way), it runs the program and returns the output."
  [program input]
  (let [-out- (count program)
        -in- (inc -out-)
        input-len (count input)] ;TODO: should we just use 0 and 1 for the special registers?
    (loop [mem (vec (concat program [0 0] (vec (range 0 1023)))) ; memory starts out as the program, then the special output cell, then the special input cell, followed by 1024 memory cells filled with the numbers from 0 to 1023
           regs {:ip 0, :in true}
           output ""]
      (let [[mem regs] (subleq -in- -out- mem regs)]
        (if (neg? (mem -in-))
          [output (- -1 (mem -in-))] ; if wrote a negative value to -in-, exit with one less than its absolute value (so write -1 to exit with code 0)
          (let [output (str output
                            (if (:out regs)
                              (char (mem -out-))))
                mem (assoc mem -in- (if (:in regs)
                                      (if (or (>= (mem -in-) input-len)
                                              (neg? (mem -in-)))
                                        -1
                                        (nth input (mem -in-))) ;TODO: does this need to be casted?
                                      (mem -in-))
                               -out- (if (:out regs) 0 (mem -out-)))
                regs (dissoc regs :in :out)]
          (recur mem regs output)))))))

(defn first-pass
  "First pass of assembler. Takes a seq of lines and, expands macros, counts out labels. Returns a list of integers/labels/expressions and a map of labels to positions."
  [lines]
  (loop [lines (remove empty? (map #(.trim %) lines))
         code ()
         toc {}
         macros {}
         i 0]
    (if (seq lines)
      (condp = (ffirst lines)
        \= #_"equality" (recur (next lines)
                               code
                               (let [remainder (.substring (first lines) 2)
                                     position (.indexOf remainder (int \space))]
                                 (assoc toc
                                        (read-string (.substring remainder 0 position))
                                        (read-string (.substring remainder (inc position)))))
                               macros
                               i)
        \. #_"literal" (let [fixed (re-gsub #"'([^']*)'"
                                            (fn [[m $1]]
                                              (apply str (interpose " "
                                                                    (map #(str \\ %) $1))))
                                            (first lines))
                             stuff (read-string (str \( (.substring fixed 2) \)))]
                         (recur (next lines)
                                (concat code stuff)
                                toc
                                macros
                                (+ i (count stuff))))
        \+ #_"defmacro" (let [definition (take-while #(not= \- (first %)) lines)
                              prog (next (drop-while #(not= \- (first %)) lines))
                              [macro & args] (read-string (str \( definition \)))]
                          (recur prog
                                 code
                                 toc
                                 (assoc macros
                                        macro {:args args, :code (next definition)})
                                 i))
        \> #_"call macro" (let [tokens (read-string (str \( (.substring (first lines) 1) \)))
                                macro (macros (first tokens))
                                args (next tokens)
                                expansion (replace (zipmap (:args macro) args) (:code macro))]
                            (recur (next lines)
                                   (concat code expansion)
                                   toc
                                   macros
                                   (+ i (count expansion))))
        \: #_"label" (recur (next lines)
                            code
                            (assoc toc (read-string (.substring (first lines) 1)) i)
                            macros
                            i)
        \# #_"comment" (recur (next lines)
                              code
                              toc
                              macros
                              i)
        #_"instruction" (recur (next lines)
                               (concat code
                                       (let [args (vec (read-string (str \( (first lines) \))))] ;has to be a vector for conj
                                         (condp = (count args)
                                           3 args
                                           2 (conj args '(inc ?))
                                           1 (vector (first args) (first args) '(inc ?)))))
                               toc
                               macros
                               (+ i 3)))
      [code toc])))

(defn second-pass
  "Second pass of assembler. Takes a seq of things and fills in all label references (and relative references, and special labels) with numbers. Outputs a list of integers".
  [[code toc]]
  (let [len (count code)
        bindings (apply concat toc)
        more-bindings (apply concat {'OUT len ; TODO: more special labels?
                                     'IN (inc len)
                                     'Z (+ 2 len)})]
    (map (comp int (fn [i thing]
           (eval `(let ~(vec (concat ['? i]
                                     more-bindings
                                     bindings))
                    ~thing))))
         (iterate inc 0) code)))

(defn assemble-subleq
  [tokens]
  (second-pass (first-pass tokens)))

(defn run-subleq
  ([filename] (run-subleq filename ""))
  ([filename input]
   (drive-subleq (assemble-subleq
                   (.split (slurp filename)
                           "\n"))
                 input)))
