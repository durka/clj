(ns org.durka.sim.oisc
  (:use clojure.contrib.str-utils)
  (:import [clojure.lang Keyword]))

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
    (loop [mem (vec (concat program
                            [0 (if (zero? input-len) -1 (nth input 0))]
                            (vec (range 0 1023)))) ; memory starts out as the program, then the special output cell, then the special input cell, followed by 1024 memory cells filled with the numbers from 0 to 1023
           regs {:ip 0}
           output ""]
      (let [[mem regs] (subleq -in- -out- mem regs)]
        (if (and (:in regs)
                 (neg? (mem -in-)))
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

(defn tokenize
  "Zeroth pass of assembler. Takes a seq of lines and converts it into a lazy vector of vectors of symbols/lists/keywords. Strips out comments."
  [lines]
  (map #(read-string (str \[ % \]))
       (remove empty? (map (comp #(let [i (.indexOf % "#")] ;strip comments
                                    (if (not= i -1)
                                      (.substring % 0 i)
                                      %))
                                 #(.trim %))
                           lines))))

(defn replace-rec
  "Like clojure.core/replace, but recurses into sub-colls."
  [smap coll] ; this function is mostly copied from c.core/replace, just with recursion added
  (if (vector? coll)
    (reduce (fn [v i] ; to preserve vectors, because map will turn it into a list
              (let [e (nth v i)]
                (if (coll? e) ; have to recurse
                  (assoc v i (replace-rec smap e))
                  (if-let [r (find smap e)] ; look for a replacement
                    (assoc v i (val r))
                    v))))
            coll (range (count coll)))
    (map #(if (coll? %) ; have to recurse
            (replace-rec smap %)
            (if-let [r (find smap %)] ; look for a replacement
              (val r)
              %))
         coll)))

(defn first-pass
  "First pass of assembler. Takes a seq of vectors of tokens and, expanding macros, counts out labels. Returns a list of integers/labels/expressions and a map of labels to positions."
  [lines]
  (loop [lines lines
         code ()
         toc {}
         macros {}
         i 0]
    (if (seq lines)
      (condp #((first %1) (second %1) %2) (ffirst lines)
        [= '=] #_"equality" (recur (next lines)
                                   code
                                   (assoc toc
                                          (second (first lines))
                                          (last (first lines)))
                                   macros
                                   i)
        [= '.] #_"literal" (let [stuff (mapcat #(if (string? %)
                                                  %
                                                  (list %))
                                               (next (first lines)))]
                             (recur (next lines)
                                    (concat code stuff)
                                    toc
                                    macros
                                    (+ i (count stuff))))
        [= 'Macro] #_"defmacro" (let [definition (take-while #(not= 'End (first %)) lines)
                                      prog (next (drop-while #(not= 'End (first %)) lines))
                                      [_ macro & args] (first definition)]
                                  (recur prog
                                         code
                                         toc
                                         (assoc macros
                                                macro {:args args, :code (next definition)})
                                         i))
        [find macros] #_"call macro" (let [tokens (first lines)
                                           macro (macros (first tokens))
                                           args (next tokens)
                                           expansion (replace-rec (zipmap (:args macro) args) (:code macro))]
                                       (recur (concat expansion (next lines))
                                              code
                                              toc
                                              macros
                                              i))
        [instance? Keyword] #_"label" (recur (next lines)
                                             code
                                             (assoc toc (.sym (ffirst lines)) i)
                                             macros
                                             i)
        #_"instruction" (recur (next lines)
                               (concat code
                                       (let [args (first lines)] ;has to be a vector for conj
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
                   (tokenize
                     (.split (slurp filename)
                             "\n")))
                 input)))
