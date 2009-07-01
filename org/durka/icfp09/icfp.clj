(ns org.durka.icfp09.icfp
  (:use clojure.contrib.shell-out)
  (:import [java.io FileOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel]))

(def *dir* "/Users/alex/Programming/clojure/clj/org/durka/icfp09")

(def +G+ 6.67428e-11) ;m^3/(kg s^2)
(def +dt+ 1) ;second
(def +Me+ 6.0e24) ;kg
(def +Re+ 6.357e6) ;m
(def +Mm+ 7.347e22) ;kg

(defn gravity
  "Calculates the force of gravity by m1 on m2."
  [m1 m2 r]
  (/ (* +G+ m1 m2) (* r r)))

(defn orbital-velocity
  "Calculates the velocity that a satellite is going in order to orbit the earth in UCM at a given radius."
  [r]
  (Math/sqrt (* +G+ +Me+ (/ r))))

(defn sqr [x] (* x x))

(defn distance
  [sx1 sy1 sx2 sy2]
  (Math/sqrt (+ (sqr (- sx1 sx2))
                (sqr (- sy1 sy2)))))

(defn radius
  [sx sy]
  (distance 0 0 sx sy))

(defn hohmann
  "Calculates the instantaneous velocity changes necessary to perform a Hohmann transfer from a circular orbit at r1 to one at r2. Returns a vector with three elements: the first velocity change, the second, and the time in between."
  [r1 r2]
  [(* (Math/sqrt (* +G+ +Me+ (/ r1)))
      (- (Math/sqrt (/ (* 2 r2)
                       (+ r1 r2)))
         1))
   (* (Math/sqrt (* +G+ +Me+ (/ r2)))
      (- 1
         (Math/sqrt (/ (* 2 r1)
                       (+ r1 r2)))))
   (* Math/PI
      (Math/sqrt (/ (Math/pow (+ r1 r2) 3)
                    (* 8 +G+ +Me+))))])

(defn nth-in
  [m & ks]
  (reduce nth m ks))

(defn concat-nums
  [& bytes]
  (loop [acc 0
         bytes bytes]
    (if (first bytes)
      (recur (+ (bit-shift-left acc 8) (first bytes))
             (next bytes))
      acc)))

(defn unpack-executable
  [filename]
  (let [[data & instrs] (.split 
                          (sh (str *dir* "/decode")
                              (str *dir* "/" filename)
                              :dir *dir*)
                          "\n")]
    [(read-string (str "(" data ")"))
     (map #(read-string (str "(" % ")")) instrs)]))

(defn encode-binary
  [filename scenario inputs]
  (let [inputs (concat inputs '({})) ; append the final frame
        buf (.order (ByteBuffer/allocate (+ 12 (* 52 (count inputs))))
                    ByteOrder/LITTLE_ENDIAN)]
    (.putInt buf (int 0xCAFEBABE))
    (.putInt buf 678)
    (.putInt buf scenario)
    (dorun (map (fn [i ports prev]
                  (when (not= ports prev) ; the final frame will always get written because ports will never equal prev, because all the other ones will have the configuration value in them
                    (.putInt buf i)
                    (let [to-write (remove #(= (ports (key %))
                                               (prev (key %)))
                                           ports)]
                      (.putInt buf (count to-write))
                      (doseq [pair to-write]
                        (.putInt buf (key pair))
                        (.putDouble buf (val pair))))))
                (iterate inc 0) inputs (cons {} inputs)))
    (with-open [file (FileOutputStream. (str *dir* "/" filename))]
      (let [bytes (make-array Byte/TYPE (.position buf))]
        (.position buf 0)
        (.get buf bytes)
        (.write file bytes)))))

(defn simulate
  [data status code in]
  (loop [code code
         mem (vec data)
         pc 0
         status status
         out {}]
    (if (first code)
      (condp = (ffirst code)
        'Add (recur (next code)
                    (assoc mem pc (+ (mem (nth-in code 0 1))
                                     (mem (nth-in code 0 2))))
                    (inc pc)
                    status
                    out)
        'Sub (recur (next code)
                    (assoc mem pc (- (mem (nth-in code 0 1))
                                     (mem (nth-in code 0 2))))
                    (inc pc)
                    status
                    out)
        'Mult (recur (next code)
                     (assoc mem pc (* (mem (nth-in code 0 1))
                                      (mem (nth-in code 0 2))))
                     (inc pc)
                     status
                     out)
        'Div (recur (next code)
                    (assoc mem pc (let [r1 (mem (nth-in code 0 1))
                                        r2 (mem (nth-in code 0 2))]
                                    (if (zero? r2)
                                      0.0
                                      (/ r1 r2))))
                    (inc pc)
                    status
                    out)
        'Output (recur (next code)
                       mem
                       (inc pc)
                       status
                       (assoc out (nth-in code 0 1)
                                  (mem (nth-in code 0 2))))
        'Phi (recur (next code)
                    (assoc mem pc (if status
                                    (mem (nth-in code 0 1))
                                    (mem (nth-in code 0 2))))
                    (inc pc)
                    status
                    out)
        'Noop (recur (next code)
                     mem
                     (inc pc)
                     status
                     out)
        'Cmpz (recur (next code)
                     mem
                     (inc pc)
                     ((resolve (nth-in code 0 1)) (mem (nth-in code 0 2)) 0.0)
                     out)
        'Sqrt (recur (next code)
                     (assoc mem pc (Math/sqrt (mem (nth-in code 0 1))))
                     (inc pc)
                     status
                     out)
        'Copy (recur (next code)
                     (assoc mem pc (mem (nth-in code 0 1)))
                     (inc pc)
                     status
                     out)
        'Input (recur (next code)
                      (assoc mem pc (in (nth-in code 0 1)))
                      (inc pc)
                      status
                      out))
      [mem status out])))

(defn run-simulation
  [scenario infile outfile controller]
  (let [[data code] (unpack-executable infile)
        history (loop [hist ()
                       state [data false {}]
                       ctrl-state {}
                       timestep 0]
                  (if-let [[new-inputs new-ctrl-state] (controller (last state) timestep ctrl-state)]
                    (do
                      (comment(prn (last state) new-inputs new-ctrl-state))
                      (recur (conj hist new-inputs)
                             (simulate (first state) (second state) code new-inputs)
                             (merge ctrl-state new-ctrl-state)
                             (inc timestep)))
                    (reverse hist)))]
    (encode-binary outfile scenario history)))

(defn interactive-controller
  [outputs timestep state]
  (println (str "Timestep #" timestep ", outputs:") outputs)
  (if (do
        (print "continue? ") (flush)
        (read))
    (let [dVx (do
                (print "new delta Vx? ") (flush)
                (read))
          dVy (do
                (print "new delta Vy? ") (flush)
                (read))
          cfg (do
                (print "new scenario number? ") (flush)
                (read))]
      [{2 dVx, 3 dVy, 0x3E80 cfg} nil])
    nil))

(defn observer-controller
  [scenario outputs timestep state]
  (println (str (outputs 2 0)) "," (outputs 3 0))
  [{2 0, 3 0, 0x3E80 scenario} nil])

(defn scripted-hohmann
  [scenario outputs timestep state]
  (println (str "Timestep #" timestep ", radius: " (radius (outputs 2 0) (outputs 3 0)) ", outputs: " outputs))
  (if (zero? (outputs 0 0))
    (condp = timestep
      0 [{2 0
          3 -2466.484644920925
          0x3E80 scenario}
         nil]
      18875 [{2 0
              3 1482.9350796525914
              0x3E80 scenario}
             nil]
      [{2 0
        3 0
        0x3E80 scenario}])))

(defn hohmann-controller
  [scenario outputs timestep state]
  (println (str "Timestep #" timestep ", radius: " (radius (outputs 2 0) (outputs 3 0)) ", outputs: " outputs))
  (comment(println (format "%d,%g,%g" timestep (outputs 2 0.0) (outputs 3 0.0))))
  (if (zero? (outputs 0 0))
    (condp = timestep
      0 [{2 0
          3 0
          0x3E80 scenario}
         nil]
      1 [{2 0
          3 0
          0x3E80 scenario}
         {:x (outputs 2) :y (outputs 3)}]
      2 (let [dir (Math/atan2 (- (outputs 3) (state :y))
                              (- (outputs 2) (state :x)))
              transfer (hohmann (radius (outputs 2) (outputs 3)) (outputs 4))]
          [{2 (* -1 (transfer 0) (Math/cos dir))
            3 (* -1 (transfer 0) (Math/sin dir))
            0x3E80 scenario}
           {:transfer transfer}])
      (if-let [t (state :transfer)] (Math/round (t 2))) [{2 0
                                                          3 0
                                                          0x3E80 scenario}
                                                         {:x (outputs 2) :y (outputs 3)}]
      (if-let [t (state :transfer)] (Math/round (inc (t 2)))) (let [dir (Math/atan2 (- (outputs 3) (state :y))
                                                                                    (- (outputs 2) (state :x)))]
                                                                [{2 (* -1 ((state :transfer) 1) (Math/cos dir))
                                                                  3 (* -1 ((state :transfer) 1) (Math/sin dir))
                                                                  0x3E80 scenario}
                                                                 nil])
      [{2 0
        3 0
        0x3E80 scenario}
       nil])
    nil))

(comment(defn decode-instruction
  [& bytes]
  (if (> 3 (Integer/numberOfLeadingZeros (first bytes))) ; S-type
    (let [instr (nth bytes 0)
          imm (concat-nums (nth bytes 1)
                           (bit-shift-right (nth bytes 2) 6))
          r1 (concat-nums (bit-and (nth bytes 2) (bit-not 0xC0))
                          (nth bytes 3))]
      [instr imm r1])
    ; D-type
    (let [instr (bit-shift-right (nth bytes 0) 4)
          r1 (concat-nums (- (nth bytes 0) instr))]))))
