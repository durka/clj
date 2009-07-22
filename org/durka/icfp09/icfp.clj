(ns org.durka.icfp09.icfp
  (:use clojure.contrib.shell-out
        clojure.contrib.str-utils
        clojure.contrib.seq-utils
        clojure.contrib.duck-streams
     penumbra.opengl penumbra.matrix penumbra.window)
  (:import [java.io RandomAccessFile FileOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]))

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

(defn find-biggest-transfer
  "Finds the most extravagant combination of Hohmann transfers (current to detour, back to current, back to detour, finally to target) possible given the current orbit radius, the target radius, and the available fuel. Starts with a seed value for a detour radius to which to transfer first, and performs a sort of binary search to use the most fuel possible."
  [current target detour fuel increment epsilon]
  (let [trans1 (hohmann current detour)
        trans2 (hohmann detour target)
        need (apply + (map #(Math/abs %)
                           [(first trans1) (second trans1)
                            (first trans1) (second trans1)
                            (first trans1) (second trans1)
                            (first trans2) (second trans2)]))]
    (prn trans1 trans2 detour increment need)
    (if (> fuel need)
      (if (> epsilon (- fuel need))
        [trans1 trans2]
        (find-biggest-transfer current target (+ detour increment) fuel increment epsilon))
      (find-biggest-transfer current target (- detour increment) fuel (* 0.9 increment) epsilon))))

(defn bielliptic
  "Calculates the instantaneous velocity changes necessary to perform a bi-elliptic transfer from a circular orbit at r1 to one at r2, using rb as the intermediary apogee. Returns a vector with three elements: the first velocity change, the second, the third, and the times in between."
  [r1 r2 rb]
  (let [a1 (/ (+ r1 rb)
              2)
        a2 (/ (+ rb r2)
              2)]
    [(- (Math/sqrt (- (* 2 +G+ +Me+ (/ r1))
                      (* +G+ +Me+ (/ a1))))
        (Math/sqrt (* +G+ +Me+ (/ r1))))
     (- (Math/sqrt (- (* 2 +G+ +Me+ (/ rb))
                      (* +G+ +Me+ (/ a2))))
        (Math/sqrt (- (* 2 +G+ +Me+ (/ rb))
                      (* +G+ +Me+ (/ a1)))))
     (- (Math/sqrt (* +G+ +Me+ (/ r2)))
        (Math/sqrt (- (* 2 +G+ +Me+ (/ r2))
                      (* +G+ +Me+ (/ a2)))))
     (* Math/PI (Math/sqrt (/ (Math/pow a1 3)
                              (* +G+ +Me+))))
     (* Math/PI (Math/sqrt (/ (Math/pow a2 3)
                              (* +G+ +Me+))))]))

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

(defn unpack-submission
  [osf]
  (with-open [file (.getChannel (RandomAccessFile. (str *dir* "/" osf) "r"))]
    (let [buf (.order
                (.map file FileChannel$MapMode/READ_ONLY 0 (.size file))
                ByteOrder/LITTLE_ENDIAN)]
      (.getInt buf) ; 0xCAFEBABE
      {:team (.getInt buf)
       :challenge (.getInt buf)
       :frames (loop [acc (sorted-map)]
                 (let [t (.getInt buf)
                       k (.getInt buf)]
                   (if (zero? k)
                     (assoc acc t {})
                     (recur (assoc acc t (apply hash-map
                                                (apply concat
                                                       (take k (repeatedly
                                                                 (fn [] [(.getInt buf)
                                                                         (.getDouble buf)]))))))))))})))

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
                      (assoc mem pc (in (nth-in code 0 1) (double 0)))
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

(defn textual-log
  [hist]
  (printf "Team #%d competing in challenge #%d\n" (:team hist) (:challenge hist))
  (doseq [[t inputs] (:frames hist)]
    (printf "\tClock reads %d seconds\n" t)
    (if (seq inputs)
      (doseq [[addr value] inputs]
        (printf "\t\tInput port 0x%X = %g\n" addr value))
      (println "\t\tSimulation finished"))))

(def orbit (atom nil))
(def rot-x (ref -45))
(def rot-y (ref 30))

(defn draw-earth
  [r]
  (material 1 0 0 1)
  (draw-line-loop
    (doseq [phi   (map #(* 1/180 Math/PI %) (range 180))
            theta (map #(* 10 1/180 Math/PI %) (range 36))]
      (material (Math/abs (- (/ phi (* 0.5 Math/PI)) 1)) (Math/abs (- (/ theta Math/PI) 1)) 1 1)
      (vertex (* r (Math/sin phi) (Math/sin theta))
              (* r (Math/cos phi) (Math/sin theta))
              (* r (Math/cos theta))))))

(defn draw-message
  []
  (write "Hokay, so: here's the Earth!" 0 1))

(defn mouse-drag [[dx dy] _]
  (dosync
    (commute rot-x + dy)
    (commute rot-y + dx)))

(defn gl-init
  []
  (enable :auto-normal)
  (enable :normalize)
  (enable :depth-test)
  (shade-model :smooth)
  (set-display-list orbit (draw-earth 0.3)))

(defn replay
  [bin hist]
  (print (format "Team #%d competing in challenge #%d\n" (:team hist) (:challenge hist)))
  (let [[data code] (unpack-executable bin)
        frames (:frames hist)]
    (loop [t 0
           mem data
           status false
           out {}
           in {}]
      (print (format "Clock reads %d seconds\n" t))
      (let [ins (frames t in)] ; the frame's inputs, unless it doesn't have any, in which case the previous frame's inputs
        (if (and (not-empty ins) (not= ins in))
          (doseq [[addr value] ins]
            (print (format "\t\t\tInput port 0x%X = %g\n" addr value))))
        (let [[mem status outs] (simulate mem status code ins)]
          (if (not= out outs)
            (doseq [[addr value] (into (sorted-map) outs)]
              (print (format "\t\t\tOutput port 0x%X = %g\n" addr (double value)))))
          (if (not-empty ins)
            (recur (inc t)
                   mem
                   status
                   outs
                   ins) ; preserve these inputs in case they remain in effect for the next frame
            (println "\t\tSimulation finished")))))))

(defn csv-replay
  [bin hist out cols]
  "Cols is a map of port numbers to output spreadsheet columns. Positive numbers are interpreted as referring to input ports, while negative numbers are for output. Time is automatically prepended as the first column."
  (with-open [csv (writer (str *dir* "/" out))]
    (.println csv (str-join "," (concat ["Time"] (vals cols))))
    (let [[data code] (unpack-executable bin)
          frames (:frames hist)]
      (loop [t 0
             mem data
             status false
             out {}
             in {}]
        (let [ins (frames t in)
              [mem status outs] (simulate mem status code ins)]
          (.println csv (str-join "," (concat [t] (for [port (keys cols)]
                                                    (if (pos? port)
                                                      (ins port "")
                                                      (outs (- port) ""))))))
          (if (not-empty ins)
            (recur (inc t)
                   mem
                   status
                   outs
                   ins)))))))

(defn gl-replay
  [bin hist]
  (start {:init gl-init
          :display (fn [d t]
                     (translate 0 0 -2)
                     (rotate @rot-x 1 0 0)
                     (rotate @rot-y 0 1 0)
                     (set-light-position 0 [1 1 1 0])
                     (call-display-list @orbit))
          :reshape (fn [x y w h]
                     (frustum-view 50 (/ (double w)
                                         h)
                                   0.1
                                   100)
                     (load-identity))
          :mouse-drag mouse-drag}))

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


(defn hohmann-bielliptic
  [scenario factor outputs timestep state]
  (println (str "Timestep #" timestep ", radius: " (radius (outputs 2 0) (outputs 3 0)) ", outputs: " outputs))
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
              transfer (bielliptic (radius (outputs 2) (outputs 3)) (outputs 4) (* factor (outputs 4)))]
          [{2 (* -1 (transfer 0) (Math/cos dir))
            3 (* -1 (transfer 0) (Math/sin dir))
            0x3E80 scenario}
           {:transfer transfer}])
      (if-let [t (state :transfer)] (Math/round (t 3))) [{2 0
                                                          3 0
                                                          0x3E80 scenario}
                                                         {:x (outputs 2) :y (outputs 3)}]
      (if-let [t (state :transfer)] (Math/round (inc (t 3)))) (let [dir (Math/atan2 (- (outputs 3) (state :y))
                                                                                    (- (outputs 2) (state :x)))]
                                                                [{2 (* -1 ((state :transfer) 1) (Math/cos dir))
                                                                  3 (* -1 ((state :transfer) 1) (Math/sin dir))
                                                                  0x3E80 scenario}
                                                                 nil])
      (if-let [t (state :transfer)] (Math/round (+ (t 3) (t 4)))) [{2 0
                                                                    3 0
                                                                    0x3E80 scenario}
                                                                   {:x (outputs 2) :y (outputs 3)}]
      (if-let [t (state :transfer)] (Math/round (+ (t 3) (t 4) 1))) (let [dir (Math/atan2 (- (outputs 3) (state :y))
                                                                                          (- (outputs 2) (state :x)))]
                                                                      [{2 (* -1 ((state :transfer) 2) (Math/cos dir))
                                                                        3 (* -1 ((state :transfer) 2) (Math/sin dir))
                                                                        0x3E80 scenario}
                                                                       nil])
      [{2 0
        3 0
        0x3E80 scenario}
       nil])
    nil))


(defn return-second
  [a b]
  b)

(defn return-always
  [a & _]
  a)

(defn do-vector
  [[f arg1] arg2]
  (f arg1 arg2))

(defmacro controller
  [name & cases]
  `(let
     [state# (atom {})
      inputs# (atom {})
      ~'save (fn [k# fun# & args#]
               (swap! state# assoc k# (apply fun# (@state# k#) args#)))
      ~'recall (fn [k#]
                 (@state# k#))
      ~'schedule (fn [t# dV# & pairs#]
                   (~'save :burns assoc (int (Math/round (double t#))) dV#)
                   (if pairs#
                     (recur (+ t# (first pairs#))
                            (second pairs#)
                            (nnext pairs#))))
      ~'do! (fn [port# value#]
              (swap! inputs# assoc port# value#))]
     (defn ~name
       [scenario# ~'outputs timestep# st#]
       (println (str "Timestep #" timestep# ", radius: " (radius (~'outputs 2 0) (~'outputs 3 0)) ", outputs: " ~'outputs ", " (count (~'recall :burns)) " burns left"))
       (swap! state# (partial return-always st#))
       (swap! inputs# empty)
       (when (zero? (~'outputs 0 0))
         (condp do-vector timestep#
           ~@(map (fn [elem i]
                    (if (even? i) ;change the numbers into [= #] pairs
                      [= elem]
                      elem))
                  cases (iterate inc 0))
           [contains? (~'recall :burns)] (let [dir# (Math/atan2 (- (~'outputs 3) (~'recall :y))
                                                                (- (~'outputs 2) (~'recall :x)))]
                                           (~'do! 2 (* -1 ((~'recall :burns) timestep#) (Math/cos dir#)))
                                           (~'do! 3 (* -1 ((~'recall :burns) timestep#) (Math/sin dir#)))
                                           (~'save :burns dissoc timestep#))
           (do
             (~'do! 2 0)
             (~'do! 3 0)))
         (~'save :x return-second (~'outputs 2))
         (~'save :y return-second (~'outputs 3))
         (~'do! 0x3E80 scenario#)
       [@inputs# @state#]))))

(controller hohmann-double
            1 (let [[trans1 trans2] (find-biggest-transfer (radius (outputs 2) (outputs 3)) (outputs 4) (* 2 (outputs 4)) (outputs 1) 1e6 10)]
                (schedule 2               (nth trans1 0)      ; launch from home to detour x1
                          (nth trans1 2)  (nth trans1 1)      ; stop at detour x1
                          100             (- (nth trans1 1))  ; launch back home x1
                          (nth trans1 2)  (- (nth trans1 0))  ; stop at home x1
                          100             (nth trans1 0)      ; launch from home to detour x2
                          (nth trans1 2)  (nth trans1 1)      ; stop at detour x2
                          100             (nth trans2 0)      ; launch from detour to target
                          (nth trans2 2)  (nth trans2 1))     ; stop at target
                (do! 2 0)
                (do! 3 0)))
