(ns raytracer
  (:use [clojure.contrib java-utils])
  (:import [javax.imageio ImageIO]
     [java.awt.image BufferedImage]))

(def *recursion-limit* 2)

(defmacro write-image
  "Binds an Image to the specified symbol. If the file does not exist, it is created. If the filename is not given or nil, the image is not initially attached to any file. Defines utility functions: (getp x y), (setp x y Color), (write filename)."
  ([[sym & args] & body]
   (let [params (apply hash-map args)] ; params: in, out, w, h, format. must give :out/format to call write. must have :in xor :w/h
     `(let [~sym ~(if (:in params)
                    `(ImageIO/read (file ~(:in params)))
                    `(BufferedImage. ~(:w params) ~(:h params) (BufferedImage/TYPE_INT_RGB)))]
        (letfn [(~'getp [x# y#]
                      (.getRGB ~sym x# y#))
                (~'setp [x# y# col#]
                      (.setRGB ~sym x# y# col#))
                (~'write []
                       (ImageIO/write ~sym
                                      (or ~(:format params)
                                          (second (re-find #"\.(\w+)$" ~(:out params))))
                                      (file ~(:out params))))]
          ~@body)))))

(defstruct vect :x :y :z)
(defstruct ray :origin :dir)

(defn make-ray-for-pixel
  "Generates a ray going through a certain pixel"
  [xres yres x y]
  (struct-map ray
              :origin (struct vect 0 0 0)
              :dir (struct-map vect :x (- x (/ xres 2)) :y (- y (/ yres 2)) :z 2000)))

(defn sqr [x] (* x x))

(defn vect-norm
  [v]
  (let [len (Math/sqrt (+ (sqr (:x v))
                          (sqr (:y v))
                          (sqr (:z v))))]
    (assoc v :x (/ (:x v) len)
             :y (/ (:y v) len)
             :z (/ (:z v) len))))

(defn ray-norm
  [r]
  (assoc r :dir (vect-norm (:dir r))))

(defn vect-op
  [v1 v2 op]
  (struct vect (op (:x v1) (:x v2))
               (op (:y v1) (:y v2))
               (op (:z v1) (:z v2))))

(def vect-add #(vect-op %1 %2 +))
(def vect-sub #(vect-op %1 %2 -))

(defn vect-scale
  [v sc]
  (assoc v :x (* sc (:x v))
           :y (* sc (:y v))
           :z (* sc (:z v))))

(defn vect-dot
  [v1 v2]
  (+ (* (:x v1) (:x v2))
     (* (:y v1) (:y v2))
     (* (:z v1) (:z v2))))

(defn vect-dist
  [v1 v2]
  (let [d (vect-sub v1 v2)]
    (Math/sqrt (vect-dot d d))))

(defmulti intersection #(:type %2))

(defmethod intersection :sphere
  [r sph]
  (let [oc (vect-sub (:center sph) (:origin r))
        tca (vect-dot oc (:dir r))]
    (if (> tca 0)
      (let [thc2 (- (sqr (:radius sph))
                    (- (vect-dot oc oc) (sqr tca)))]
        (if (> thc2 0)
          (- tca (Math/sqrt thc2)))))))

#_(defmethod intersection :sphere
  [r sph]
  (let [dist (vect-sub (:origin r) (:center sph))
        b (vect-dot dist (:dir r))
        c (- (vect-dot dist dist) (sqr (:radius sph)))
        d (- (sqr b) c)]
    (if (pos? d)
      (- (- b) (Math/sqrt d)))))

(defmethod intersection :plane
  [r pl]
  (let [cos (vect-dot (:dir (:normal pl)) (:dir r))]
    (if (pos? cos)
      (/ (vect-dot (:dir r)
                   (vect-sub (:origin (:normal pl))
                             (:origin r)))
         cos))))

(defmulti normal (fn [s _] (:type s)))

(defmethod normal :sphere
  [sph place]
  (vect-scale (vect-sub place (:center sph)) (/ (:radius sph))))

(defmethod normal :plane
  [pl place]
  (vect-scale (:dir (:normal pl)) -1))

(defn closest-intersection
  [r objs]
  (let [inters (filter second (map #(vector % (intersection r %)) objs))]
    (reduce (fn ([] nil) ([i j] (min-key second i j))) inters)))

(defn lambertian
  [ra light thing inter objs]
  (let [place-of-int (vect-add (:origin ra) (vect-scale (:dir ra) inter))
        int-to-light (vect-norm (vect-sub light place-of-int))
        [shadow-thing shadow-inter] (closest-intersection (struct ray place-of-int int-to-light) objs)
        norm (normal thing place-of-int)]
    (if (and shadow-inter (< 0 shadow-inter (vect-dist light place-of-int)))
      (* 0.5 (vect-dot norm int-to-light))
      (vect-dot norm int-to-light))))

(defn twobyte
  [x]
  (min (int x) 255))

(defn pack-rgb
  [r g b]
  (+ (twobyte (* 255 b))
     (bit-shift-left (twobyte (* 255 g)) 8)
     (bit-shift-left (twobyte (* 255 r)) 16)))

(defn raytrace
  [filename xres yres objects lights]
  (write-image [rt :w xres :h yres :out filename]
               (dorun (map (fn [[xx yy]] (setp xx yy (let [ra (ray-norm (make-ray-for-pixel xres yres xx yy))
                                                           [thing inter] (closest-intersection ra objects)]
                                                       (if thing
                                                         (let [r (:r (:color thing))
                                                               g (:g (:color thing))
                                                               b (:b (:color thing))
                                                               rs (map (comp :r :color) lights)
                                                               gs (map (comp :g :color) lights)
                                                               bs (map (comp :b :color) lights)
                                                               coeffs (map #(max 0
                                                                                 (lambertian ra (:pos %) thing inter objects))
                                                                           lights)]
                                                           (pack-rgb (apply + (map #(* r %1 %2) coeffs rs))
                                                                     (apply + (map #(* g %1 %2) coeffs gs))
                                                                     (apply + (map #(* b %1 %2) coeffs bs))))
                                                         0))))
                           (for [x (range xres)
                                 y (range yres)]
                             (vector x y))))
               (write)))


(def objs [{:type :sphere, :color {:r 0, :g 1, :b 0}, :center (struct vect -5 -5 1200), :radius 20}
           {:type :sphere, :color {:r 1, :g 0, :b 0}, :center (struct vect 70 80 1600), :radius 30}
           {:type :sphere, :color {:r 0, :g 0, :b 1}, :center (struct vect -20 -20 800), :radius 20}
           {:type :plane,  :color {:r 0, :g 2, :b 2}, :normal (struct ray (struct vect 0 200 1000)
                                                                          (struct vect 0 1 0))}
           {:type :plane,  :color {:r 2, :g 2, :b 0}, :normal (struct ray (struct vect -200 0 1000)
                                                                          (struct vect -1 0 0))}
           {:type :plane,  :color {:r 2, :g 2, :b 0}, :normal (struct ray (struct vect 200 0 1000)
                                                                          (struct vect 1 0 0))}
           {:type :plane,  :color {:r 0, :g 2, :b 2}, :normal (struct ray (struct vect 0 -200 1000)
                                                                          (struct vect 0 -1 0))}
           {:type :plane,  :color {:r 0.25, :g 0, :b 0.25}, :normal (struct ray (struct vect 0 0 15000)
                                                                          (struct vect 0 0 1))}
           ])

(def lamps [{:color {:r 1, :g 1, :b 1}, :pos (struct vect -25 -25 -30)}
            ;{:color {:r 0.2, :g 0, :b 0}, :pos (struct vect 6000 -4000 -250)}
            ])

;(raytrace "boom.png" 100 200 objs lamps)

