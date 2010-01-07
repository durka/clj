(ns raytracer
  (:use [clojure.contrib java-utils])
  (:import [javax.imageio ImageIO]
     [java.awt.image BufferedImage]))

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
              :dir (struct-map vect :x (- x (/ xres 2)) :y (- y (/ yres 2)) :z 255)))

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

(defn vect-dot
  [v1 v2]
  (+ (* (:x v1) (:x v2))
     (* (:y v1) (:y v2))
     (* (:z v1) (:z v2))))

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

(defn closest-intersection
  [r objs]
  (let [inters (filter second (map #(vector % (intersection r %)) objs))]
    (:color (first (reduce (fn ([] [{:color 0}]) ([i j] (min-key second i j))) inters)))))

(defn raytrace
  [filename xres yres objects]
  (write-image [rt :w xres :h yres :out filename]
               (dorun (map (fn [[xx yy]] (setp xx yy (closest-intersection (ray-norm (make-ray-for-pixel xres yres xx yy))
                                                                           objects)))
                           (for [x (range xres)
                                 y (range yres)]
                             (vector x y))))
               (write)))


(def objs [{:type :sphere, :color 0x00FF00, :center (struct vect 0 0 50), :radius 7}
           {:type :sphere, :color 0xFF0000, :center (struct vect 5 5 40), :radius 10}])

;(raytrace "boom.png" 100 200 objs)

