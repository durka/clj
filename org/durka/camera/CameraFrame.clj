(ns org.durka.camera.frame
  (:use clojure.contrib.stacktrace))

(import '[javax.swing JFrame JLabel ImageIcon])
(import '[java.awt Toolkit Color Component Graphics])
(import '[java.awt.image BufferedImage DirectColorModel Raster DataBufferInt FilteredImageSource RGBImageFilter])
(import '[quicktime QTSession])
(QTSession/open) ; have to do this before importing QDGraphics
(import '[quicktime.std.sg SequenceGrabber SGVideoChannel SGDataProc])
(import '[quicktime.std.image QTImage CodecComponent DSequence Matrix])
(import '[quicktime.qd QDGraphics QDRect])
(import '[quicktime.std StdQTConstants])
(import '[quicktime.util RawEncodedImage])

(def *width* (ref 0))
(def *height* (ref 0))

(defn init
  [width height]
  (dosync
    (ref-set *width* width)
    (ref-set *height* height)))

(defn initialize-viewer
  []
  (try
    (let [image-size (QDRect. @*width* @*height*)
          world (QDGraphics. image-size)
          pixel-data (.. world getPixMap getPixelData)
          pixel-data-array-size (* (.getRowBytes pixel-data) 1/4 @*height*)
          pixel-data-array (make-array Integer/TYPE pixel-data-array-size)
          sg (doto (SequenceGrabber.)
               (.setGWorld world nil))
          vc (doto (SGVideoChannel. sg)
               (.setBounds image-size)
               (.setUsage StdQTConstants/seqGrabRecord)
               (.setFrameRate 0)
               (.setCompressorType StdQTConstants/kComponentVideoCodecType))
          img (doto (BufferedImage. (DirectColorModel. 32 0x00ff0000 0x0000ff00 0x000000ff)
                                    (let [size (/ (.getRowBytes pixel-data) 4)]
                                      (Raster/createPackedRaster (DataBufferInt.
                                                                   pixel-data-array
                                                                   (.getSize pixel-data))
                                                                 @*width* @*height*
                                                                 (/ (.getRowBytes pixel-data) 4)
                                                                 (into-array Integer/TYPE
                                                                             [0x00ff0000
                                                                              0x0000ff00
                                                                              0x000000ff])
                                                                 nil))
                                      false
                                    nil))
          comp (proxy [Component] []
                 (paint [g]
                        (proxy-super paint g)
                        (let [mx (atom 0)
                              my (atom 0)
                              mr (atom 0)]
                          (doto g
                            (.drawImage (.. Toolkit
                                          getDefaultToolkit
                                          (createImage
                                            (FilteredImageSource.
                                              (.getSource img)
                                              (proxy [RGBImageFilter] []
                                                (filterRGB [x y rgb]
                                                           (let [r (bit-and rgb 0xffff0000)]
                                                             (when (> r @mr)
                                                               (swap! mx (fn [_] (do x)))
                                                               (swap! my (fn [_] (do y)))
                                                               (swap! mr (fn [_] (do r))))
                                                             rgb))))))
                                        0 0 this)
                            (.setColor Color/RED)
                            (.drawLine @mx 0 @mx @*height*)
                            (.drawLine 0 @my @*width* @my)))))
          frame (doto (JFrame.)
                  (.setSize @*width* @*height*)
                  (.add comp))
          data-proc (let [raw-data (make-array Byte/TYPE (QTImage/getMaxCompressionSize
                                                           world
                                                           (.getBounds world)
                                                           0
                                                           StdQTConstants/codecLowQuality
                                                           StdQTConstants/kComponentVideoCodecType
                                                           CodecComponent/anyCodec))
                          ds (ref nil)
                          frame-count (atom 0)]
                      (proxy [SGDataProc] []
                          (execute [chan data offset ctrl-info start-time type]
                                   (prn (swap! frame-count inc) (System/currentTimeMillis))
                                   (if (instance? SGVideoChannel chan)
                                     (try
                                       (let [desc (.getImageDescription vc)
                                             raw-data (or raw-data (make-array Byte/TYPE
                                                                               (.getSize data)))
                                             raw-image (RawEncodedImage. raw-data)]
                                         (.copyToArray data 0 raw-data 0 (.getSize data))
                                         (if @ds
                                           (.decompressFrameS @ds
                                                              raw-image
                                                              StdQTConstants/codecNormalQuality)
                                           (dosync (ref-set ds (DSequence. desc
                                                                           raw-image
                                                                           world
                                                                           image-size
                                                                           (Matrix.)
                                                                           nil
                                                                           0
                                                                           StdQTConstants/codecNormalQuality
                                                                           CodecComponent/anyCodec))))
                                         (.. world getPixMap getPixelData (copyToArray
                                                                            0
                                                                            pixel-data-array
                                                                            0
                                                                            pixel-data-array-size))
                                         (.repaint comp)
                                         0)
                                       (catch Exception _ 1))
                                     1))))]
      (doto sg
        (.setDataProc data-proc)
        (.setDataOutput nil StdQTConstants/seqGrabDontMakeMovie)
        (.prepare false true)
        (.startRecord))
      (.start (Thread. (proxy [Runnable] []
                         (run []
                              (try
                                (loop []
                                  (doto sg
                                    (.idleMore)
                                    (.update nil))
                                  (recur))
                                (catch Exception e
                                  (prn "Exception in infinite loop!")
                                  (print-stack-trace e)))))))
      frame)
    (catch Throwable tr
      (try
        (QTSession/close)
        (catch Throwable tr-again
          (prn "Exception while trying to handle exception!")
          (print-stack-trace tr-again)))
      (prn "Exception!")
      (print-stack-trace tr)
      (JFrame.))))

(init 320 240)
(let [frame (initialize-viewer)]
  (.setVisible frame true))
