(comment
  "This library contains two different things: a (rather feeble) attempt to find all the unresolvable classes in a Clojure source file. This is incomplete, but it revolves around find-first-unresolvable and fix-unresolvable. The second, and probably more useful, part is a way to build an index of all Java classes on the classpath, and look them up by unqualified name (so you can find out what to import). You have to create the index, which is pretty time- and CPU-consuming. The main function is make-index, which returns the database. For convenience agent-make-index is provided, which dispatches an agent to run make-index, and beeps when the agent finishes (at which point @agt is the database).
  => (def agt (agent-make-index))
  => (.getQueueCount agt)
  1 ;; eventually this will be zero
  => (agent-errors agt)
  ;; if all goes well, this will remain nil
  ;; *time goes by*
  ;; *BEEP*
  => (def db @agt) ;; don't evaluate @agt directly at the repl -- it is enormous! 
  => (db "Ref") ;; lookup by unqualified name
  => (lookup-re db #"File.*Exception$") ;; lookup by regex of unqualified name
  CPU usage will spike while the agent is working, and will fall off a cliff again when it beeps. If the CPU usage falls off a cliff and there is no beep, an exception probably occurred. getQueueCount will still return 1, but agent-errors will give something ugly. A known issue so far (there are undoubtedly many unknown issues) is that broken symlinks on the classpath will cause exceptions (note: recursive symlinks are not a problem).")

(ns org.durka.smuggler
  (:use clojure.contrib.seq-utils)
  (import [java.io File FileInputStream DataInputStream]
          [java.util.jar JarFile JarEntry JarFile$JarFileEntry]
          [javassist.bytecode ClassFile]
          [java.awt Toolkit]))

(defn replace-in
  "Recursive replace"
  [smap coll]
  (if (vector? coll)
    (reduce (fn [v i]
              (if (coll? (v i))
                (assoc v i (replace-in smap (v i)))
                (if-let [e (find smap (nth v i))]
                  (assoc v i (val e))
                  v)))
            coll (range (count coll)))
    (map #(if (coll? %)
            (replace-in smap %)
            (if-let [e (find smap %)]
              (val e)
              %))
         coll)))

(defn muzzle
  "Swallows exceptions and returns a default value instead."
  [default f & args]
  (try (apply f args)
    (catch Throwable
      default)))

(defn get-referenced-classes ;;OLD
  "Find all the classes that a piece of code references. Pass code as a list of top-level forms, not a string."
  [ast]
  (filter #(and (symbol? %)
                (not (= \. (first (.toString %))))
                (= \. (last  (.toString %))))
          (flatten ast)))

(defn get-unknown-symbols ;;OLD
  [ast]
  (filter #(and (symbol? %) (not (special-form-anchor %))
                (not (muzzle nil resolve %)))
          (flatten ast)))

(defn analyze
  [code]
  (clojure.lang.Compiler/analyze clojure.lang.Compiler$C/STATEMENT
                                 code))

(comment(defmacro def-ns
  [nmspc sym val]
  (let [v (eval val)] ;HACK -- how do you really do this (eval one of the args) in a macro?
    `(binding [*ns* ~nmspc]
       (eval '(def ~sym ~v))))))

(defn def-ns
  [nmspc sym val]
  (binding [*ns* nmspc]
    (eval `(def ~sym ~val))))

(defmacro setup-ns
  [name]
  `(binding [*ns* *ns*]
     (in-ns (ns-name (create-ns ~name)))
     (refer 'clojure.core)
     *ns*))

(defmulti fix-unresolvable (fn [t v n a] t)) ;given a namespace and an abstract syntax tree, fix an unresolvable symbol/classname. The meaning of the code may be changed, but subsequent calls to find-first-unresolvable on the namespace/AST pair should not pick up the symbol/classname.
(defmethod fix-unresolvable "symbol"
  [type name nmspc ast]
  (def-ns nmspc name (str (gensym)))
  [nmspc ast]) ;just define it to something meaningless
(defmethod fix-unresolvable "classname"
  [type name nmspc ast]
  (def-ns nmspc name Object) ;this takes care of . .. and new forms
  [nmspc
   (replace-in {name 'do, (symbol (str name ".")) 'do} ast)]) ;this takes care of Classname. and (new Classname) forms

(defn find-first-unresolvable
  "Analyze an abstract syntax tree in a given namespace or a new one, returning the name of the first unresolvable symbol/classname."
  ([ast] (find-first-unresolvable ast (setup-ns (gensym))))
  ([ast new-ns]
   (binding [*ns* new-ns]
     (try
       (analyze ast)
       (catch clojure.lang.Compiler$CompilerException ce
         (or
           (rest
             (re-find #"Unable to resolve (symbol|classname): ([\w\$-]+)"
                      (.getMessage (.getCause ce))))
           ce))))))

(comment (defn get-unresolvable-symbols
  [ast]
  (let [ns (create-ns (gensym))
        analyze #(clojure.lang.Compiler/analyze clojure.lang.Compiler$C/STATEMENT %)]
    (loop [errors #{}
           to-def nil]
      (try
        (binding [*ns* *ns*]
          (in-ns (ns-name ns))
          (if to-def (eval `(def ~to-def (gensym))))
          (analyze ast))
        (catch Exception e
          (let [sym (symbol
                      (second (re-find #"Unable to resolve symbol: (\w+) in this context"
                                       (.getMessage (.getCause e)))))]
            (recur (conj errors sym) sym)))
        (finally
          (in-ns 'smuggler)))))))

(defn filter-classes
  "In a flat seq of strings which correspond to absolute pathnames of .class files, filter out matches for a particular (unqualified) class. Return matches with the root path and extension stripped off, and path separators converted to dots (in other words it will look like a fully qualified class name)."
  [name seq root]
  (let [class (str "/" name ".class")
        prefix (if root (+ 1 (count root)) 0)]  
    (map (comp #(.substring % 0 (- (count %) 6))
               #(.replace % \/ \.)) ;replace / with . and remove the .class filetype suffix
         (filter #(.endsWith % class)
                 (map #(.substring % prefix)
                      seq)))))

(defmulti make-class-file class)
(defmethod make-class-file File
  [f]
  (ClassFile.
    (DataInputStream.
      (FileInputStream. f))))
(defmethod make-class-file JarEntry ;the JarEntry must have as a comment the name of the enclosing jar
  [je]
  (ClassFile.
    (DataInputStream.
      (.getInputStream
        (JarFile.
          (.getComment je)) je))))

(defn filetype?
  [name ext]
  (and (.contains name ".")
       (= ext (last (.split name "\\.")))))

(defn jar?
  [file]
  (filetype? (.getCanonicalPath file) "jar"))

(def spec-version (System/getProperty "java.specification.version"))

(defmulti classfile? class)
(defmethod classfile? File
  [f]
  (and
    (filetype? (.getName f) "class")))
(defmethod classfile? JarEntry
  [je]
  (and
    (filetype? (.getName je) "class")
    (try ; get the specification version from the jar file manifest and compare to the current spec-version. If there is no manifest or no Specification-Version attribute, an NPE will be thrown and the check will be skipped (assumed successful)
      (.. (JarFile. (.getComment je))
        getManifest
        getMainAttributes
        (getValue "Specification-Version")
        (equals spec-version))
      (catch NullPointerException npe
        spec-version))))

(defn path-tree
  "Scan a directory, recursing into both subdirectories (but avoiding symlink loops) and jar archives, returning a list of files. Real files are given as Files with absolute paths, and files inside archives are given as JarEntries, with the containing JarFile as comment."
  [dir]
  (let [hist (atom #{})]
    (tree-seq
      (fn branch?
        [f]
        (if (and (not (or
                        (= JarEntry (class f))
                        (= JarFile$JarFileEntry (class f))))
                 (not (contains? @hist (.getCanonicalPath f)))
                 (or (.isDirectory f)
                     (jar? f)))
          (do
            (swap! hist conj (.getCanonicalPath f))
            true)
          false))
      (fn children
        [f]
        (if (jar? f)
          (let [fname (.getCanonicalPath f)]
            (map #(do (.setComment % fname) %)
                 (enumeration-seq (.entries (JarFile. (.getAbsolutePath f))))))
          (.listFiles f)))
      dir)))

(defn class-name
  [f]
  (.getName (make-class-file f)))

(defn ignore
  "Given a function of no arguments, returns a function that takes any number of arguments, ignore them and delegates to the original function."
  [f]
  (fn
    [& args]
    (f)))

(defn make-index
  "Scans the classpath for .class files (including inside jars) and builds up an index. Returns a map with unqualified classnames as keys and vectors of qualified classnames as values."
  []
  (group-by #(last (.split % "\\."))
            (distinct
              (map class-name
                   (filter classfile?
                           (flatten (map (comp path-tree #(File. %))
                                         (apply concat (map #(.split (System/getProperty (str "java." %))
                                                                     (System/getProperty "path.separator"))
                                                            '["class.path" "ext.dirs" "endorsed.dirs"])))))))))

(defn agent-make-index
  "Has an agent do make-index asynchronously. Beeps when done. The index will be in the state of the returned agent after the beep. Time taken to scan is printed."
  []
  (send-off (add-watcher (agent nil)
                         :send (agent nil) (ignore #(.. Toolkit getDefaultToolkit beep))) ;construct an agent with a watcher that just beeps
            (ignore #(time (make-index)))))

(defn lookup-re
  "Look up a classname by regular expression."
  [db re-string-or-pattern]
  (let [re (re-pattern re-string-or-pattern)]
    (vals (filter #(re-find (re-matcher re (key %)))
                  db))))
