(ns playground.util
  (:import [java.util.concurrent BlockingQueue LinkedBlockingQueue TimeUnit]
           [java.lang.ref WeakReference]))

(set! *warn-on-reflection* true)

(defmacro ignore-errors
  "Returns the result of evaluating body, or nil if it throws an exception."
  ([& body] `(try ~@body (catch java.lang.Exception _# nil))))

(defn ffilter
  "Returns the first item in coll for which (pred item) is true."
  ([pred coll]
     (when-let [coll (seq coll)]
       (let [item (first coll)]
         (if (pred item) item (recur pred (rest coll)))))))

;; Stolen from clojure/core.clj
(defmacro assert-args [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in "
                      ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro doto-let
  "bindings => binding-form expr

Evaluates expr, and evaluates body with its result bound to the binding-form.
Returns the result of expr."
  ([bindings & body]
     (assert-args
       (vector? bindings) "a vector for its binding"
       (= 2 (count bindings)) "exactly 2 forms in binding vector")
     (let [[bf expr] bindings]
       `(let [value# ~expr]
          (let [~bf value#]
            ~@body value#)))))

(defmacro prog1
  "Evaluates the result of expr, then evaluates the forms in body (presumably
for side-effects), then returns the result of expr."
  ([expr & body]
     `(let [value# ~expr]
        ~@body value#)))

(defmacro ->>+
  "binding => [name]

Threads the expr through the forms.  Inserts x in the first form at the place
specified by the binding-name, or the last position if the name is not
mentioned, and making a list of the form first if it isn't a list already.
Inserts the resulting form recursively into any subsequent forms."
  ([binding x form]
     (assert-args
       (vector? binding) "a vector for its binding"
       (= 1 (count binding)) "exactly 1 form in binding vector")
     (let [[name] binding]
       (if (seq? form)
         `~(loop [[head & others :as form] form, result []]
             (cond (empty? form) (seq (conj result x))
                   (= name head) (concat (conj result x) others)
                   :else         (recur others (conj result head))))
         (list form x))))
  ([binding x form & more]
     `(->>+ ~binding (->>+ ~binding ~x ~form) ~@more)))

;;; Stolen with tweaks from old clojure.contrib.seq
(defn fill-queue
  "filler-func will be called in another thread with a single arg
  'fill'.  filler-func may call fill repeatedly with one arg each
  time which will be pushed onto a queue, blocking if needed until
  this is possible.  fill-queue will return a lazy seq of the values
  filler-func has pushed onto the queue, blocking if needed until each
  next element becomes available.  filler-func's return value is ignored."
  ([filler-func & optseq]
    (let [opts (apply array-map optseq)
          apoll (:alive-poll opts 1)
          q (LinkedBlockingQueue. ^Integer (:queue-size opts 100))
          NIL (Object.) ;nil sentinel since LBQ doesn't support nils
          weak-target (Object.)
          alive? (WeakReference. weak-target)
          fill (fn fill [x]
                 (if (.get alive?)
                   (if (.offer q (if (nil? x) NIL x) apoll TimeUnit/SECONDS)
                     x
                     (recur x))
                   (throw (Exception. "abandoned"))))
          f (future
              (try
                (filler-func fill)
                (finally
                  (.put q q))) ;q itself is eos sentinel
              nil)] ; set future's value to nil
      ((fn drain []
         weak-target ; force closing over this object
         (lazy-seq
           (let [x (.take q)]
             (if (identical? x q)
               @f  ;will be nil, touch just to propagate errors
               (cons (if (identical? x NIL) nil x)
                     (drain))))))))))

(defn update
  "Like update-in, but only for a single key"
  ([map key f] (assoc map key (f (get map key))))
  ([map key f x] (assoc map key (f (get map key) x)))
  ([map key f x y] (assoc map key (f (get map key) x y)))
  ([map key f x y & args] (assoc map key (apply f (get map key) x y args))))

(defn assoc-conj
  "Return a new collection conj-ing val into the collection found at key,
placing def there if no value was present already."
  ([map key def val] (assoc map key (conj (get map key def) val))))

(defn assoc-in-conj
  "Like assoc-conj for assoc-in."
  ([map keys def val] (assoc-in map keys (conj (get-in map keys def) val))))

(defn map-group
  "Like group-by, except f should return a [key val] pair, and val will be
grouped by key."
  [f coll]
  (persistent!
   (reduce (fn [ret [k v]]
             (assoc! ret k (conj (get ret k []) v)))
           (transient {})
           (map f coll))))

(defmacro case-expr
  "Like case, but only supports individual test expressions, which are
evaluated at macro-expansion time."
  [e & clauses]
  `(case ~e
     ~@(concat
        (mapcat (fn [[test result]]
                  [(if (or (list? test) (symbol? test))
                     (eval test)
                     test)
                   result])
                (partition 2 clauses))
        (when (odd? (count clauses))
          (list (last clauses))))))

(defmacro defmulti-group [& forms]
  "Define a group of related multimethods."
  `(do ~@(map #(cons 'defmulti %) forms)))

(defmacro defmethod-group [& specs]
  "Implement a group of related multimethods sharing common dispatch values."
  (letfn [(parse-impls [specs]
            (lazy-seq
             (when (seq specs)
               (let [dval (first specs),
                     [fns specs] (split-with list? (rest specs))]
                 (cons (cons dval fns) (parse-impls specs))))))]
    `(do ~@(mapcat (fn [[dval & fns]]
                     (map #(list* 'defmethod (first %) dval (rest %)) fns))
                   (parse-impls specs)))))

(defn zip
  "Zip together two or more sequences into sequences of tuples."
  [& colls] (apply map vector colls))

;;; Taken with modification from /Joy of Clojure/ pg. 283
(defn seq1 [s]
  "De-chunk potentially-chunked seq s, realizing only one element at a time."
  (lazy-seq
   (when (seq s)
     (cons (first s) (seq1 (rest s))))))

(defmacro duration [& body]
  `(let [start# (System/currentTimeMillis)]
     (do ~@body)
     (- (System/currentTimeMillis) start#)))

(defmacro ^:private protect-queue [q & body]
  `(try ~@body (catch Exception e# (.put ~q ~q) (throw e#))))

(defn pmap-ooo
  "Like clojure.core/pmap, but potentially improves parallelism by allowing
results to return out-of-order."
  ([f coll]
     (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
           ^BlockingQueue outq (LinkedBlockingQueue. (* 2 n))
           monitor (agent [(seq coll) {}])]
       (letfn [(start [marker x]
                 (future
                   (try (f x) (finally (send-off monitor complete marker)))))
               (activate [[s active] marker]
                 [(rest s) (assoc active marker (start marker (first s)))])
               (complete [[s active :as state] marker]
                 (protect-queue outq
                   (.put outq (get active marker))
                   (if (seq s)
                     (activate state marker)
                     (let [active (dissoc active marker)]
                       (when (empty? active) (.put outq outq))
                       [nil active]))))
               (initiate [[s _ :as state]]
                 (protect-queue outq
                   (let [n (->> (take n s) (count) (min n))]
                     (reduce activate state (range n)))))
               (drain []
                 (lazy-seq
                  (let [x (.take outq)]
                    (if (identical? x outq)
                      (do @monitor nil)
                      (cons @x (drain))))))]
         (send monitor initiate)
         (drain))))
  ([f coll & colls]
     (pmap-ooo #(apply f %) (apply map vector coll colls))))
