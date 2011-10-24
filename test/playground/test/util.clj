(ns playground.test.util
  (:use [playground.util])
  (:use [clojure.test]))

(deftest test-ignore-errors
  (is (= :good (ignore-errors :good))
      "Returns result of body on success")
  (is (= nil (ignore-errors (throw (RuntimeException.)) :bad))
      "Returns nil on exceptions"))

(deftest test-ffilter
  (is (= 2 (ffilter even? [1 3 5 7 2 4 6 8]))
      "Returns first value matching predicate when value matches")
  (is (= nil (ffilter even? [1 3 5 7 9]))
      "Returns nil when no value matches"))

(deftest test-doto-let
  (is (= :good (doto-let [_ :good] :bad))
      "Returns result of binding-form expression")
  (is (= :good @(doto-let [x (atom :bad)] (reset! x :good)))
      "Evaluates body expressions for side-effects"))

(deftest test-->>+
  (is (= :good (->>+ [it] (vector) (reduce conj it [:good]) first))
      "Threads forms at specified position, or last if unspecified"))

(deftest test-update
  (is (= {:key 1} (update {:key 0} :key inc))
      "Creates new map with f applied to the value at key")
  (is (= {:key 2} (update {:key 0} :key + 2))
      "Creates new map with f applied to the value at key plus arguments"))

(deftest test-assoc-conj
  (is (= {:key #{:value}} (assoc-conj {} :key #{} :value))))

(deftest test-map-group
  (is (= {true [2 4], false [3 5]}
         (map-group #(vector (odd? %) (inc %)) [1 2 3 4]))))

(deftest test-zip
  (is (= [[1 2] [3 4] [5 6]] (zip [1 3 5] [2 4 6]))))

(deftest test-pmap-ooo
  (testing "Basic functionality"
    (let [input (range 100)]
      (is (= input (sort (pmap-ooo identity input)))
          "All inputs map to outputs for single input collection"))
    (let [input1 (range 100), input2 (range 99 -1 -1)]
      (is (= (zip input1 input2) (sort (pmap-ooo vector input1 input2)))
          "All inputs map to outputs for multiple input collections")))
  (testing "Performance"
    (let [input (doall (seq1 (repeatedly 100 #(inc (rand 9)))))]
      (is (> (duration (dorun (pmap #(Thread/sleep %) input)))
             (duration (dorun (pmap-ooo #(Thread/sleep %) input))))
          "Out-performs pmap for function with 10x execution-time range"))))

(comment

(defn demo-pmap [& fs]
  (let [times (seq1 (doall (map (fn [_] (inc (rand 100))) (range 100))))]
    (doseq [[f n] (zip fs (range))]
      (println (str "function #" n ":"))
      (time (dotimes [_ 5] (dorun (f #(do (Thread/sleep %)) times)))))))

;; playground.util> (demo-pmap pmap pmap-ooo)
;; function #0:
;; "Elapsed time: 54056.55043 msecs"
;; function #1:
;; "Elapsed time: 42391.166418 msecs"

)
