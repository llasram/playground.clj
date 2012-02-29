(ns playground.weka
  (:use [playground.util :only [doto-let defmulti-group defmethod-group]])
  (:import [java.util List ArrayList]
           [weka.core Instance Instances DenseInstance SparseInstance
                      Attribute]))

(defn attribute-type [^Attribute attr]
  (cond (.isNumeric attr)        :numeric
        (.isNominal attr)        :nominal
        (.isString attr)         :string
        (.isDate attr)           :date
        (.isRelationValued attr) :relational))

(defn data-type [data]
  (let [x (if (seq? data) (first data) data)]
    (cond (number? x)  :numeric
          (keyword? x) :nominal
          (symbol? x)  :nominal
          (string? x)  :string)))

(defn attribute-values [^Attribute attr]
  (-> attr .enumerateValues enumeration-seq))

(defmulti-group
  (create-attribute*
    "Create new Weka Attribute of indicated type."
    (fn [type aname column] type))
  (attribute-intern
    "Convert val to interned numeric representation for attribute attr."
    (fn [attr val] (attribute-type attr))))

(defmethod-group :numeric
  (create-attribute* [type ^String aname column] (Attribute. aname))
  (attribute-intern [attr val] (double val)))

(defn- nominalize [x]
  (if (keyword? x) (name x) (str x)))

(defmethod-group :nominal
  (create-attribute* [type ^String aname column]
    (let [^List values (distinct (map nominalize column))]
      (Attribute. aname values)))
  (attribute-intern [attr val]
    (doto-let [id (.indexOfValue attr (nominalize val))]
      (when (neg? id)
        (throw (RuntimeException. "invalid nominal value"))))))

(defmethod-group :string
  (create-attribute* [type ^String aname column]
    (let [^List values nil]
      (doto-let [attr (Attribute. aname values)]
        (doseq [value column]
          (.addStringValue attr value)))))
  (attribute-intern [attr val]
    (let [val (str val), id (.indexOfValue attr val)]
      (if-not (neg? id) id (.addStringValue attr val)))))

(defn create-attribute
  ([type] (create-attribute* type "attribute" []))
  ([type aname] (create-attribute* type aname []))
  ([type aname column] (create-attribute* type aname column)))

(defn derive-attribute
  ([column] (derive-attribute "attribute" column))
  ([aname column]
     (create-attribute (data-type column) aname column)))

(defn derive-attributes [data]
  (let [indices (-> data first count range),
        anames (map #(str "attribute-" (inc %)) indices)
        columns (for [idx indices] (map #(nth % idx) data))]
    (map derive-attribute anames columns)))

(defn build-dataset
  ([data] (build-dataset (derive-attributes data) data))
  ([attrs data]
     (let [alist (java.util.ArrayList. attrs),
           size (if (vector? data) (count data) 0)]
       (doto-let [dataset (Instances. "dataset" alist size)]
         (doseq [row data]
           (let [row (double-array (map attribute-intern attrs row)),
                 inst (DenseInstance. 1.0 row)]
             (.setDataset inst dataset)
             (.add dataset inst)))))))
