(ns datalog.core
  (:require [datalog.fj :as fj]))

(defn logic-name? [o]
  (and (symbol? o)
       (.startsWith (name o) "?")))

(defprotocol MatchP
  (-match [a b]))

(extend-protocol MatchP
  Object
  (-match [a b]
    (when (= b a)
      {a b}))
  java.lang.Class
  (-match [a b]
    (when (isa? b a)
      {a b}))
  clojure.lang.Keyword
  (-match [a b]
    (when (isa? b a)
      {a b}))
  clojure.lang.Symbol
  (-match [a b]
    (if (logic-name? a)
      {a b}
      (when (= a b)
        {a b}))))

;; (mf/defmulti --match (fn [a b] (type a)))

;; (mf/defmethod --match :default [a b]
;;   (when (= b a)
;;     {a b}))

;; (mf/defmethod --match java.lang.Class [a b]
;;   (when (isa? b a)
;;     {a b}))

;; (mf/defmethod --match clojure.lang.Keyword [a b]
;;   (when (isa? b a)
;;     {a b}))

;; (mf/defmethod --match clojure.lang.Symbol [a b]
;;   (if (logic-name? a)
;;       {a b}
;;       (when (= a b)
;;         {a b})))

(defn match [a b]
  (-match a b))

(defn resolve-in [clause environment]
  ((if (seq? clause) identity vec)
   (map (fn [v] (get environment v v)) clause)))

(defn get-by-attribute [db attribute]
  (if attribute
    (filter #(= attribute (second %)) db)
    db))

(declare search)

(defn free-vars [[rule-head & rule-body]]
  (let [bound-vars (set (rest rule-head))]
    (persistent! (reduce (fn [m v]
                           (if (and (logic-name? v)
                                    (not (contains? bound-vars v)))
                             (assoc! m v (gensym v))
                             m))
                         (transient {})
                         (tree-seq coll? seq rule-body)))))

(defn search-with-rules
  [rule-name args db rules vars facts clauses environment]
  (for [rule rules
        :let [[rule-head & rule-body] rule
              free-rule-vars (free-vars rule)
              rule-env (merge (zipmap (rest rule-head) args)
                              free-rule-vars)
              new-clauses (concat (map #(resolve-in % rule-env)
                                       rule-body)
                                  (rest clauses))]
        result (search db
                       rules
                       vars
                       (if (seq? (first new-clauses))
                         (get-by-attribute db nil)
                         (get-by-attribute db (second (first new-clauses))))
                       new-clauses
                       environment)]
    result))

(defn rule [db rules vars facts clauses environment]
  (lazy-seq
    (let [[rule-name & args] (first clauses)]
      (concat
       (search-with-rules
         rule-name args db rules vars facts clauses environment)
       (search db rules vars nil nil environment)))))

(defn rule-or-predicate [db rules vars facts clauses environment]
  (lazy-seq
    (if-let [rules (seq (filter #(= (first (first %))
                                    (first (first clauses))) rules))]
      (rule db rules vars facts clauses environment)
      (if-let [p (resolve (first (first clauses)))]
        (when (apply p (map environment (rest (first clauses))))
          (search db
                  rules
                  vars
                  (if (seq? (first (rest clauses)))
                    (get-by-attribute db nil)
                    (get-by-attribute db (second (first (rest clauses)))))
                  (rest clauses)
                  environment))
        (throw (Exception. "failed rule"))))))

(defn out-of-facts-or-clauses
  [db rules vars facts clauses environment]
  (when (every? #(contains? environment %) vars)
    (lazy-seq
      [(reduce
        (fn [m [k v]]
          (assoc m (keyword (subs (name k) 1)) v))
        {}
        (select-keys environment vars))])))

(defn search-next-clause [db rules vars facts clauses environment]
  (let [clause (resolve-in (first clauses) environment)
        fact (first facts)
        m (map match clause fact)
        pass? (and (seq m)
                   (every? map? m)
                   (= (count (set (map keys m)))
                      (count (set (map vals m)))))]
    (when pass?
      (let [new-env (apply merge m)]
        (search db
                rules
                vars
                (get-by-attribute db (second (second clauses)))
                (rest clauses)
                (into environment new-env))))))

(defn search-rest-of-facts [db rules vars facts clauses environment]
  (search db
          rules
          vars
          (rest facts)
          clauses
          environment))

(defn search [db rules vars facts clauses environment]
  (lazy-seq
    (if (seq? (first clauses))
      (rule-or-predicate db rules vars facts clauses environment)
      (when (seq facts)
        (if (or (not (seq facts))
                (not (seq clauses)))
          (out-of-facts-or-clauses db rules vars facts clauses environment)
          (concat
           (search-next-clause db rules vars facts clauses environment)
           (search-rest-of-facts db rules vars facts clauses environment)))))))

(defn psearch [db rules vars facts clauses environment]
  (let [n 4]
    (if (> (count facts) 1000)
      (->> facts
           (partition-all (long (/ (count facts) n)))
           ((fn [pfacts]
              (concat
               (search db rules vars (first pfacts) clauses environment)
               (doall
                (map (fn [facts]
                       (fj/fj
                        (doall
                         (search db rules vars facts clauses environment))))
                     (rest pfacts))))))
           (mapcat fj/join))
      (search db rules vars facts clauses environment))))

(defn q [vars query rules db]
  (let [vars (set vars)
        given-names (set (filter symbol? (tree-seq coll? seq query)))]
    (if (not (every? (partial contains? given-names) vars))
      (throw (Exception. "unknown variable"))
      (psearch
       db
       rules
       vars
       (if (seq? (first query))
         (get-by-attribute db nil)
         (get-by-attribute db (second (first query))))
       query
       {}))))
