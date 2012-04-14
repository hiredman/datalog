(ns datalog.core)

(defmulti match (comp type first list))

(defmethod match :default [a b]
  (when (isa? b a)
    {a b}))

(defmethod match clojure.lang.Symbol [a b]
  (if (.startsWith (name a) "?")
    {a b}
    (when (= a b)
      {a b})))

(defn resolve-in [clause environment]
  (vec (map (fn [v] (get environment v v)) clause)))

(defn get-by-attribute [db attribute]
  (filter #(= attribute (second %)) db))

(defn process-results [results vars]
  (for [r results
        :when (every? #(contains? r %) vars)]
    (reduce
     (fn [m [k v]]
       (assoc m (keyword (subs (name k) 1)) v))
     {}
     (select-keys r vars))))

(defn f [vars query db]
  (let [vars (set vars)
        given-names (set (filter symbol? (tree-seq coll? seq query)))]
    (if (not (every? (partial contains? given-names) vars))
      (throw (Exception. "unknown variable"))
      (loop [facts (get-by-attribute db (second (first query)))
             clauses query
             results #{}
             environment {}
             prev nil]
        (if (and (not (seq facts))
                 (not prev))
          (process-results results vars)
          (if (or (not (seq facts))
                  (not (seq clauses)))
            (let [e environment
                  r results
                  {:keys [facts clauses results environment prev]} prev]
              (recur facts
                     clauses
                     (conj (into results r) e)
                     environment
                     prev))
            (let [clause (resolve-in (first clauses) environment)
                  fact (first facts)
                  m (map match clause fact)
                  pass? (and (seq m) (every? map? m))]
              (if pass?
                (let [new-env (apply merge m)]
                  (recur (get-by-attribute db (second (second clauses)))
                         (rest clauses)
                         results
                         (into environment new-env)
                         {:facts (rest facts)
                          :clauses clauses
                          :results results
                          :environment environment
                          :prev prev}))
                (recur (rest facts)
                       clauses
                       results
                       environment
                       prev)))))))))

