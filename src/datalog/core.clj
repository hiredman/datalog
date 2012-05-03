(ns datalog.core)

(defn logic-name? [o]
  (and (symbol? o)
       (.startsWith (name o) "?")))

(defmulti match (comp type first list))

(defmethod match :default [a b]
  (when (isa? b a)
    {a b}))

(defmethod match clojure.lang.Symbol [a b]
  (if (logic-name? a)
    {a b}
    (when (= a b)
      {a b})))

(defn resolve-in [clause environment]
  ((if (seq? clause) identity vec)
   (map (fn [v] (get environment v v)) clause)))

(defn get-by-attribute [db attribute]
  (if attribute
    (filter #(= attribute (second %)) db)
    db))

(declare search)

(defn search-with-rules
  [rule-name args db rules vars facts clauses environment prev]
  (apply concat (for [rule rules
                      :let [[rule-head & rule-body] rule
                            free-rule-vars (->> (tree-seq coll? seq rule-body)
                                                (filter logic-name?)
                                                (remove (set (rest rule-head)))
                                                set
                                                (map (juxt identity
                                                           gensym))
                                                (into {}))
                            rule-env (merge (zipmap (rest rule-head) args)
                                            free-rule-vars)
                            new-clauses (concat (map
                                                 #(resolve-in % rule-env)
                                                 rule-body)
                                                (rest clauses))]]
                  (search db rules
                          vars
                          (if (seq? (first new-clauses))
                            (get-by-attribute db nil)
                            (get-by-attribute db (second (first new-clauses))))
                          new-clauses
                          environment
                          nil))))

(defn rule [db rules vars facts clauses environment prev]
  (lazy-seq
   (let [[rule-name & args] (first clauses)]
     (concat
      (search-with-rules
        rule-name args db rules vars facts clauses environment prev)
      (search db rules vars nil nil environment prev)))))

(defn rule-or-predicate [db rules vars facts clauses environment prev]
  (lazy-seq
   (if-let [rules (seq (filter #(= (first (first %))
                                   (first (first clauses))) rules))]
     (rule db rules vars facts clauses environment prev)
     (if-let [p (resolve (first (first clauses)))]
       (if (apply p (map environment (rest (first clauses))))
         (search db rules vars
                 (if (seq? (first (rest clauses)))
                   (get-by-attribute db nil)
                   (get-by-attribute db (second (first (rest clauses)))))
                 (rest clauses)
                 environment
                 prev)
         (when prev
           (let [{:keys [facts clauses environment prev]} prev]
             (search db rules
                     vars
                     facts
                     clauses
                     environment
                     prev))))
       (throw (Exception. "failed rule"))))))

(defn out-of-facts-or-clauses
  [db rules vars facts clauses environment prev]
  (let [e environment
        {:keys [facts clauses environment prev]} prev]
    (if (every? #(contains? e %) vars)
      (lazy-seq
       (cons (reduce
              (fn [m [k v]]
                (assoc m (keyword (subs (name k) 1)) v))
              {}
              (select-keys e vars))
             (search db rules
                     vars
                     facts
                     clauses
                     environment
                     prev)))
      (search db rules
              vars
              facts
              clauses
              environment
              prev))))

(defn search [db rules vars facts clauses environment prev]
  (lazy-seq
   (if (and (every? #(contains? environment %) vars)
            (not prev))
     [environment]
     (if (seq? (first clauses))
       (rule-or-predicate db rules vars facts clauses environment prev)
       (if (and (not (seq facts))
                (not prev))
         nil
         (if (or (not (seq facts))
                 (not (seq clauses)))
           (out-of-facts-or-clauses
            db rules vars facts clauses environment prev)
           (lazy-seq
            (let [clause (resolve-in (first clauses) environment)
                  fact (first facts)
                  m (map match clause fact)
                  pass? (and (seq m)
                             (every? map? m)
                             (= (count (set (map keys m)))
                                (count (set (map vals m)))))]
              (if pass?
                (let [new-env (apply merge m)]
                  (search db rules
                          vars
                          (get-by-attribute db (second (second clauses)))
                          (rest clauses)
                          (into environment new-env)
                          {:facts (rest facts)
                           :clauses clauses
                           :environment environment
                           :prev prev}))
                (search db rules
                        vars
                        (rest facts)
                        clauses
                        environment
                        prev))))))))))

(defn q [vars query rules db]
  (let [vars (set vars)
        given-names (set (filter symbol? (tree-seq coll? seq query)))]
    (if (not (every? (partial contains? given-names) vars))
      (throw (Exception. "unknown variable"))
      (search
       db
       rules
       vars
       (if (seq? (first query))
         (get-by-attribute db nil)
         (get-by-attribute db (second (first query))))
       query
       {}
       nil))))
