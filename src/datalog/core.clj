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
  ((if (seq? clause) identity vec)
   (map (fn [v] (get environment v v)) clause)))

(defn get-by-attribute [db attribute]
  (if attribute
    (filter #(= attribute (second %)) db)
    db))

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

(defn g [vars query rules db]
  (let [vars (set vars)
        given-names (set (filter symbol? (tree-seq coll? seq query)))]
    (if (not (every? (partial contains? given-names) vars))
      (throw (Exception. "unknown variable"))
      ((fn F [facts clauses environment prev]
         (lazy-seq
          (if (and (every? #(contains? environment %) vars)
                   (not prev))
            [environment]
            (if (seq? (first clauses))
              (if-let [rules (seq (filter #(= (first (first %))
                                              (first (first clauses))) rules))]
                (let [[rule-name & args] (first clauses)]
                  (concat (apply concat (for [rule rules
                                              :let [[rule-head & rule-body] rule
                                                    free-rule-vars (->> (tree-seq coll? seq rule-body)
                                                                        (filter #(and (symbol? %)
                                                                                      (.startsWith (name %) "?")))
                                                                        (remove (set (rest rule-head)))
                                                                        set
                                                                        (map (juxt identity
                                                                                   gensym))
                                                                        (into {}))
                                                    rule-env (merge (zipmap (rest rule-head) args)
                                                                    free-rule-vars)
                                                    new-clauses (concat (map #(resolve-in % rule-env) rule-body)
                                                                        (rest clauses))]]
                                          (F (if (seq? (first new-clauses))
                                               (get-by-attribute db nil)
                                               (get-by-attribute db (second (first new-clauses))))
                                             new-clauses
                                             environment
                                             nil)))
                          (F nil nil environment prev)))
                (if-let [p (resolve (first (first clauses)))]
                  (if (apply p (map environment (rest (first clauses))))
                    (F (get-by-attribute db (second (second clauses)))
                       (rest clauses)
                       environment
                       prev)
                    (F (rest facts)
                       clauses
                       environment
                       prev))
                  (throw (Exception. "failed rule"))))
              (if (and (not (seq facts))
                       (not prev))
                nil
                (if (or (not (seq facts))
                        (not (seq clauses)))
                  (let [e environment
                        {:keys [facts clauses environment prev]} prev]
                    (if (every? #(contains? e %) vars)
                      (lazy-seq
                       (cons (reduce
                              (fn [m [k v]]
                                (assoc m (keyword (subs (name k) 1)) v))
                              {}
                              (select-keys e vars))
                             (F facts
                                clauses
                                environment
                                prev)))
                      (F facts
                         clauses
                         environment
                         prev)))
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
                         (F (get-by-attribute db (second (second clauses)))
                            (rest clauses)
                            (into environment new-env)
                            {:facts (rest facts)
                             :clauses clauses
                             :environment environment
                             :prev prev}))
                       (F (rest facts)
                          clauses
                          environment
                          prev)))))))
            (if (seq? (first clauses))
              (if-let [rules (seq (filter #(= (first (first %))
                                              (first (first clauses))) rules))]
                (let [[rule-name & args] (first clauses)]
                  (concat (apply concat (for [rule rules
                                              :let [[rule-head & rule-body] rule
                                                    free-rule-vars (->> (tree-seq coll? seq rule-body)
                                                                        (filter #(and (symbol? %)
                                                                                      (.startsWith (name %) "?")))
                                                                        (remove (set (rest rule-head)))
                                                                        set
                                                                        (map (juxt identity
                                                                                   gensym))
                                                                        (into {}))
                                                    rule-env (merge (zipmap (rest rule-head) args)
                                                                    free-rule-vars)
                                                    new-clauses (concat (map #(resolve-in % rule-env) rule-body)
                                                                        (rest clauses))]]
                                          (F (if (seq? (first new-clauses))
                                               (get-by-attribute db nil)
                                               (get-by-attribute db (second (first new-clauses))))
                                             new-clauses
                                             environment
                                             nil)))
                          (F nil nil environment prev)))
                (if-let [p (resolve (first (first clauses)))]
                  (if (apply p (map environment (rest (first clauses))))
                    (F (get-by-attribute db (second (second clauses)))
                       (rest clauses)
                       environment
                       prev)
                    (F (rest facts)
                       clauses
                       environment
                       prev))
                  (throw (Exception. "failed rule"))))
              ))))
       (if (seq? (first query))
         (get-by-attribute db nil)
         (get-by-attribute db (second (first query))))
       query
       {}
       nil))))
