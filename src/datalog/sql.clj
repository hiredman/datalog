(ns datalog.sql
  (:require [datalog.core :as d]
            [clojure.string :as str]
            [clojure.java.jdbc :as sql]
            [clojure.pprint :refer :all]))

(defn sql-q [vars query & [rules db]]
  (let [table-name "datalog"
        vars (set vars)
        given-names (set (filter symbol? (tree-seq coll? seq query)))]
    (if (not (every? (partial contains? given-names) vars))
      (throw (Exception. "unknown variable"))
      (let [{:keys [bind tables] :as m}
            (apply merge-with into
                   (for [clause query
                         :when (not (list? clause))
                         :let [m (gensym)
                               [entity attr value] clause]]
                     {:tables [m]
                      :bind (keep-indexed
                             (fn [idx item]
                               [{:table m
                                 :field (case idx
                                          0 :entity
                                          1 :attr
                                          2 :value)}
                                (if (d/logic-name? item)
                                  item
                                  (pr-str item))])
                             clause)}))
            rbind (reduce #(if (d/logic-name? (second %2))
                             (assoc % (second %2) (first %2))
                             %) {} bind)
            where (for [[{:keys [table field]} b] bind
                        :when (not (d/logic-name? b))]
                    [(str table "." (name field)) b])
            join (for [table tables]
                   (str table-name " AS " table))]
        (pprint m)
        (doto (vec (list* (str "SELECT "
                               (str/join ","
                                         (for [x vars
                                               :let [{:keys [table field]}
                                                     (get rbind x)]]
                                           (str (str table "." (name field))
                                                " AS "
                                                (subs (name x) 1))))
                               " "
                               (reduce (fn [accum x]
                                         (str accum " CROSS JOIN " x))
                                       (str "FROM " (first join))
                                       (rest join))
                               " WHERE "
                               (str/join " AND "
                                         (for [[a b] where]
                                           (str a " = ?")))
                               (apply str
                                      (for [[_ fields]
                                            (group-by second (seq bind))
                                            :when (< 1 (count fields))
                                            :let [fields (map first fields)]
                                            fielda fields
                                            fieldb fields
                                            :when (not= fielda fieldb)
                                            :let [{tablea :table
                                                   fielda :field} fielda
                                                   {tableb :table
                                                    fieldb :field} fieldb]]
                                        (str " AND " tablea "."
                                             (name fielda) " = "
                                             tableb "." (name fieldb)))))
                          (map second where)))
          prn)))))


(def db
  #{
    [#uuid "9d6280a3-7043-4e7e-9187-ad800743406a" :last-name "Downey"]
    [#uuid "9d6280a3-7043-4e7e-9187-ad800743406a" :street "Hanam"]
    [#uuid "a99b3f8c-e81f-450d-97b6-527d5db4c877" :name "Foo"]
    [#uuid "87408604-bc27-4650-adb4-cd36ef4e054a" :owners-name "Kevin"]
    [#uuid "a99b3f8c-e81f-450d-97b6-527d5db4c877" :url "http://foo"]
    [#uuid "5d416576-9c6c-49c3-98ad-70f44b525004" :owners-name "Michael"]
    [#uuid "ee3f8ced-d0ff-432b-b479-03aa7e8e0e4f" :last-name "Downey"]
    [#uuid "33005ea0-e284-4b0e-b120-e4d5b828630e" :street "Lake City Way NE"]
    [#uuid "5d416576-9c6c-49c3-98ad-70f44b525004" :name "Bar"]
    [#uuid "87408604-bc27-4650-adb4-cd36ef4e054a" :name "Bar"]
    [#uuid "5d416576-9c6c-49c3-98ad-70f44b525004" :url "http://bar"]
    [#uuid "9d6280a3-7043-4e7e-9187-ad800743406a" :first-name "Michael"]
    [#uuid "87408604-bc27-4650-adb4-cd36ef4e054a" :url "http://bar"]
    [#uuid "33005ea0-e284-4b0e-b120-e4d5b828630e" :last-name "Abelseth"]
    [#uuid "ee3f8ced-d0ff-432b-b479-03aa7e8e0e4f" :first-name "Kevin"]
    [#uuid "ee3f8ced-d0ff-432b-b479-03aa7e8e0e4f" :street "Lake City Way NE"]
    [#uuid "33005ea0-e284-4b0e-b120-e4d5b828630e" :first-name "Ariella"]
    [#uuid "a99b3f8c-e81f-450d-97b6-527d5db4c877" :owners-name "Ariella"]
    ["bill" :parent "mary"]
    ["mary" :parent "john"]
    ["john" :parent "jack"]
    ["jack" :parent "george"]
    ["tom" :parent "jack"]})

(defn f []
  (sql/with-connection {:classname "org.apache.derby.jdbc.EmbeddedDriver"
                        :subprotocol "derby"
                        :subname "/tmp/datalog.derby"
                        :create true}
    (sql/create-table :datalog
                      [:entity "varchar(1000)"]
                      [:attr "varchar(1000)"]
                      [:value "varchar(1000)"])
    (doseq [row db]
      (sql/insert-values
       :datalog
       [:entity :attr :value]
       (map pr-str row)))))

(defn g []
  (sql/with-connection {:classname "org.apache.derby.jdbc.EmbeddedDriver"
                        :subprotocol "derby"
                        :subname "/tmp/datalog.derby"
                        :create true}
    (sql/with-query-results res
      (sql-q '[?fname ?lname]
             '[[?person :street "Lake City Way NE"]
               [?person :first-name ?fname]
               [?person :last-name ?lname]])
      (for [r (doall res)]
        (zipmap (keys r)
                (map read-string (vals r)))))))


(defn q [vars query]
  (sql/with-connection {:classname "org.apache.derby.jdbc.EmbeddedDriver"
                        :subprotocol "derby"
                        :subname "/tmp/datalog.derby"
                        :create true}
    (sql/with-query-results res
      (sql-q vars
             query)
      (for [r (doall res)]
        (zipmap (keys r)
                (map read-string (vals r)))))))

(use 'clojure.pprint)

(defn dump-table []
  (sql/with-connection {:classname "org.apache.derby.jdbc.EmbeddedDriver"
                        :subprotocol "derby"
                        :subname "/tmp/datalog.derby"
                        :create true}
    (sql/with-query-results res
      ["SELECT * FROM datalog"]
      (doseq [r res]
        (pprint r)))))
