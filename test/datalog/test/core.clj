(ns datalog.test.core
  (:use [datalog.core]
        [clojure.test])
  (:import (java.util UUID)))

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

(deftest t-stuff
  (is (= (f '[?p2-first-name ?p1-last-name ?p1-name ?place1]
            '[[?person1 :street "Lake City Way NE"]
              [?person1 :last-name ?p1-last-name]
              [?person2 :last-name ?p1-last-name]
              [?person2 :street "Hanam"]
              [?person2 :first-name ?p2-first-name]
              [?place1 :owners-name ?p2-first-name]
              [?place1 :name ?p1-name]]
            db)
         '({:p2-first-name "Michael",
            :p1-last-name "Downey",
            :p1-name "Bar"
            :place1 #uuid "5d416576-9c6c-49c3-98ad-70f44b525004"})))

  (is (= (f '[?p1-first-name ?p1-last-name ?p2-first-name]
            '[[?person1 :street "Lake City Way NE"]
              [?person1 :last-name ?p1-last-name]
              [?person1 :first-name ?p1-first-name]
              [?person2 :last-name ?p1-last-name]
              [?person2 :first-name ?p2-first-name]]
            db)
         '({:p2-first-name "Michael",
            :p1-last-name "Downey",
            :p1-first-name "Kevin"}
           {:p2-first-name "Kevin",
            :p1-last-name "Downey",
            :p1-first-name "Kevin"}
           {:p2-first-name "Ariella",
            :p1-last-name "Abelseth",
            :p1-first-name "Ariella"})))

  (is (= (set (g '[?p1-first-name ?p1-last-name ?p2-first-name]
                 '[[?person1 :street "Lake City Way NE"]
                   [?person1 :last-name ?p1-last-name]
                   [?person1 :first-name ?p1-first-name]
                   [?person2 :last-name ?p1-last-name]
                   [?person2 :first-name ?p2-first-name]]
                 []
                 db))
         (set '({:p2-first-name "Michael",
                 :p1-last-name "Downey",
                 :p1-first-name "Kevin"}
                {:p2-first-name "Kevin",
                 :p1-last-name "Downey",
                 :p1-first-name "Kevin"}
                {:p2-first-name "Ariella",
                 :p1-last-name "Abelseth",
                 :p1-first-name "Ariella"}))))

  (is (= (g '[?p2-first-name ?p1-last-name ?p1-name ?place1]
            '[[?person1 :street "Lake City Way NE"]
              [?person1 :last-name ?p1-last-name]
              [?person2 :last-name ?p1-last-name]
              [?person2 :street "Hanam"]
              [?person2 :first-name ?p2-first-name]
              [?place1 :owners-name ?p2-first-name]
              [?place1 :name ?p1-name]]
            []
            db)
         '({:p2-first-name "Michael",
            :p1-last-name "Downey",
            :p1-name "Bar"
            :place1 #uuid "5d416576-9c6c-49c3-98ad-70f44b525004"})))
  
  (is (= (g '[?a]
            '[(ancestor "bill" ?a)]
            '[[[ancestor ?X ?Y] [?X :parent ?Y]]
              [[ancestor ?X ?Y] [?X :parent ?Z] (ancestor ?Z ?Y)]]
            db)
         '({:a "mary"}
           {:a "john"}
           {:a "jack"}
           {:a "george"})))

  (is (= (g '[?a]
            '[(sibling "tom" ?a)]
            '[[[sibling ?X ?Y]
               [?X :parent ?Z]
               [?Y :parent ?Z]]]
            db)
         '({:a "john"}
           {:a "tom"})))

  (is (= (g '[?fname ?f]
            '[[?person :street "Lake City Way NE"]
              (family ?person ?relative)
              [?person :first-name ?f]
              [?relative :first-name ?fname]
              (not= ?person ?relative)]
            '[[[family ?X ?Y]
               [?X :last-name ?Z]
               [?Y :last-name ?Z]]]
            db)
         '({:fname "Michael", :f "Kevin"}))))
