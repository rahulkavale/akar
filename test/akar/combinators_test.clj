(ns akar.combinators-test
  (:require [clojure.test :refer :all]
            [akar.patterns :refer :all]
            [akar.combinators :refer :all]
            [akar.primitives :refer :all])
  (:use midje.sweet))

(deftest combinators-test

  (facts "!and"

         (fact "fails if any of the patterns fails"
               ((!and !any !bind !fail) 9) => nil)

         (fact "emits all the values, when all patterns succeed"
               ((!and !any !bind !bind) 9) => [9 9])

         (fact "succeeds if no patterns are available"
               ((!and) 9) => []))

  (facts "!or"

         (fact "fails if no pattern succeeds"
               ((!or !fail !fail !fail) 9) => nil)

         (fact "emits values from the first matched pattern"
               ((!or !any !bind) 9) => []
               ((!or !bind !fail) 9) => [9])

         (fact "fails if no pattern is available"
               ((!or) 9) => nil))

  (facts "!not"

         (fact "reverses a good match"
               ((!not !bind) 9) => nil)

         (fact "reverses a bad match"
               ((!not !fail) 9) => []))

  (facts "!at"

         ;; this can be a simple !at with a predicate why need try-match
         (fact "if matched, gives the same input"
               (try-match* 2 (clauses*
                              (!at (!pred even?)) (fn [x] x))) => 2)
         ;; ((!at (!pred even?)) 2) => 2

         (fact "if not matched, gives nothing"
               (try-match* 3 (clauses*
                              (!at (!pred even?)) (fn [x] x))) => clause-not-applied))

  (facts "!guard"

         (fact "original pattern succeeds, only if guard succeeds too"
               (try-match* 2 (clauses*
                              (!guard (!pred even?) (partial = 2)) (fn [] :even-and-2))) => :even-and-2)

         (fact "original pattern fails, if the guard fails"
               (try-match* 4 (clauses*
                              (!guard (!pred even?) (partial = 2)) (fn [] :even-and-2))) => clause-not-applied))

  (facts "!view"

         (let [block (clauses*
                      (!view inc !bind) (fn [x] x))]
           (match* 9 block) => 10))

  (facts "!further"
         (let [block (clauses*
                      (!further !cons [!bind !bind]) (fn [hd tl]
                                                       {:hd hd
                                                        :tl tl}))]
           (fact "'furthers' a pattern"
                 (is (= {:hd 3 :tl [4 5]}
                        (match* [3 4 5] block))))))

  (facts "!further-many"

         (fact "'furthers' a pattern into variadic patterns list"
               (let [block (clauses*
                            (!further-many !seq [!bind !any !true !bind]) (fn [x y] [x y]))]
                 (try-match* [1 :whatevs true 3] block) => [1 3]
                 (try-match* [1 :whatevs false] block) => clause-not-applied))

         (fact "supports 'rest' patterns"
               (let [block (clauses*
                            (!further-many !seq [!bind !bind] !bind) (fn [a b rest]
                                                                       [a b rest]))]
                 (try-match* [3 4 5 6] block) => [3 4 [5 6]]
                 (try-match* [3 4] block) => [3 4 []]
                 (try-match* [3] block) => clause-not-applied))))
