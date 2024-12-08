(ns re-graph.internals-test
  (:require [re-graph.internals :as internals]
            [re-graph.core :as re-graph]
            [day8.re-frame.test :refer [run-test-sync]
             :refer-macros [run-test-sync]]
            [clojure.test :refer [deftest is testing]
             :refer-macros [deftest is testing]]))

(deftest options-test
  (run-test-sync

    (testing "WebSocket options"
      (is (nil? (internals/ws-options {:ws nil})))
      (is (nil? (internals/ws-options {:ws {:url nil}})))

      (is (= (merge internals/ws-initial-state internals/ws-default-options) (:ws (internals/ws-options {:ws {}}))))

      (let [test-url "ws://example.org/graphql-ws"
            options  (internals/ws-options {:ws {:url test-url}})]
        (is (= test-url (get-in options [:ws :url])))
        (is (= "graphql-ws" (get-in options [:ws :sub-protocol])))))

    (testing "HTTP options"
      (is (nil? (internals/http-options {:http nil})))
      (is (nil? (internals/http-options {:http {:url nil}})))

      (is (= (merge internals/http-initial-state internals/http-default-options) (:http (internals/http-options {:http {}}))))

      (let [test-url "http://example.org/graphql"
            options  (internals/http-options {:http {:url test-url}})]
        (is (= test-url (get-in options [:http :url])))))))

(defmacro m ([a]) ([a b]))
(defmacro mx [])

#?(:cljs
   (deftest test-max-arity
     ;; for CLJS max-arity only returns 1
     (is (= 1 (internals/max-arity #(+ % 32))))
     (is (= 1 (internals/max-arity #(+ %1 %2)))))

   :clj
   (deftest test-max-arity
     (testing "with an anonymous #(… %1) function"
       (is (= 1 (internals/max-arity #(+ % 32))))
       (is (= 1 (internals/max-arity #(+ %1 32))))
       (is (= 2 (internals/max-arity #(+ %1 %2))))
       (is (= 13 (internals/max-arity #(+ %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13))))
       (is (= :variadic (internals/max-arity #(apply + %&))))
       (is (= :variadic (internals/max-arity #(apply + % %&)))))
     (testing "with an anonymous (fn [] …) function"
       (testing "single body"
         (is (= 0 (internals/max-arity (fn []))))
         (is (= 1 (internals/max-arity (fn [a]))))
         (is (= 2 (internals/max-arity (fn [a b]))))
         (is (= 20 (internals/max-arity (fn [a b c d e f g h i j k l m n o p q r s t]))))
         (is (= :variadic (internals/max-arity (fn [a b & more])))))
       (testing "multiple bodies"
         (is (= 0 (internals/max-arity (fn ([])))))
         (is (= 1 (internals/max-arity (fn ([a])))))
         (is (= 2 (internals/max-arity (fn ([a]) ([a b])))))
         (is (= :variadic (internals/max-arity (fn ([a]) ([a b & c])))))))
     (testing "with a defined function"
       (is (= :variadic (internals/max-arity map)))
       (is (= :variadic (internals/max-arity +)))
       (is (= 1 (internals/max-arity inc))))
     (testing "with a var to a macro"
       (is (= :variadic (internals/max-arity #'->)))
       (is (= 2 (internals/max-arity #'m)))
       (is (= 0 (internals/max-arity #'mx))))))