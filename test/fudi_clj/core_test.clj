(ns fudi-clj.core-test
  (:require [clojure.test :refer :all]
            [fudi-clj.core :refer :all]))

;; (deftest tcptest
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))


(def RNG (let [gen (new java.util.Random)]
           #(.nextFloat gen)))

(deftest test-1
  (let [send-random-every-100ms
        (future
          (while true
            (let [random-float (RNG)]
              (do
                (send-udp {:random random-float} :localhost 3001)
                (Thread/sleep 100)))))]
    (do
      (Thread/sleep 10000)
      (future-cancel send-random-every-100ms))))


(deftest test-2
  (let [send-random-seq-every-sec
        (future
          (while true
            (let [random-seq (repeatedly 512 RNG)]
              (do
                (send-udp {:reset true} :localhost 3001)
                (doseq [batch (partition-all 128 random-seq)]
                  (send-udp {:batch batch} :localhost 3001))
                (Thread/sleep 1000)))))]
    (do
      (Thread/sleep 10000)
      (future-cancel send-random-seq-every-sec))))
