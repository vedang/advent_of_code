(ns aoc2022
  (:require [clj-http.client :as client]
            [clojure.string :as str]))

(defonce ^:dynamic *cookie-string* nil)

(def get-input
  (memoize
   (fn get-input* [url]
     (-> url
         (client/get {:headers {:cookie *cookie-string*}})
         :body
         str/split-lines))))

(defn aoc-url
  [year day]
  (format "https://adventofcode.com/%s/day/%s/input" year day))

;;; ================
(defn day-1-part-1
  []
  (->> (aoc-url 2022 01)
       get-input
       (partition-by (partial = ""))
       (remove (partial = '("")))
       (map (fn [c] (map parse-long c)))
       (map (fn [c] (apply + c)))
       (sort >)
       first))

(defn day-1-part-2
  []
  (->> (aoc-url 2022 01)
       get-input
       (partition-by (partial = ""))
       (remove (partial = '("")))
       (map (fn [c] (map parse-long c)))
       (map (fn [c] (apply + c)))
       (sort >)
       (take 3)
       (apply +)))

;;; Total Time taken for day 1: 21 minutes
;;; ================

(let [->move {"A" :rock "X" :rock
              "B" :paper "Y" :paper
              "C" :scissors "Z" :scissors}
      ->loses {:rock :paper
               :paper :scissors
               :scissors :rock}
      ->wins (reduce (fn [acc [k v]] (assoc acc v k)) {} ->loses)
      score {:lose 0 :draw 3 :win 6
             :rock 1 :paper 2 :scissors 3}]

  (defn day-2-part-1
    []
    (->> (aoc-url 2022 02)
         get-input
         (map (fn [s]
                (let [[them us] (map ->move (str/split s #" "))]
                  (cond
                    (= us (->loses them)) (+ (score us) (score :win))
                    (= us them) (+ (score us) (score :draw))
                    :else (+ (score us) (score :lose))))))
         (apply +)))

 (defn day-2-part-2
   []
   (->> (aoc-url 2022 02)
        get-input
        (map (fn [s]
               (let [[play strat] (str/split s #" ")
                     move (->move play)]
                 (case strat
                   "Z" (+ (score (->loses move)) (score :win))
                   "Y" (+ (score move) (score :draw))
                   "X" (+ (score (->wins move)) (score :lose))))))
        (apply +))))

;;; Total time taken for day 2: 19 minutes
;;; ================
