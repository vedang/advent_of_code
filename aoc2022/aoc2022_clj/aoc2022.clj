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
         (str/split #"\n")))))

(defn aoc-url
  [year day]
  (format "https://adventofcode.com/%s/day/%s/input" year day))

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

(let [code->move {"A" :rock
                  "X" :rock
                  "B" :paper
                  "Y" :paper
                  "C" :scissors
                  "Z" :scissors}
      wins {:rock :paper
            :paper :scissors
            :scissors :rock}
      loses (reduce (fn [acc [k v]] (assoc acc v k)) {} wins)
      score {:lose 0, :paper 2, :scissors 3, :rock 1, :win 6, :draw 3}
      strategy (get-input (aoc-url 2022 02))]

  (defn day-2-part-1
    []
    (->> strategy
         (map (fn [s]
                (let [play (map code->move (str/split s #" "))]
                  (+ (score (second play))
                     (cond
                       (= (second play) (wins (first play))) (score :win)
                       (= (second play) (first play)) (score :draw)
                       (= (second play) (loses (first play))) (score :lose))))))
         (apply +)))

 (defn day-2-part-2
   []
   (apply +
          (map (fn [s]
                 (let [[m strat] (str/split s #" ")
                       move (code->move m)
                       our-move (case strat
                                  "X" (loses move)
                                  "Y" move
                                  "Z" (wins move))]
                   (+ (score our-move)
                      (case strat
                        "Z" (score :win)
                        "Y" (score :draw)
                        "X" (score :lose)))))
               strategy))))
