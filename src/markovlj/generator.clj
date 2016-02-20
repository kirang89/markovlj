(ns markovlj.generator
  (:gen-class)
  (:require [clojure.string  :as string])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(def prefix-list ["On the" "We think" "For every"
                  "To a" "And every" "For his" "And the"
                  "But the" "Are the" "For the" "When we"
                  "In the" "With only" "Are the"
                  "Though the"  "And when" "We sit" "And this"
                  "With a" "And at" "Of the" "So that"
                  "And all" "When they" "But before"
                  "And it's" "For example," "Also in"])

(defn train [data]
  (let [words   (string/split data #"[\s|\n]")
        words3  (partition-all 3 1 words)]
    (reduce (fn [m [w1 w2 w3 :as l]]
              (let [res (get m [w1 w2] #{})]
                (if-not (nil? w2)
                  (assoc m [w1 w2] (conj res w3)) m)))
            {} words3)))


(defn get-training-data [dirname]
  (->> (.list (io/file dirname))
       (map #(train (slurp (str dirname "/" %))))
       (apply merge-with set/union)))


(defn sanitize [text]
  (let [cleaned-text (apply str (re-seq #"[^.,?!]*[\s\w]+[.,?!]" text))
        final-text (string/replace cleaned-text #"[,|\s]$" ".")]
    (if (empty? final-text) text final-text)))


(defn generate-text [start length data]
  (loop [prefixes (string/split start #"\s")
         res      prefixes]
    (let [suffix      (first (shuffle (get data prefixes #{})))
          new-prefix  [(second prefixes) suffix]
          text        (apply str (interpose " " res))
          total-count (+ (count text) (count suffix))]
      (if (or (>= total-count length) (nil? suffix))
        (sanitize text)
        (recur new-prefix (conj res suffix))))))


(defn -main
  [& args]
  (if (>= (count args) 2)
    (let [dirname       (second args)
          prefix        (first (shuffle prefix-list))
          char-count    (Integer. (first args))
          training-data (get-training-data dirname)]
      (prn (generate-text prefix char-count training-data)))
    (println "Invalid arguments. Requires character count and directory for training data.")))
