(ns day3.day3)

;; part 1
(->> (slurp "inputs/day3.txt") ; slurp 😋   
     (re-seq #"mul\(\d+,\d+\)") ; extract all valid mul instructions
     (mapv #(mapv Integer/parseInt (re-seq #"\d+" %))); extract numbers 
     (mapv #(apply * %)) ; multiply each pair
     (reduce +)) ; sum

;; part 2
(->> (slurp "inputs/day3.txt") ; slurp 😋
     (re-seq #"mul\(\d+,\d+\)|do(n't)?\(\)") ; extract all valid instructions
     (map first) ; only take full matching group
     (reduce (fn [[acc on?] instr]
               (case instr
                 "do()" [acc 1] ; if do, then on
                 "don't()" [acc 0] ; if don't, then off
                 [(+ acc (apply * on? (mapv Integer/parseInt (re-seq #"\d+" instr)))) on?])) ; else, extract ints, multiply
             [0 1])
     (first)) ; ignore "on"

;; part 1 (one line monstrosity)
(reduce + (apply map * (apply map list (mapv #(mapv Integer/parseInt (re-seq #"\d+" %)) (re-seq #"mul\(\d+,\d+\)" (slurp "inputs/day3.txt"))))))

;; part 2 (one line monstrosity)
(first (reduce (fn [[acc on?] instr] (case instr "do()" [acc 1] "don't()" [acc 0] [(+ acc (apply * on? (mapv Integer/parseInt (re-seq #"\d+" instr)))) on?])) [0 1] (mapv first (re-seq #"mul\(\d+,\d+\)|do(n't)?\(\)" (slurp "inputs/day3.txt")))))
