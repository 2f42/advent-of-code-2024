(ns day3.day3)

;; part 1
(->> (slurp "inputs/day3.txt") ; slurp 😋   
     (re-seq #"mul\(\d+,\d+\)") ; extract all valid mul instructions
     (mapv #(mapv Integer/parseInt (re-seq #"\d+" %))); extract numbers 
     (mapv #(apply * %)) ; multiply each pair
     (reduce +)) ; sum

;; part 2
(->> (slurp "inputs/day3.txt") ; slurp 😋
     (re-seq #"(mul\(\d+,\d+\)|do(n't)?\(\))") ; extract all valid instructions
     (map first) ; only take full matching group
     (reduce (fn [[acc on?] instr]
               (cond
                 (re-matches #"mul\(\d+,\d+\)" instr) [(+ acc (apply * on? (mapv Integer/parseInt (re-seq #"\d+" instr)))) on?] ; extract ints, multiply
                 (re-matches #"do\(\)" instr) [acc 1] ; if do, then on
                 :else [acc 0])) ; if don't, then off
             [0 1])
     (first)) ; ignore "on"
