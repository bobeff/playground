(defn find-sub-str [s i sv]
  (loop [j (count s)]
    (if (> j i)
      (if (.contains sv (subs s i j))
        j
        (recur (dec j)))
      j)))

(defn shorten-ranges [s v]
  (let [sv (apply str v)]
    (loop [cs s i 0]
      (if (< i (count cs))
        (let [j (find-sub-str cs i sv)
              sub-str-len (- j i)]
          (if (> sub-str-len 2)
            (let [first-char (nth cs i)
                  last-char (nth cs (dec j))
                  replacer (str first-char ".." last-char)]
              (recur (str (subs cs 0 i)
                          replacer
                          (subs cs j))
                     (+ i 4)))
            (if (> sub-str-len 0)
              (recur cs (+ i sub-str-len))
              (recur cs (inc i)))))
        cs))))
