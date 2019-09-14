(ns assembler.util)

(defn decimal-to-binary-n-digit
  [d n]
  (let [fmt (str  "%" n "s")
        space-padded-binary (format fmt
                                    (Integer/toBinaryString d))
        zero-padded-binary (clojure.string/replace space-padded-binary
                                                   #" "
                                                   "0")]
    zero-padded-binary))

(defn decimal-to-binary-n-digit-4-nybbles
  [d n]
  (let [fmt (str  "%" n "s")
        space-padded-binary (format fmt
                                    (Integer/toBinaryString d))
        zero-padded-binary (clojure.string/replace space-padded-binary
                                                   #" "
                                                   "0")
        nybbles (map #(apply str %)
                     (partition 4
                                zero-padded-binary))]
    (clojure.string/join " " nybbles)))
