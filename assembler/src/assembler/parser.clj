(ns assembler.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def parser (insta/parser (clojure.java.io/resource
                           "assembly.bnf")))
(defn parse
  [file-name]
  (let [rdr (io/reader file-name)
        lines (line-seq rdr)]
    (map (comp second parser)
         (remove #(or (= % "")
                      (= \/ (first %)))
                 lines))))

(comment 
  (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/add/Add.asm")

  (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/max/MaxL.asm")

  (let [file-name "/home/anuj/nand2tetris/nand2tetris/projects/06/max/Max.asm"
        rdr (io/reader file-name)
        lines (line-seq rdr)]
    (clojure.pprint/pprint
     (map #(insta/parses
            parser
            % :partial true)
          lines)))

  (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/max/Max.asm")

  (time (count
         (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/pong/Pong.asm")))

  (let [parser (insta/parser (clojure.java.io/resource
                              "assembly.bnf"))]
    (parser "  D=M              // D = first number"))

  )





