(ns assembler.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(defn parse
  [file-name]
  (let [parser (insta/parser (clojure.java.io/resource
                              "assembly.bnf"))
        rdr (io/reader file-name)
        lines (line-seq rdr)]
    (map (comp second parser)
         lines)))

(comment 
  (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/add/Add.asm")

  (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/max/MaxL.asm")

  (parse "/home/anuj/nand2tetris/nand2tetris/projects/06/max/Max.asm")

  (let [parser (insta/parser (clojure.java.io/resource
                              "assembly.bnf"))]
    (parser "  D=M              // D = first number"))

  )





