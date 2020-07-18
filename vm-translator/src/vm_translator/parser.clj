(ns vm-translator.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def parser (insta/parser
             (clojure.java.io/resource "vm.bnf")))
(defn parse
  [file-name]
  (let [rdr (io/reader file-name)
        lines (line-seq rdr)]
    (map (fn [line]
           {:code line
            :parse-tree (second (parser
                                 line))})
         lines)))


(comment

  (clojure.pprint/pprint (parse
                          "/home/anuj/nand2tetris/nand2tetris/projects/07/MemoryAccess/BasicTest/BasicTest.vm"))

  (clojure.pprint/pprint (parse
                          "/home/anuj/nand2tetris/nand2tetris/projects/08/FunctionCalls/NestedCall/Sys.vm"))

  ((parser) " push constant 0 ")

  )


