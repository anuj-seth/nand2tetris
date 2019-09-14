(ns assembler.core
  (:require [assembler.parser :as parser]
            [assembler.code :as code]
            [assembler.symbol-table :as sym-tab]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  [asm-file]
  (let [hack-file (string/replace asm-file #".asm" ".hack")
        parsed (parser/parse asm-file)
        symbol-table (sym-tab/symbol-table parsed)
        instructions (remove nil?
                             (map #(code/encode %
                                                symbol-table)
                                  parsed))]
    (with-open [w (io/writer hack-file)]
      (doseq [instruction instructions]
        (.write w instruction)
        (.newLine w)))))

(comment 
  (-main "/home/anuj/nand2tetris/nand2tetris/projects/06/add/Add.asm")

  (parser/parse "/home/anuj/nand2tetris/nand2tetris/projects/06/pong/miniPong.asm"))




