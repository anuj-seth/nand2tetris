(ns assembler.core
  (:require [assembler.parser :as parser]
            [assembler.code :as code])
  (:gen-class))

(defn -main
  [& args]
  (let [asm-file (first args)
        parsed (parser/parse asm-file)]
    (map code/encode parsed)))

(comment 
  (-main "/home/anuj/nand2tetris/nand2tetris/projects/06/add/Add.asm"))


