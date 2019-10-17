(ns vm-translator.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [vm-translator.parser :as parser]
            [vm-translator.code :as code])
  (:gen-class))

(def debug true)
(def line-num (atom 0))

(defn -main
  [vm-file]
  (println vm-file)
  (let [write-with-newline (fn [writer line]
                             (.write writer line)
                             (.newLine writer))
        asm-file (string/replace vm-file #".vm" ".asm")
        parsed (parser/parse vm-file)
        instructions (filter :assembly
                             (code/add-assembly parsed))]
    (with-open [w (io/writer asm-file)]
      (doseq [{:keys [code assembly]} instructions]
        (.write w "// ")
        (write-with-newline w code)
        (doseq [hack-cmd assembly]
          (.write w hack-cmd)
          (when debug
            (write-with-newline w (str "   // " @line-num))
            (swap! line-num inc)))))))

