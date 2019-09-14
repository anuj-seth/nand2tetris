(ns assembler.symbol-table)

(def predefined-symbols {"SP" "0"
                         "LCL" "1"
                         "ARG" "2"
                         "THIS" "3"
                         "THAT" "4"
                         "R0" "0"
                         "R1" "1"
                         "R2" "2"
                         "R3" "3"
                         "R4" "4"
                         "R5" "5"
                         "R6" "6"
                         "R7" "7"
                         "R8" "8"
                         "R9" "9"
                         "R10" "10"
                         "R11" "11"
                         "R12" "12"
                         "R13" "13"
                         "R14" "14"
                         "R15" "15"
                         "SCREEN" "16384"
                         "KBD" "24576"})

(def counter-increment-strategy {:COMMENT identity
                                 :EMPTYLINE identity
                                 :LABEL identity
                                 :A-INSTRUCTION inc
                                 :C-INSTRUCTION inc})

(defn- tag-line-numbers
  [{:keys [counter instructions]} current-instruction]
  (let [tp (first current-instruction)
        new-counter ((counter-increment-strategy tp)
                     counter)]
    {:counter new-counter
     :instructions (conj instructions
                         {:instruction current-instruction
                          :line-no counter})}))
(defn labels
  [instructions]
  (let [numbered-instructions (reduce tag-line-numbers
                                      {:counter 0
                                       :instructions []}
                                      instructions)
        label-instructions (filter #(= :LABEL (first (:instruction %)))
                                   (:instructions numbered-instructions))]
    (apply conj {}
           (map (fn [{:keys [instruction line-no]}]
                  [(second instruction) (str line-no)])
                label-instructions))))

(defn variables
  [instructions known-symbols]
  (let [all-symbols (map #(get-in % [2 1])
                           (filter (fn [[inst-tp _ [number-or-var _]]]
                                     (and (= :A-INSTRUCTION inst-tp)
                                           (= :VARIABLE number-or-var)))
                                   instructions))
        variables (map #(vector %1 (str (+ 16 %2)))
                       (distinct (remove known-symbols
                                         all-symbols))
                       (range))]
    (apply conj
           {}
           variables)))

(defn symbol-table
  [instructions]
  (let [symbols (merge predefined-symbols
                       (labels instructions))
        vars (variables instructions symbols)]
    (merge symbols
           vars)))

(comment

  (get-in [:A-INSTRUCTION "@" [:VARIABLE "R0"]]
          [2 1])

  (variables
   '([:COMMENT [:COMMENT-TEXT " This file is part of www.nand2tetris.org"]] [:COMMENT [:COMMENT-TEXT " and the book \"The Elements of Computing Systems\""]] [:COMMENT [:COMMENT-TEXT " by Nisan and Schocken, MIT Press."]] [:COMMENT [:COMMENT-TEXT " File name: projects/06/max/Max.asm"]] [:EMPTYLINE] [:COMMENT [:COMMENT-TEXT " Computes R2 = max(R0, R1)  (R0,R1,R2 refer to RAM[0],RAM[1],RAM[2])"]] [:EMPTYLINE] [:A-INSTRUCTION "@" [:VARIABLE "R0"]] [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:M "M"]]] [:A-INSTRUCTION "@" [:VARIABLE "R1"]] [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:DMINUSM "D-M"]]] [:A-INSTRUCTION "@" [:VARIABLE "OUTPUT_FIRST"]] [:C-INSTRUCTION [:COMPUTE [:DREG "D"]] [:JUMP [:JGT "JGT"]]] [:A-INSTRUCTION "@" [:VARIABLE "R1"]] [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:M "M"]]] [:A-INSTRUCTION "@" [:VARIABLE "OUTPUT_D"]] [:C-INSTRUCTION [:COMPUTE [:ZERO "0"]] [:JUMP [:JMP "JMP"]]] [:LABEL "OUTPUT_FIRST"] [:A-INSTRUCTION "@" [:VARIABLE "R0"]] [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:M "M"]]] [:LABEL "OUTPUT_D"] [:A-INSTRUCTION "@" [:VARIABLE "R2"]] [:C-INSTRUCTION [:DEST [:MEMORY "M"]] [:COMPUTE [:DREG "D"]]] [:LABEL "INFINITE_LOOP"] [:A-INSTRUCTION "@" [:VARIABLE "INFINITE_LOOP"]] [:C-INSTRUCTION [:COMPUTE [:ZERO "0"]] [:JUMP [:JMP "JMP"]]])
   {"R0" "0"})

  (let [instructions '([:COMMENT [:COMMENT-TEXT " This file is part of www.nand2tetris.org"]]
                       [:COMMENT [:COMMENT-TEXT " and the book \"The Elements of Computing Systems\""]]
                       [:COMMENT [:COMMENT-TEXT " by Nisan and Schocken, MIT Press."]]
                       [:COMMENT [:COMMENT-TEXT " File name: projects/06/rect/Rect.asm"]]
                       [:EMPTYLINE]
                       [:COMMENT [:COMMENT-TEXT " Draws a rectangle at the top-left corner of the screen."]]
                       [:COMMENT [:COMMENT-TEXT " The rectangle is 16 pixels wide and R0 pixels high."]]
                       [:EMPTYLINE]
                       [:A-INSTRUCTION "@" [:NUMBER "0"]]
                       [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:M "M"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "INFINITE_LOOP"]]
                       [:C-INSTRUCTION [:COMPUTE [:DREG "D"]] [:JUMP [:JLE "JLE"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "counter"]]
                       [:C-INSTRUCTION [:DEST [:MEMORY "M"]] [:COMPUTE [:DREG "D"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "SCREEN"]]
                       [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:AREG "A"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "address"]]
                       [:C-INSTRUCTION [:DEST [:MEMORY "M"]] [:COMPUTE [:DREG "D"]]]
                       [:LABEL "LOOP"]
                       [:A-INSTRUCTION "@" [:VARIABLE "address"]]
                       [:C-INSTRUCTION [:DEST [:REGISTER "A"]] [:COMPUTE [:M "M"]]]
                       [:C-INSTRUCTION [:DEST [:MEMORY "M"]] [:COMPUTE [:MINUSONE "-1"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "address"]]
                       [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:M "M"]]]
                       [:A-INSTRUCTION "@" [:NUMBER "32"]]
                       [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:DPLUSA "D+A"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "address"]]
                       [:C-INSTRUCTION [:DEST [:MEMORY "M"]] [:COMPUTE [:DREG "D"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "counter"]]
                       [:C-INSTRUCTION [:DEST [:MEMORY "M"] [:REGISTER "D"]] [:COMPUTE [:DECM "M-1"]]]
                       [:A-INSTRUCTION "@" [:VARIABLE "LOOP"]]
                       [:C-INSTRUCTION [:COMPUTE [:DREG "D"]] [:JUMP [:JGT "JGT"]]]
                       [:LABEL "INFINITE_LOOP"]
                       [:A-INSTRUCTION "@" [:VARIABLE "INFINITE_LOOP"]]
                       [:C-INSTRUCTION [:COMPUTE [:ZERO "0"]] [:JUMP [:JMP "JMP"]]])
        syms (merge predefined-symbols
                    (labels instructions))]
    (variables instructions syms))
  )
