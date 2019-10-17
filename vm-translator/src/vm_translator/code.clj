(ns vm-translator.code
  (:require [instaparse.core :as insta]))

(def predefined-symbols {:REGISTER/SP 0
                         :REGISTER/LCL 1
                         :REGISTER/ARG 2
                         :REGISTER/THIS 3
                         :REGISTER/THAT 4
                         :REGISTER/R0 0
                         :REGISTER/R1 1
                         :REGISTER/R2 2
                         :REGISTER/R3 3
                         :REGISTER/R4 4
                         :REGISTER/R5 5
                         :REGISTER/R6 6
                         :REGISTER/R7 7
                         :REGISTER/R8 8
                         :REGISTER/R9 9
                         :REGISTER/R10 10
                         :REGISTER/R11 11
                         :REGISTER/R12 12
                         :REGISTER/R13 13
                         :REGISTER/R14 14
                         :REGISTER/R15 15
                         :MEM-LOCATION/SCREEN 16384
                         :MEM-LOCATION/KBD 24576
                         :MEM-SEGMENT/LOCAL    "LCL"
                         :MEM-SEGMENT/ARGUMENT "ARG"
                         :MEM-SEGMENT/THIS     "THIS"
                         :MEM-SEGMENT/THAT     "THAT"})

(defmulti segment-encoder (fn [[segment] _] segment))

(defmethod segment-encoder :CONSTANT
  [_ index]
  {:type :CONSTANT
   :assembly [(str "@" index)]})

(defmethod segment-encoder :TEMP
  [_ index]
  {:type :TEMP
   :assembly ["@5"
              "D=A"
              (str  "@" index)
              "A=D+A"]})

(defmethod segment-encoder :default
  [[segment] index]
  (let [register {:LOCAL    "LCL"
                  :ARGUMENT "ARG"
                  :THIS     "THIS"
                  :THAT     "THAT"}]
    [(str "@"
          (register segment))
     "D=M"
     (str "@" index)
     "D=D+A"
     "A=D"]))

(def assembly {:end ["(END)" "@END" "0;JMP"]
               :move-sp-to-a ["A=M"]
               :move-a-to-sp-target ["D=A" "@SP" "A=M" "M=D"]
               :move-a-target-to-sp-target ["D=M" "@SP" "A=M" "M=D"]
               :move-d-to-sp-target ["@SP" "A=M" "M=D"]
               :move-sp-target-to-d ["@SP" "A=M" "D=M"]
               :move-sp-target-to-a ["@SP" "A=M" "A=M"]
               :inc-sp ["@SP" "M=M+1"]
               :dec-sp ["@SP" "M=M-1"]})

(defn binary-op
  [operation]
  (let [op ({:ADD "+"
             :SUB "-"
             :AND "&"
             :OR  "|"} operation)]
    (concat (:dec-sp assembly)
            (:move-sp-target-to-d assembly)
            (:dec-sp assembly)
            (:move-sp-to-a assembly)
            [(str "M=M" op "D")]
            (:inc-sp assembly))))

(defn unary-op
  [operation]
  (let [op ({:NOT "!"
             :NEG "-"} operation)]
    (concat (:dec-sp assembly)
            (:move-sp-target-to-d assembly)
            [(str "D=" op "D")]
            (:move-d-to-sp-target assembly)
            (:inc-sp assembly))))

(def label-counter (atom 0))

(defn comparison-op
  [comparison-type]
  (let [counter (swap! label-counter inc)
        op-data {:LT {:label-name (str "LT_" counter) :op "JLT"}
                 :GT {:label-name (str "GT_" counter) :op "JGT"}
                 :EQ {:label-name (str "EQ_" counter) :op "JEQ"}}
        {:keys [label-name op]} (op-data comparison-type)]
    (concat (:dec-sp assembly)
            (:move-sp-target-to-d assembly)
            (:dec-sp assembly)
            (:move-sp-target-to-a assembly)
            ["D=A-D"
             (str "@" label-name)
             (str "D;" op)
             "D=0"
             (str "@D_TO_SP_" counter)
             "0;JMP"
             (str "(" label-name ")")
             "D=-1"
             (str  "(D_TO_SP_" counter ")")]
            (:move-d-to-sp-target assembly)
            (:inc-sp assembly))))

(derive :MEM-SEGMENT/LOCAL :MEM-SEGMENT/PHYSICAL)
(derive :MEM-SEGMENT/ARGUMENT :MEM-SEGMENT/PHYSICAL)
(derive :MEM-SEGMENT/THIS :MEM-SEGMENT/PHYSICAL)
(derive :MEM-SEGMENT/THAT :MEM-SEGMENT/PHYSICAL)
(defmulti stack-cmd (fn [push-or-pop [segment idx]] [push-or-pop segment]))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/CONSTANT]
  [_ [_ idx]]
  (println "in push constant " idx)
  (concat [(str "@" idx)]
          (:move-a-to-sp-target assembly)
          (:inc-sp assembly)))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/TEMP]
  [_ [_ idx]]
  (concat ["@5"
           "D=A"
           (str  "@" idx)
           "A=D+A"]
          (:move-a-target-to-sp-target assembly)
          (:inc-sp assembly)))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/PHYSICAL]
  [_ [segment idx]]
  (println "in PHYSICAL push" segment)
  (let [register #:MEM-SEGMENT{:LOCAL    "LCL"
                               :ARGUMENT "ARG"
                               :THIS     "THIS"
                               :THAT     "THAT"}]
    (concat [(str "@"
                  (register segment))
             "D=M"
             (str "@" idx)
             "D=D+A"
             "A=D"]
            (:move-a-target-to-sp-target assembly)
            (:inc-sp assembly))))

(defmethod stack-cmd :POP
  [_ segment]
  (concat segment
          ["D=A" "@R5" "M=D"]
          (:dec-sp assembly)
          (:move-sp-target-to-d assembly)
          ["@R5" "A=M" "M=D"]))

(defmethod stack-cmd :default
  [push-or-pop segment]
  (println push-or-pop segment))

(defn encode
  [{:keys [parse-tree] :as data}]
  (assoc data
         :assembly
         (insta/transform {:END       (constantly (:end assembly))
                           :COMMENT   (constantly nil)
                           :EMPTYLINE (constantly nil)
                           :INDEX     identity 
                           :SEGMENT   (fn [[segment] idx]
                                        [(keyword "MEM-SEGMENT" (name segment)) idx]) ;;segment-encoder
                           :PUSH      (constantly :PUSH)
                           :POP       (constantly :POP)
                           :STACK-CMD stack-cmd
                           :ADD       #(binary-op :ADD)
                           :SUB       #(binary-op :SUB)
                           :AND       #(binary-op :AND)
                           :OR        #(binary-op :OR)
                           :EQ        #(comparison-op :EQ)
                           :LT        #(comparison-op :LT)
                           :GT        #(comparison-op :GT)
                           :NEG       #(unary-op :NEG)
                           :NOT       #(unary-op :NOT)}
                          parse-tree)))

(defn add-assembly
  [parse-trees]
  (conj (mapv encode parse-trees)
        (encode {:code "END loop"
                 :parse-tree [:END]})))


(comment
  (insta/transform
   {:COMMENT nil
    :EMPTYLINE (constantly nil)
    :INDEX #(Integer/parseInt %)
    :CONSTANT (constantly :CONSTANT)}
   [:EMPTYLINE])

  (insta/transform {:COMMENT (constantly nil)
                    :EMPTYLINE (constantly nil)
                    :INDEX identity 
                    :CONSTANT (constantly :CONSTANT)
                    :SEGMENT segment-encoder
                    :PUSH (constantly ["D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"])
                    :STACK-CMD (fn [push-or-pop segment]
                                 (concat segment
                                         push-or-pop))}
                   [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "8"]]])

  (add-assembly
       '({:code "// This file is part of www.nand2tetris.org",
          :parse-tree [:COMMENT [:COMMENT-TEXT " This file is part of www.nand2tetris.org"]]}
         {:code "// and the book \"The Elements of Computing Systems\"",
          :parse-tree [:COMMENT [:COMMENT-TEXT " and the book \"The Elements of Computing Systems\""]]}
         {:code "// by Nisan and Schocken, MIT Press.",
          :parse-tree [:COMMENT [:COMMENT-TEXT " by Nisan and Schocken, MIT Press."]]}
         {:code "// File name: projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm",
          :parse-tree [:COMMENT [:COMMENT-TEXT " File name: projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm"]]}
         {:code "", :parse-tree [:EMPTYLINE]}
         {:code "// Pushes and adds two constants.",
          :parse-tree [:COMMENT [:COMMENT-TEXT " Pushes and adds two constants."]]}
         {:code "push constant 7",
          :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "7"]]]}
         {:code "push constant 8",
          :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "8"]]]}
         {:code "push local 8",
          :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "8"]]]}
         {:code "add", :parse-tree [:ADD]}))
  (add-assembly
   '({:code "// This file is part of www.nand2tetris.org",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT " This file is part of www.nand2tetris.org"]]}
     {:code "// and the book \"The Elements of Computing Systems\"",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " and the book \"The Elements of Computing Systems\""]]}
     {:code "// by Nisan and Schocken, MIT Press.",
      :parse-tree
      [:COMMENT [:COMMENT-TEXT " by Nisan and Schocken, MIT Press."]]}
     {:code
      "// File name: projects/07/StackArithmetic/StackTest/StackTest.vm",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " File name: projects/07/StackArithmetic/StackTest/StackTest.vm"]]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "// Executes a sequence of arithmetic and logical operations",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " Executes a sequence of arithmetic and logical operations"]]}
     {:code "// on the stack. ",
      :parse-tree [:COMMENT [:COMMENT-TEXT " on the stack. "]]}
     {:code "push constant 17",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "17"]]]}
     {:code "push constant 17",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "17"]]]}
     {:code "eq", :parse-tree [:EQ]}
     {:code "push constant 17",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "17"]]]}
     {:code "push constant 16",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "16"]]]}
     {:code "eq", :parse-tree [:EQ]}
     {:code "push constant 16",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "16"]]]}
     {:code "push constant 17",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "17"]]]}
     {:code "eq", :parse-tree [:EQ]}
     {:code "push constant 892",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "892"]]]}
     {:code "push constant 891",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "891"]]]}
     {:code "lt", :parse-tree [:LT]}
     {:code "push constant 891",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "891"]]]}
     {:code "push constant 892",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "892"]]]}
     {:code "lt", :parse-tree [:LT]}
     {:code "push constant 891",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "891"]]]}
     {:code "push constant 891",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "891"]]]}
     {:code "lt", :parse-tree [:LT]}
     {:code "push constant 32767",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "32767"]]]}
     {:code "push constant 32766",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "32766"]]]}
     {:code "gt", :parse-tree [:GT]}
     {:code "push constant 32766",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "32766"]]]}
     {:code "push constant 32767",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "32767"]]]}
     {:code "gt", :parse-tree [:GT]}
     {:code "push constant 32766",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "32766"]]]}
     {:code "push constant 32766",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "32766"]]]}
     {:code "gt", :parse-tree [:GT]}
     {:code "push constant 57",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "57"]]]}
     {:code "push constant 31",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "31"]]]}
     {:code "push constant 53",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "53"]]]}
     {:code "add", :parse-tree [:ADD]}
     {:code "push constant 112",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "112"]]]}
     {:code "sub", :parse-tree [:SUB]}
     {:code "neg", :parse-tree [:NEG]}
     {:code "and", :parse-tree [:AND]}
     {:code "push constant 82",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "82"]]]}
     {:code "or", :parse-tree [:OR]}
     {:code "not", :parse-tree [:NOT]}))
  )
