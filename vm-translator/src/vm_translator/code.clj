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
                         :MEM-SEGMENT/TEMP "5"
                         :MEM-SEGMENT/LOCAL    "LCL"
                         :MEM-SEGMENT/ARGUMENT "ARG"
                         :MEM-SEGMENT/THIS     "THIS"
                         :MEM-SEGMENT/THAT     "THAT"})

;; (defmulti segment-encoder (fn [[segment] _] segment))

;; (defmethod segment-encoder :CONSTANT
;;   [_ index]
;;   {:type :CONSTANT
;;    :assembly [(str "@" index)]})

;; (defmethod segment-encoder :TEMP
;;   [_ index]
;;   {:type :TEMP
;;    :assembly ["@5"
;;               "D=A"
;;               (str  "@" index)
;;               "A=D+A"]})

;; (defmethod segment-encoder :default
;;   [[segment] index]
;;   (let [register {:LOCAL    "LCL"
;;                   :ARGUMENT "ARG"
;;                   :THIS     "THIS"
;;                   :THAT     "THAT"}]
;;     [(str "@"
;;           (register segment))
;;      "D=M"
;;      (str "@" index)
;;      "D=D+A"
;;      "A=D"]))

(def assembly {:end (fn []
                      ["(END)" "@END" "0;JMP"])
               :move-sp-to-a (fn []
                               ["A=M"])
               :move-a-to-sp-target (fn []
                                      ["D=A" "@SP" "A=M" "M=D"])
               :move-a-target-to-sp-target (fn []
                                             ["D=M" "@SP" "A=M" "M=D"])
               :move-d-to-sp-target (fn []
                                      ["@SP" "A=M" "M=D"])
               :move-sp-target-to-d (fn []
                                      ["@SP" "A=M" "D=M"])
               :move-sp-target-to-a (fn []
                                      ["@SP" "A=M" "A=M"])
               :inc-sp (fn []
                         ["@SP" "M=M+1"])
               :dec-sp (fn []
                         ["@SP" "M=M-1"])
               :calculate-index-into-physical-segment (fn [segment idx]
                                                        [(str "@"
                                                              (predefined-symbols segment))
                                                         "D=M"
                                                         (str "@" idx)
                                                         "D=D+A"
                                                         "A=D"])
               :calculate-index-into-temp-segment (fn [idx]
                                                    ["@5"
                                                     "D=A"
                                                     (str "@" idx)
                                                     "D=D+A"
                                                     "A=D"])
               :move-a-to-r13 (fn []
                                ["D=A" "@R13" "M=D"])
               :move-d-to-r13-target (fn []
                                       ["@R13" "A=M" "M=D"])})

(defn binary-op
  [operation]
  (let [op ({:ADD "+"
             :SUB "-"
             :AND "&"
             :OR  "|"} operation)]
    (mapcat #(%)
            [(:dec-sp assembly)
             (:move-sp-target-to-d assembly)
             (:dec-sp assembly)
             (:move-sp-to-a assembly)
             (fn []
               [(str "M=M" op "D")])
             (:inc-sp assembly)])))

(defn unary-op
  [operation]
  (let [op ({:NOT "!"
             :NEG "-"} operation)]
    (mapcat #(%)
            [(:dec-sp assembly)
             (:move-sp-target-to-d assembly)
             (fn []
               [(str "D=" op "D")])
             (:move-d-to-sp-target assembly)
             (:inc-sp assembly)])))

(def label-counter (atom 0))

(defn comparison-op
  [comparison-type]
  (let [counter (swap! label-counter inc)
        op-data {:LT {:label-name (str "LT_" counter) :op "JLT"}
                 :GT {:label-name (str "GT_" counter) :op "JGT"}
                 :EQ {:label-name (str "EQ_" counter) :op "JEQ"}}
        {:keys [label-name op]} (op-data comparison-type)]
    (mapcat #(%)
            [(:dec-sp assembly)
             (:move-sp-target-to-d assembly)
             (:dec-sp assembly)
             (:move-sp-target-to-a assembly)
             (fn []
               ["D=A-D"
                (str "@" label-name)
                (str "D;" op)
                "D=0"
                (str "@D_TO_SP_" counter)
                "0;JMP"
                (str "(" label-name ")")
                "D=-1"
                (str  "(D_TO_SP_" counter ")")])
             (:move-d-to-sp-target assembly)
             (:inc-sp assembly)])))

(derive :MEM-SEGMENT/LOCAL :MEM-SEGMENT/PHYSICAL)
(derive :MEM-SEGMENT/ARGUMENT :MEM-SEGMENT/PHYSICAL)
(derive :MEM-SEGMENT/THIS :MEM-SEGMENT/PHYSICAL)
(derive :MEM-SEGMENT/THAT :MEM-SEGMENT/PHYSICAL)
(defmulti stack-cmd (fn [push-or-pop [segment idx]] [push-or-pop segment]))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/CONSTANT]
  [_ [_ idx]]
  (mapcat #(%)
          [(fn []
             [(str "@" idx)])
           (:move-a-to-sp-target assembly)
           (:inc-sp assembly)]))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/TEMP]
  [_ [_ idx]]
  (mapcat #(%)
          [(partial (:calculate-index-into-temp-segment assembly)
                    idx)
           (:move-a-target-to-sp-target assembly)
           (:inc-sp assembly)]))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/PHYSICAL]
  [_ [segment idx]]
  (mapcat #(%)
          [(partial (:calculate-index-into-physical-segment assembly)
                    segment
                    idx)
           (:move-a-target-to-sp-target assembly)
           (:inc-sp assembly)]))

(defmethod stack-cmd [:POP :MEM-SEGMENT/TEMP]
  [_ [_ idx]]
  (mapcat #(%)
          [(partial (:calculate-index-into-temp-segment assembly)
                    idx)
           (:move-a-to-r13 assembly)
           (:dec-sp assembly)
           (:move-sp-target-to-d assembly)
           (:move-d-to-r13-target assembly)]))

(defmethod stack-cmd [:POP :MEM-SEGMENT/PHYSICAL]
  [_ [segment idx]]
  (mapcat #(%)
          [(partial (:calculate-index-into-physical-segment assembly)
                    segment
                    idx)
           (:move-a-to-r13 assembly)
           (:dec-sp assembly)
           (:move-sp-target-to-d assembly)
           (:move-d-to-r13-target assembly)]))

(defmethod stack-cmd :default
  [push-or-pop [segment _]]
  (throw (ex-info "No matching stack-cmd"
                  {:op push-or-pop
                   :segment segment})))

(defn encode
  [{:keys [parse-tree] :as data}]
  (assoc data
         :assembly
         (insta/transform {:END       (:end assembly)
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
  
  (add-assembly
   '({:code "// This file is part of www.nand2tetris.org",
      :parse-tree [:COMMENT [:COMMENT-TEXT " This file is part of www.nand2tetris.org"]]}
     {:code "// and the book \"The Elements of Computing Systems\"",
      :parse-tree [:COMMENT [:COMMENT-TEXT " and the book \"The Elements of Computing Systems\""]]}
     {:code "// by Nisan and Schocken, MIT Press.",
      :parse-tree [:COMMENT [:COMMENT-TEXT " by Nisan and Schocken, MIT Press."]]}
     {:code "// File name: projects/07/MemoryAccess/BasicTest/BasicTest.vm",
      :parse-tree [:COMMENT [:COMMENT-TEXT " File name: projects/07/MemoryAccess/BasicTest/BasicTest.vm"]]}
     {:code "",
      :parse-tree [:EMPTYLINE]}
     {:code "// Executes pop and push commands using the virtual memory segments.",
      :parse-tree [:COMMENT [:COMMENT-TEXT " Executes pop and push commands using the virtual memory segments."]]}
     {:code "push constant 10",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "10"]]]}
     {:code "pop local 0",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:LOCAL] [:INDEX "0"]]]}
     {:code "push constant 21",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "21"]]]}
     {:code "push constant 22",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "22"]]]}
     {:code "pop argument 2",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:ARGUMENT] [:INDEX "2"]]]}
     {:code "pop argument 1",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:ARGUMENT] [:INDEX "1"]]]}
     {:code "push constant 36",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "36"]]]}
     {:code "pop this 6",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:THIS] [:INDEX "6"]]]}
     {:code "push constant 42",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "42"]]]}
     {:code "push constant 45",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "45"]]]}
     {:code "pop that 5",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:THAT] [:INDEX "5"]]]}
     {:code "pop that 2",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:THAT] [:INDEX "2"]]]}
     {:code "push constant 510",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "510"]]]}
     {:code "pop temp 6",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:TEMP] [:INDEX "6"]]]}
     {:code "push local 0",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "0"]]]}
     {:code "push that 5",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:THAT] [:INDEX "5"]]]}
     {:code "add",
      :parse-tree [:ADD]}
     {:code "push argument 1",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:ARGUMENT] [:INDEX "1"]]]}
     {:code "sub",
      :parse-tree [:SUB]}
     {:code "push this 6",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:THIS] [:INDEX "6"]]]}
     {:code "push this 6",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:THIS] [:INDEX "6"]]]}
     {:code "add", :parse-tree [:ADD]}
     {:code "sub",
      :parse-tree [:SUB]}
     {:code "push temp 6",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:TEMP] [:INDEX "6"]]]}
     {:code "add", :parse-tree [:ADD]}))

  )




