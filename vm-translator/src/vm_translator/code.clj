(ns vm-translator.code
  (:require [vm-translator.shared-state :as shared-state]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))


(def labels-stack (atom []))


(def jump-label-counter (atom 0))

(def segments (atom nil))

(defmulti assembly (fn [instruction & more]
                     instruction))

(defmethod assembly :end
  [_]
  (let [label-prefix (shared-state/vm-file-name)
        label (format "%s$%s" label-prefix "END")]
    [(format "(%s)" label)
     (str "@" label)
     "0;JMP"]))

(defmethod assembly :move-sp-to-a
  [_]
  ["A=M"])

(defmethod assembly :move-a-to-sp-target
  [_]
  ["D=A" "@SP" "A=M" "M=D"])

(defmethod assembly :move-a-target-to-sp-target
  [_]
  ["D=M" "@SP" "A=M" "M=D"])

(defmethod assembly :move-d-to-sp-target
  [_]
  ["@SP" "A=M" "M=D"])

(defmethod assembly :move-sp-target-to-d
  [_]
  ["@SP" "A=M" "D=M"])

(defmethod assembly :move-sp-target-to-a
  [_]
  ["@SP" "A=M" "A=M"])

(defmethod assembly :inc-sp
  [_]
  ["@SP" "M=M+1"])

(defmethod assembly :dec-sp
  [_]
  ["@SP" "M=M-1"])

(defmethod assembly :indirectly-addressed-segment
  [_ segment idx]
  [(str "@"
        ((first @segments) segment))
   "D=M"
   (str "@" idx)
   "D=D+A"
   "A=D"])

(defmethod assembly :directly-addressed-segment
  [_ segment idx]
  [(str "@"
        ((first @segments) segment))
   "D=A"
   (str "@" idx)
   "D=D+A"
   "A=D"])

(defmethod assembly :static-segment
  [_ segment idx]
  [(format "@%s.%s"
           ((first @segments) segment)
           idx)])

(defmethod assembly :move-a-to-r13
  [_]
  ["D=A" "@R13" "M=D"])

(defmethod assembly :move-d-to-r13-target
  [_]
  ["@R13" "A=M" "M=D"])

(defn binary-op
  [operation]
  (let [op ({:ADD "+"
             :SUB "-"
             :AND "&"
             :OR  "|"} operation)]
    (concat (assembly :dec-sp)
            (assembly :move-sp-target-to-d)
            (assembly :dec-sp)
            (assembly :move-sp-to-a)
            [(format "M=M%sD" op)]
            (assembly :inc-sp))))


(defn unary-op
  [operation]
  (let [op ({:NOT "!"
             :NEG "-"} operation)]
    (concat (assembly :dec-sp)
            (assembly :move-sp-target-to-d)
            [(format "D=%sD" op)]
            (assembly :move-d-to-sp-target)
            (assembly :inc-sp))))

(defn comparison-op
  [comparison-type]
  (let [counter (swap! jump-label-counter inc)
        label-prefix "X" ;;(last @labels-stack)
        op-data {:LT {:label-name (str label-prefix "_LT_" counter)
                      :op "JLT"}
                 :GT {:label-name (str label-prefix "_GT_" counter)
                      :op "JGT"}
                 :EQ {:label-name (str label-prefix "_EQ_" counter)
                      :op "JEQ"}}
        {:keys [label-name op]} (op-data comparison-type)]
    (concat (assembly :dec-sp)
            (assembly :move-sp-target-to-d)
            (assembly :dec-sp)
            (assembly :move-sp-target-to-a)
            ["D=A-D"
             (format "@%s"
                     label-name)
             (format "D;%s"
                     op)
             "D=0"
             (format  "@%s_D_TO_SP_%d"
                      label-prefix
                      counter)
             "0;JMP"
             (format "(%s)"
                     label-name)
             "D=-1"
             (format "(%s_D_TO_SP_%d)"
                     label-prefix
                     counter)]
            (assembly :move-d-to-sp-target)
            (assembly :inc-sp))))

(derive :MEM-SEGMENT/TEMP :MEM-SEGMENT/DIRECTLY-ADDRESSED)
(derive :MEM-SEGMENT/POINTER :MEM-SEGMENT/DIRECTLY-ADDRESSED)
(derive :MEM-SEGMENT/LOCAL :MEM-SEGMENT/INDIRECTLY-ADDRESSED)
(derive :MEM-SEGMENT/ARGUMENT :MEM-SEGMENT/INDIRECTLY-ADDRESSED)
(derive :MEM-SEGMENT/THIS :MEM-SEGMENT/INDIRECTLY-ADDRESSED)
(derive :MEM-SEGMENT/THAT :MEM-SEGMENT/INDIRECTLY-ADDRESSED)
(defmulti stack-cmd (fn [push-or-pop [segment idx]] [push-or-pop segment]))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/CONSTANT]
  [_ [_ idx]]
  (concat [(str "@" idx)]
          (assembly :move-a-to-sp-target)
          (assembly :inc-sp)))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/STATIC]
  [_ [segment idx]]
  (concat (assembly :static-segment
                    segment
                    idx)
          (assembly :move-a-target-to-sp-target)
          (assembly :inc-sp)))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/DIRECTLY-ADDRESSED]
  [_ [segment idx]]
  (concat (assembly :directly-addressed-segment
                    segment
                    idx)
          (assembly :move-a-target-to-sp-target)
          (assembly :inc-sp)))

(defmethod stack-cmd [:PUSH :MEM-SEGMENT/INDIRECTLY-ADDRESSED]
  [_ [segment idx]]
  (concat (assembly :indirectly-addressed-segment
                    segment
                    idx)
          (assembly :move-a-target-to-sp-target)
          (assembly :inc-sp)))

(defmethod stack-cmd [:POP :MEM-SEGMENT/DIRECTLY-ADDRESSED]
  [_ [segment idx]]
  (concat (assembly :directly-addressed-segment
                    segment
                    idx)
          (assembly :move-a-to-r13)
          (assembly :dec-sp)
          (assembly :move-sp-target-to-d)
          (assembly :move-d-to-r13-target)))

(defmethod stack-cmd [:POP :MEM-SEGMENT/STATIC]
  [_ [segment idx]]
  (concat (assembly :static-segment
                    segment
                    idx)
          (assembly :move-a-to-r13)
          (assembly :dec-sp)
          (assembly :move-sp-target-to-d)
          (assembly :move-d-to-r13-target)))

(defmethod stack-cmd [:POP :MEM-SEGMENT/INDIRECTLY-ADDRESSED]
  [_ [segment idx]]
  (concat (assembly :indirectly-addressed-segment
                    segment
                    idx)
          (assembly :move-a-to-r13)
          (assembly :dec-sp)
          (assembly :move-sp-target-to-d)
          (assembly :move-d-to-r13-target)))

(defmethod stack-cmd :default
  [push-or-pop [segment _]]
  (throw (ex-info "No matching stack-cmd"
                  {:op push-or-pop
                   :segment segment})))

(defn if-goto
  [label-name]
  (concat (assembly :dec-sp)
          (assembly :move-sp-target-to-d)
          (let [context "X" ;;(last @labels-stack)
                ]
            [(format "@%s$%s" context label-name)
             "D;JNE"])))

(defn goto
  [label-name]
  (let [context "X" ;;(last @labels-stack)
        ]
    [(format "@%s$%s" context label-name)
     "0;JMP"]))

(defn function-call
  [fn-name number-of-args]
  (let [label-counter          (swap! jump-label-counter inc)
        return-address         (format "RETURN_FROM_%s_%d"
                                       fn-name
                                       label-counter)
        push-return-address    (stack-cmd :PUSH
                                          [:MEM-SEGMENT/CONSTANT return-address])
        push-LCL-ARG-THIS-THAT (apply concat
                                      (for [register ["LCL" "ARG" "THIS" "THAT"]]
                                        (concat [(format "@%s" register)
                                                 "D=M"]
                                                (assembly :move-d-to-sp-target)
                                                (assembly :inc-sp))))
        reposition-ARG         (let [go-back-by (+ 5 number-of-args)]
                                 ["@SP"
                                  "D=M"
                                  (format "@%d" go-back-by)
                                  "D=D-A"
                                  "@ARG"
                                  "M=D"])
        reposition-LCL         ["@SP" "D=M" "@LCL" "M=D"]
        jump-to-function       [(format "@%s" fn-name)
                                "0;JMP"]
        insert-return-address  [(format "(%s)"
                                        return-address)]]
    (concat push-return-address
            push-LCL-ARG-THIS-THAT
            reposition-ARG
            reposition-LCL
            jump-to-function
            insert-return-address)))

(defn function-return
  [_]
  (swap! labels-stack pop)
  ;; get return address
  (concat ["@LCL" "D=M" "@5" "D=D-A" "A=D" "D=M" "@R13" "M=D"]
          (assembly :dec-sp)
          (assembly :move-sp-target-to-d)
          ;; move D to ARG
          ["@ARG" "A=M" "M=D"]
          ;; restore SP
          ["@ARG" "A=M" "A=A+1" "D=A" "@SP" "M=D"]
          ;; restore THAT
          ["@LCL" "D=M" "@1" "D=D-A" "A=D" "D=M" "@THAT" "M=D"]
          ;; restore THIS
          ["@LCL" "D=M" "@2" "D=D-A" "A=D" "D=M" "@THIS" "M=D"]
          ;; restore ARG
          ["@LCL" "D=M" "@3" "D=D-A" "A=D" "D=M" "@ARG" "M=D"]
          ;; restore LCL
          ["@LCL" "D=M" "@4" "D=D-A" "A=D" "D=M" "@LCL" "M=D"]
          ;; goto RET
          ["@R13" "A=M" "0;JMP"]))

(defn encode
  [{:keys [parse-tree] :as data}]
  (assoc data
         :assembly
         (insta/transform {:END                 #(assembly :end)
                           :COMMENT             (constantly nil)
                           :EMPTYLINE           (constantly nil)
                           :INDEX               identity
                           :LABEL               (fn [label]
                                                  (let [context "X" ;;(last @labels-stack)
                                                        ]
                                                    [(format "(%s$%s)" context label)]))
                           :IF-GOTO             if-goto
                           :GOTO                goto
                           :SEGMENT             (fn [[segment] idx]
                                                  [(keyword "MEM-SEGMENT"
                                                            (name segment))
                                                   idx]) 
                           :PUSH                (constantly :PUSH)
                           :POP                 (constantly :POP)
                           :STACK-CMD           stack-cmd
                           :FUNCTION-CALL       function-call
                           :NUMBER-OF-ARGS      #(Integer/parseInt %)
                           :FUNCTION-DEFINITION (fn [fn-name push-args]
                                                  (cons (format "(%s)" fn-name)
                                                        push-args))
                           :FUNCTION-NAME       (fn [fn-name]
                                                  (swap! labels-stack conj fn-name)
                                                  fn-name)

                           :NUMBER-OF-LOCAL-VARS (fn [num-args]
                                                   (let [zeros-to-push (Integer/parseInt num-args)]
                                                     (flatten (take zeros-to-push
                                                                    (repeatedly #(stack-cmd
                                                                                  :PUSH
                                                                                  [:MEM-SEGMENT/CONSTANT 0]))))))
                           :FUNCTION-RETURN     function-return
                           :ADD                 #(binary-op :ADD)
                           :SUB                 #(binary-op :SUB)
                           :AND                 #(binary-op :AND)
                           :OR                  #(binary-op :OR)
                           :EQ                  #(comparison-op :EQ)
                           :LT                  #(comparison-op :LT)
                           :GT                  #(comparison-op :GT)
                           :NEG                 #(unary-op :NEG)
                           :NOT                 #(unary-op :NOT)}
                          parse-tree)))

(defn add-bootstrap-assembly
  []
  [{:code "SP=256"
    :parse-tree nil
    :assembly ["@256" "D=A" "@SP" "M=D"]}
   (encode {:code "call Sys.init 0",
            :parse-tree
            [:FUNCTION-CALL [:FUNCTION-NAME "Sys.init"] [:NUMBER-OF-ARGS "0"]]})])

(defn add-end-loop-assembly
  []
  [(encode {:code "END loop"
            :parse-tree [:END]})])

(defn add-assembly
  [parse-trees]
  (swap! labels-stack
         conj
         (shared-state/vm-file-name))
  ;;(reset! jump-label-counter 0)
  (reset! segments [{:MEM-SEGMENT/TEMP     "5"
                     :MEM-SEGMENT/STATIC   (shared-state/vm-file-name)
                     :MEM-SEGMENT/POINTER  "THIS"
                     :MEM-SEGMENT/LOCAL    "LCL"
                     :MEM-SEGMENT/ARGUMENT "ARG"
                     :MEM-SEGMENT/THIS     "THIS"
                     :MEM-SEGMENT/THAT     "THAT"}])
  (doall
   (map encode parse-trees)))


(comment

  (add-assembly
   '({:code "call Sys.main 5",
      :parse-tree
      [:FUNCTION-CALL [:FUNCTION-NAME "Sys.main"] [:NUMBER-OF-ARGS "5"]]}
     {:code "function Sys.add12 0",
      :parse-tree
      [:FUNCTION-DEFINITION
       [:FUNCTION-NAME "Sys.add12"]
       [:NUMBER-OF-LOCAL-VARS "0"]]}
     {:code "function Sys.main 5",
      :parse-tree
      [:FUNCTION-DEFINITION
       [:FUNCTION-NAME "Sys.main"]
       [:NUMBER-OF-LOCAL-VARS "5"]]}))

  (add-assembly
   '({:code "// Sys.vm for NestedCall test.",
      :parse-tree
      [:COMMENT [:COMMENT-TEXT " Sys.vm for NestedCall test."]]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "// Sys.init()",
      :parse-tree [:COMMENT [:COMMENT-TEXT " Sys.init()"]]}
     {:code "//", :parse-tree [:COMMENT [:COMMENT-TEXT ""]]}
     {:code "// Calls Sys.main() and stores return value in temp 1.",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " Calls Sys.main() and stores return value in temp 1."]]}
     {:code "// Does not return.  (Enters infinite loop.)",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT " Does not return.  (Enters infinite loop.)"]]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "function Sys.init 0",
      :parse-tree
      [:FUNCTION-DEFINITION
       [:FUNCTION-NAME "Sys.init"]
       [:NUMBER-OF-LOCAL-VARS "0"]]}
     {:code "push constant 4000\t// test THIS and THAT context save",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "4000"]]]}
     {:code "pop pointer 0",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:POINTER] [:INDEX "0"]]]}
     {:code "push constant 5000",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "5000"]]]}
     {:code "pop pointer 1",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:POINTER] [:INDEX "1"]]]}
     {:code "call Sys.main 0",
      :parse-tree
      [:FUNCTION-CALL [:FUNCTION-NAME "Sys.main"] [:NUMBER-OF-ARGS "0"]]}
     {:code "pop temp 1",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:TEMP] [:INDEX "1"]]]}
     {:code "label LOOP", :parse-tree [:LABEL "LOOP"]}
     {:code "goto LOOP", :parse-tree [:GOTO "LOOP"]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "// Sys.main()",
      :parse-tree [:COMMENT [:COMMENT-TEXT " Sys.main()"]]}
     {:code "//", :parse-tree [:COMMENT [:COMMENT-TEXT ""]]}
     {:code
      "// Sets locals 1, 2 and 3, leaving locals 0 and 4 unchanged to test",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " Sets locals 1, 2 and 3, leaving locals 0 and 4 unchanged to test"]]}
     {:code
      "// default local initialization to 0.  (RAM set to -1 by test setup.)",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " default local initialization to 0.  (RAM set to -1 by test setup.)"]]}
     {:code
      "// Calls Sys.add12(123) and stores return value (135) in temp 0.",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " Calls Sys.add12(123) and stores return value (135) in temp 0."]]}
     {:code
      "// Returns local 0 + local 1 + local 2 + local 3 + local 4 (456) to confirm",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT
        " Returns local 0 + local 1 + local 2 + local 3 + local 4 (456) to confirm"]]}
     {:code "// that locals were not mangled by function call.",
      :parse-tree
      [:COMMENT
       [:COMMENT-TEXT " that locals were not mangled by function call."]]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "function Sys.main 5",
      :parse-tree
      [:FUNCTION-DEFINITION
       [:FUNCTION-NAME "Sys.main"]
       [:NUMBER-OF-LOCAL-VARS "5"]]}
     {:code "push constant 4001",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "4001"]]]}
     {:code "pop pointer 0",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:POINTER] [:INDEX "0"]]]}
     {:code "push constant 5001",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "5001"]]]}
     {:code "pop pointer 1",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:POINTER] [:INDEX "1"]]]}
     {:code "push constant 200",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "200"]]]}
     {:code "pop local 1",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:LOCAL] [:INDEX "1"]]]}
     {:code "push constant 40",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "40"]]]}
     {:code "pop local 2",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:LOCAL] [:INDEX "2"]]]}
     {:code "push constant 6",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "6"]]]}
     {:code "pop local 3",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:LOCAL] [:INDEX "3"]]]}
     {:code "push constant 123",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "123"]]]}
     {:code "call Sys.add12 1",
      :parse-tree
      [:FUNCTION-CALL [:FUNCTION-NAME "Sys.add12"] [:NUMBER-OF-ARGS "1"]]}
     {:code "pop temp 0",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:TEMP] [:INDEX "0"]]]}
     {:code "push local 0",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "0"]]]}
     {:code "push local 1",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "1"]]]}
     {:code "push local 2",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "2"]]]}
     {:code "push local 3",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "3"]]]}
     {:code "push local 4",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:LOCAL] [:INDEX "4"]]]}
     {:code "add", :parse-tree [:ADD]}
     {:code "add", :parse-tree [:ADD]}
     {:code "add", :parse-tree [:ADD]}
     {:code "add", :parse-tree [:ADD]}
     {:code "return", :parse-tree [:FUNCTION-RETURN "return"]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "// Sys.add12(int n)",
      :parse-tree [:COMMENT [:COMMENT-TEXT " Sys.add12(int n)"]]}
     {:code "//", :parse-tree [:COMMENT [:COMMENT-TEXT ""]]}
     {:code "// Returns n+12.",
      :parse-tree [:COMMENT [:COMMENT-TEXT " Returns n+12."]]}
     {:code "", :parse-tree [:EMPTYLINE]}
     {:code "function Sys.add12 0",
      :parse-tree
      [:FUNCTION-DEFINITION
       [:FUNCTION-NAME "Sys.add12"]
       [:NUMBER-OF-LOCAL-VARS "0"]]}
     {:code "push constant 4002",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "4002"]]]}
     {:code "pop pointer 0",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:POINTER] [:INDEX "0"]]]}
     {:code "push constant 5002",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "5002"]]]}
     {:code "pop pointer 1",
      :parse-tree [:STACK-CMD [:POP] [:SEGMENT [:POINTER] [:INDEX "1"]]]}
     {:code "push argument 0",
      :parse-tree [:STACK-CMD [:PUSH] [:SEGMENT [:ARGUMENT] [:INDEX "0"]]]}
     {:code "push constant 12",
      :parse-tree
      [:STACK-CMD [:PUSH] [:SEGMENT [:CONSTANT] [:INDEX "12"]]]}
     {:code "add", :parse-tree [:ADD]}
     {:code "return", :parse-tree [:FUNCTION-RETURN "return"]}))

  )



 















