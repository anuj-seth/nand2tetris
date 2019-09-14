(ns assembler.code
  (:require [clojure.string :as string]
            [assembler.util :as util]))

(defn set-values-to [ks value]
  (into {}
        (map #(vector % value)
             ks)))

(defn set-bits-to-one
  [bits-to-set]
  (set-values-to bits-to-set
                 1))

(defn set-bits-to-zero
  [bits-to-set]
  (set-values-to bits-to-set
                 0))

(defmulti to-bits first)
(defmethod to-bits :DEST
  [[_ reg-or-mem]]
  (let [dest-to-bits {[:REGISTER "D"] {:d2 1}
                      [:REGISTER "A"] {:d1 1}
                      [:MEMORY "M"]   {:d3 1}}]
    (dest-to-bits reg-or-mem)))

(defmethod to-bits :COMPUTE
  [[_ [alu-op]]]
  (let [op-to-bits {:ZERO     (set-bits-to-one [:c1 :c3 :c5])
                    :ONE      (set-bits-to-one [:c1 :c2 :c3 :c4 :c5 :c6])
                    :MINUSONE (set-bits-to-one [:c1 :c2 :c3 :c5])
                    :DREG     (set-bits-to-one [:c3 :c4])
                    :AREG     (set-bits-to-one [:c1 :c2])
                    :NOTDREG  (set-bits-to-one [:c3 :c4 :c6])
                    :NOTAREG  (set-bits-to-one [:c1 :c2 :c6])
                    :MINUSD   (set-bits-to-one [:c3 :c4 :c5 :c6])
                    :MINUSA   (set-bits-to-one [:c1 :c2 :c5 :c6])
                    :INCD     (set-bits-to-one [:c2 :c3 :c4 :c5 :c6])
                    :INCA     (set-bits-to-one [:c1 :c2 :c4 :c5 :c6])
                    :DECD     (set-bits-to-one [:c3 :c4 :c5])
                    :DECA     (set-bits-to-one [:c1 :c2 :c5])
                    :DPLUSA   (set-bits-to-one [:c5])
                    :DMINUSA  (set-bits-to-one [:c2 :c5 :c6])
                    :AMINUSD  (set-bits-to-one [:c4 :c5 :c6])
                    :DANDA    (set-bits-to-one [])
                    :DORA     (set-bits-to-one [:c2 :c4 :c6])
                    :M        (set-bits-to-one [:a :c1 :c2])
                    :NOTM     (set-bits-to-one [:a :c1 :c2 :c6])
                    :MINUSM   (set-bits-to-one [:a :c1 :c2 :c5 :c6])
                    :INCM     (set-bits-to-one [:a :c1 :c2 :c4 :c5 :c6])
                    :DECM     (set-bits-to-one [:a :c1 :c2 :c5])
                    :DPLUSM   (set-bits-to-one [:a :c5])
                    :DMINUSM  (set-bits-to-one [:a :c2 :c5 :c6])
                    :MMINUSD  (set-bits-to-one [:a :c4 :c5 :c6])
                    :DANDM    (set-bits-to-one [:a])
                    :DORM     (set-bits-to-one [:a :c2 :c4 :c6]) }] 
    (println "compute" alu-op)
    (op-to-bits alu-op)))

(defmethod to-bits :default
  [instruction]
  instruction)

(defmulti encode first)

(defmethod encode :COMMENT
  [_]
  nil)

(defmethod encode :EMPTYLINE
  [_]
  nil)

(defmethod encode :A-INSTRUCTION
  [[_ _ [tp value]]]
  (let [binary-digits (util/decimal-to-binary-n-digit (Integer/parseInt value)
                                                      15)]
    (str "0" binary-digits)))

;; the bits of a c-instruction are
;; 1 1 1 a c1 c2 c3 c4 c5 c6 d1 d2 d3 j1 j2 j3
(defmethod encode :C-INSTRUCTION
  [instruction]
  (let [bit-order [:op1 :op2 :op3 :a  :c1  :c2 :c3 :c4 :c5 :c6 :d1 :d2 :d3 :j1 :j2 :j3]
        default-bits (assoc (set-bits-to-zero bit-order)
                            :op1 1
                            :op2 1
                            :op3 1)
        actual-bits (map to-bits (rest instruction))
        instruction-bits (apply merge default-bits actual-bits)
        bits ((apply juxt bit-order) instruction-bits)]
    (string/join "" bits)))

(defmethod encode :default
  [instruction]
  instruction)

(comment 
  (encode [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:AREG "A"]]])

  (encode [:C-INSTRUCTION [:DEST [:REGISTER "D"]] [:COMPUTE [:DPLUSA "D+A"]]]))
