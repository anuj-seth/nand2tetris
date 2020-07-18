(ns vm-translator.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [vm-translator.parser :as parser]
            [vm-translator.code :as code]
            [vm-translator.shared-state :as shared-state])
  (:gen-class))

(defn write-with-newline
  [writer line]
  (.write writer line)
  (.newLine writer))

(def debug true)

(def line-num (atom 0))

(defn- vm-files-in-directory
  [dir]
  (filter #(string/ends-with? % ".vm")
          (.list dir)))

(defn- parse-arg
  [arg]
  (let [f (io/file arg)]
    (cond
      (.isDirectory f) {:type :directory
                        :output-file (str arg "/" arg ".asm")
                        :vm-files (map #(str arg "/" %)
                                       (vm-files-in-directory f))}
      (.isFile f)      {:type :file
                        :output-file (string/replace arg #".vm" ".asm")
                        :vm-files [arg]}
      :else            (throw (ex-info "Argument is neither a file or a directory"
                                       {:arg arg})))))

(defn -main
  [input-arg]
  (let [{:keys [type output-file vm-files]} (parse-arg input-arg) 
        with-parse-tree                (map (fn [vm-file]
                                              {:vm-file vm-file
                                               :parsed (parser/parse vm-file)})
                                            vm-files)
        with-assembly-added            (mapv (fn [{:keys [vm-file parsed] :as data}]
                                               (shared-state/vm-file-name (string/replace vm-file
                                                                                          #"/"
                                                                                          "_"))
                                               (assoc data
                                                      :parsed
                                                      (filter :assembly
                                                              (code/add-assembly parsed))))
                                             with-parse-tree)
        bootstrap-instructions         [{:vm-file "generated_bootstrap.vm"
                                         :parsed (do
                                                   (shared-state/vm-file-name "generated_bootstrap.vm")
                                                   (code/add-bootstrap-assembly))}]
        end-loop-instructions          [{:vm-file "generated_end_loop.vm"
                                         :parsed (do
                                                   (shared-state/vm-file-name "generated_end_loop.vm")
                                                   (code/add-end-loop-assembly))}]
        all-instructions               (if (= type :directory)
                                         (concat bootstrap-instructions
                                                 with-assembly-added
                                                 end-loop-instructions)
                                         (concat with-assembly-added
                                                 end-loop-instructions))]
    (println output-file)
    (with-open [w (io/writer output-file)]
      (doseq [{:keys [vm-file parsed]} all-instructions]
        (doseq [{:keys [code assembly]} parsed]
          (.write w "// ")
          (write-with-newline w code)
          (doseq [hack-cmd assembly]
            ;;(println hack-cmd)
            (.write w hack-cmd)
            (when debug
              (let [label? (string/starts-with? hack-cmd
                                                "(")]
                (when (not label?)
                  (.write w (str "   // " @line-num))
                  (swap! line-num inc))))
            (.newLine w)))))))
