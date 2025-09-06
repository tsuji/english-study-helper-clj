(ns hst.ud-eval
  (:require
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]))

;; ===========
;; CoNLL-U パーサ（簡易）
;; ===========

(defn parse-feats [s]
  (if (or (nil? s) (= "_" s)) {}
      (->> (str/split s #"\|")
           (map #(str/split % #"=" 2))
           (map (fn [[k v]] [k v]))
           (into {}))))

(defn parse-deps [s]
  ;; ここでは未使用（簡易）。"head:deprel|"形式をとる可能性
  (if (or (nil? s) (= "_" s)) []
      (->> (str/split s #"\|")
           (map #(str/split % #":"))
           (map (fn [[h r]] {:head (parse-long h) :rel r}))
           (vec))))

(defn parse-conllu-sentences
  "Reader → ベクタ[{ :meta {...} :tokens [...] } ...]
   - 空行で文を区切る
   - `# key = value` を :meta に積む
   - 1-2（MWT）と 1.1（空ノード）はスキップ"
  [^java.io.Reader rdr]
  (let [lines (line-seq rdr)
        flush-sent (fn [{:keys [cur metas] :as st}]
                     (if (seq (:tokens cur))
                       (-> st
                           (update :acc conj {:meta metas :tokens (:tokens cur)})
                           (assoc :cur {:tokens []} :metas {}))
                       st))
        step (fn [{:keys [metas lineno] :as st} l]
               (let [ln (inc (long lineno))]
                 (cond
                   (str/blank? l)
                   (-> (flush-sent st) (assoc :lineno ln))

                   (str/starts-with? l "#")
                   (if-let [[_ k v] (re-matches #"#\s*([^=]+)\s*=\s*(.*)" l)]
                     (-> st
                         (assoc :lineno ln)
                         (assoc :metas (assoc metas (str/trim k) (str/trim v))))
                     (assoc st :lineno ln))

                   :else
                   (let [[id form lemma upos xpos feats head deprel deps misc]
                         (str/split l #"\t")]
                     (cond
                       (re-matches #"\d+-\d+" id) (assoc st :lineno ln) ; MWT
                       (re-matches #"\d+\.\d+" id) (assoc st :lineno ln) ; 空ノード
                       :else
                       (let [tok {:id    (parse-long id)
                                  :form  (some-> form str/lower-case)
                                  :lemma (when (and lemma (not= "_" lemma))
                                           (str/lower-case lemma))
                                  :upos  upos
                                  :xpos  xpos
                                  :feats (parse-feats feats)
                                  :head  (when (and head (not= "_" head)) (parse-long head))
                                  :deprel deprel
                                  :deps  (parse-deps deps)
                                  :misc  misc}]
                         (-> st
                             (assoc :lineno ln)
                             (update-in [:cur :tokens] conj tok))))))))]
    (let [final-state (reduce step {:acc [] :cur {:tokens []} :metas {} :lineno 0} lines)]
      (:acc (flush-sent final-state)))))

;; ===========
;; ルールDSL読み込み
;; ===========

(defn load-rules [f]
  (with-open [r (io/reader f)]
    (edn/read (java.io.PushbackReader. r))))

;; ===========
;; パターンマッチユーティリティ
;; ===========

(defn match-field? [tok field val]
  (let [v (get tok field)]
    (cond
      (and (string? val) (string? v)) (= v (str/lower-case val))
      (and (map? val) (map? v)) (every? (fn [[k vv]] (= (get v k) vv)) val)
      :else (= v val))))

(defn match-item? [tok itm]
  ;; itm: string（form一致） or {:lemma .. :upos .. :xpos .. :feats {...}} or {:re "..." :field :form}
  (cond
    (string? itm)
    (= (:form tok) (str/lower-case itm))

    (and (map? itm) (:re itm))
    (let [re (re-pattern (:re itm))
          field (or (:field itm) :form)
          v (get tok field)]
      (boolean (and v (re-matches re v))))

    (map? itm)
    (every? (fn [[k v]]
              (case k
                :feats (every? (fn [[fk fv]]
                                 (= (get-in tok [:feats fk]) fv)) v)
                (match-field? tok k v)))
            itm)))

(defn match-token-seq? [tokens pattern]
  (let [n (count pattern)]
    (loop [i 0]
      (when (<= (+ i n) (count tokens))
        (let [window (subvec (vec tokens) i (+ i n))]
          (if (every? true? (map match-item? window pattern))
            true
            (recur (inc i))))))))

(defn rule-hit? [rule sent]
  (let [tokens (:tokens sent)
        pos? (some (fn [sig]
                     (when (= (:type sig) :token-seq)
                       (match-token-seq? tokens (:pattern sig))))
                   (:signals rule))
        neg? (some (fn [nsig]
                     (when (= (:type nsig) :token-seq)
                       (match-token-seq? tokens (:pattern nsig))))
                   (:negative-signals rule))]
    (and pos? (not neg?))))

;; ===========
;; Gold 読み込み & 指標
;; ===========

(defn load-gold [f]
  ;; CSV: sentence_id,rule_id,label
  ;; sentence_id 例: train.conllu:42
  (with-open [r (io/reader f)]
    (->> (line-seq r)
         (remove #(or (str/blank? %) (str/starts-with? % "#")))
         (map #(str/split % #","))
         (map (fn [[sid rid lab]]
                {:sid sid :rid rid :label (= lab "1")}))
         (group-by (juxt :sid :rid)))))

(defn compute-metrics [tp fp fn]
  (let [prec (if (zero? (+ tp fp)) 0.0 (/ tp (+ tp fp)))
        rec  (if (zero? (+ tp fn)) 0.0 (/ tp (+ tp fn)))
        f1   (if (zero? (+ prec rec)) 0.0 (/ (* 2 prec rec) (+ prec rec)))]
    {:precision prec :recall rec :f1 f1
     :tp tp :fp fp :fn fn}))

;; ===========
;; 評価メイン
;; ===========

(def cli-opts
  [["-u" "--ud PATHS" "UD .conllu（カンマ区切り or グロブ不可なら複数指定）"]
   ["-r" "--rules FILE" "EDN ルールファイル（上記）"]
   ["-g" "--gold FILE" "gold_labels.csv（任意）"]
   ["-h" "--help"]])

(defn- file-name [f] (.getName (io/file f)))

(defn run-eval!
  [{:keys [ud rules gold]}]
  (let [rulez (load-rules rules)
        ud-files (map str/trim (str/split ud #","))
        gold* (when gold (load-gold gold))
        results
        (for [f ud-files
              :let [sid-prefix (file-name f)]
              :when (.exists (io/file f))]
          (with-open [r (io/reader f)]
            (let [sents (parse-conllu-sentences r)]
              {:file sid-prefix
               :hits
               (for [[idx sent] (map-indexed vector sents)
                     rule rulez
                     :let [sid (str sid-prefix ":" (inc idx))
                           hit (rule-hit? rule sent)
                           gold-key [(str sid) (:id rule)]
                           gold-label (when gold* (some-> (get gold* gold-key) first :label))]
                     :when (or hit gold-label)] ; goldが1でも拾って集計
                 {:sid sid
                  :rule (:id rule)
                  :hit hit
                  :gold gold-label})})))]
    ;; 集約
    (let [flat (mapcat :hits results)
          by-rule (group-by :rule flat)
          metrics
          (into {}
                (for [[rid xs] by-rule]
                  (let [tp (count (filter #(and (= true (:hit %))
                                                (= true (:gold %))) xs))
                        fp (count (filter #(and (= true (:hit %))
                                                (= false (:gold %))) xs))
                        fn (count (filter #(and (= false (:hit %))
                                                (= true (:gold %))) xs))]
                    [rid (compute-metrics tp fp fn)])))]
      ;; 出力（JSON）
      (println (json/generate-string
                {:rules (map (fn [r] {:id (:id r) :title (:title r)}) rulez)
                 :metrics metrics}
                {:pretty true})))))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-opts)]
    (cond
      (:help options)
      (do (println summary) (System/exit 0))

      (seq errors)
      (do (binding [*out* *err*] (doseq [e errors] (println e)))
          (System/exit 1))

      (not-every? options [:ud :rules])
      (do (binding [*out* *err*]
            (println "Required: --ud <file1.conllu,file2.conllu> --rules rules/en-grammar.edn [--gold data/gold_labels.csv]"))
          (System/exit 1))

      :else
      (run-eval! options))))
