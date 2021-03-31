(ns app.demo-ws
  (:require [com.fulcrologic.fulcro.components :as fp]
            [nubank.workspaces.core :as ws]
            [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
            [com.fulcrologic.fulcro.mutations :as fm]
            [com.fulcrologic.fulcro.dom :as dom]
            [sci.core :as sci]))

(fp/defsc FulcroDemo
  [this {:keys [counter]}]
  {:initial-state (fn [_] {:counter 0})
   :ident         (fn [] [::id "singleton"])
   :query         [:counter]}
  (dom/div
   (str "Fulcro counter demo [" counter "]")
   (dom/button {:onClick #(fm/set-value! this :counter (inc counter))} "+")))

(def ops #{`+ `- `/ `*})
(def precedent-ops #{`/ `*})

(defn last-op [stash]
  (first (filter symbol? stash)))

(defn clean-stash [stash]
  (cond (ops (first stash)) (rest stash)
        :else stash))

(defn stash->ast [stash]
  (let [s (-> stash clean-stash reverse)]
    (loop [[l op r & rest] s]
      (let [[next-op next-r & next-rest] rest]
        (cond
          (empty? rest) (if-not (nil? op) `(~op ~l ~r) l)

          (and
           (not (precedent-ops op))
           (precedent-ops next-op))
          (recur (cons `(~op ~l (~next-op ~r ~next-r)) next-rest))

          :else (recur (cons `(~op ~l ~r) rest)))))))

(defn exec-ast [ast]
  (sci/eval-form (sci/init nil) ast))

(def exec-stash (comp exec-ast stash->ast))

(defn print-stash [stash]
  (apply str (interpose " " (for [v (reverse stash)]
                               (if (symbol? v) (name v) v)))))

(defn set-op [stash op]
  (let [cleaned-stash (clean-stash stash)]
    (when-not (empty? cleaned-stash)
      (conj cleaned-stash op))))

(defn unset? [input]
  (or (nil? input) (= "" input) (symbol? input)))

(defn clean-input [input]
  (cond (unset? input) "0" 
        (clojure.string/ends-with? input ".") (subs input 0 (dec (count input)))
        :else input))

(fp/defsc Calculator
  [this {:keys [stash input]}]
  {:initial-state (fn [_] {:stash nil
                           :input nil})
   :ident         (fn [] [::id "calculator"])
   :query         [:stash :input]}
  (letfn [(apply-input [stash]
                       (let [num (js/parseFloat (clean-input input))]
                         (if-not (or (unset? input) (js/isNaN num))
                           (conj stash (js/parseFloat (clean-input input)))
                           stash)))
          (input-num [num]
                     #(do
                        (when (= (last-op stash) `=)
                          (fm/set-value!! this :stash nil))
                        (if (symbol? input)
                          (fm/set-value!! this :input (str num))
                          (fm/set-value!! this :input (-> (str input num)
                                                          (clojure.string/replace #"^0+" ""))))))
          (clear []
                 #(do
                    (fm/set-value!! this :stash nil)
                    (fm/set-value!! this :input nil)))
          (equals []
                  #(do
                     (let [cleaned-stash (->> stash
                                              (apply-input)
                                              (drop-while symbol?))
                           result (exec-stash cleaned-stash)]
                       (fm/set-value!! this :stash (conj cleaned-stash `= result))
                       (fm/set-value!! this :input (str result)))))
          (decimal []
                   #(when-not (and (string? input) (clojure.string/includes? input "."))
                      (fm/set-value!! this :input (str (clean-input input) "."))))
          (minus []
                 #(if (and
                       (or (unset? input) (= "-" input))
                       (= (last-op stash) (first stash)))
                    (fm/set-value!! this :input (if (= "-" input) "" "-"))
                    ((operation `-))))
          (operation [op]
                     #(let [stash (cond
                                    (symbol? input) (clean-stash stash)
                                    (= `= (last-op stash)) '()
                                    :else stash)]
                        (fm/set-value!! this :stash (-> stash (apply-input) (set-op op)))
                        (fm/set-value!! this :input op)))]
      (dom/div :.calc
               (dom/div :#displays
                        (dom/div :#formula (print-stash stash))
                        (dom/div :#display (str (cond
                                                  (unset? input) "0"
                                                  (string? input) input
                                                  (symbol? input) (name input)
                                                  :else "0"))))
               (dom/div :.calc-btn#zero {:onClick (input-num 0)} "0")
               (dom/div :.numbers
                        (map (fn [num name]
                               (dom/div :.calc-btn {:id name :key name :onClick (input-num num)} num))
                             (range 1 10)
                             ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]))
               (dom/div :.calc-btn#add {:onClick (operation `+)} "+")
               (dom/div :.calc-btn#subtract {:onClick (minus)} "-")
               (dom/div :.calc-btn#multiply {:onClick (operation `*)} "*")
               (dom/div :.calc-btn#divide {:onClick (operation `/)} "/")
               (dom/div :.calc-btn#decimal {:onClick (decimal)} ".")
               (dom/div :.calc-btn#equals {:onClick (equals)} "=")
               (dom/div :.calc-btn#clear {:onClick (clear)} "AC"))))

(ws/defcard fulcro-calculator
  (ct.fulcro/fulcro-card
    {::ct.fulcro/root       Calculator
     ::ct.fulcro/wrap-root? true}))


(ws/defcard fulcro-demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/root       FulcroDemo
     ::ct.fulcro/wrap-root? true}))
