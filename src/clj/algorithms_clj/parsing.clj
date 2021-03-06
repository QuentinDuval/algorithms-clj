(ns algorithms-clj.parsing
  (:require [clojure.string :as str]))

;; Trying to build a parser combinator library, as you would do with a Monad in Haskell

(defprotocol Parser
  "Defining a protocol for what is a Parser in Clojure"
  (on-success [this result input] "Transmit the parsing result to the next parser in the chain")
  (on-failure [this] "Transmit the error to the next parser on the chain"))


(defn word-parser
  "Parses a word, until the next non-word character"
  [next-parser]
  (reify Parser
    (on-success [_ result input]
      (let [idx (str/index-of input " ")
            res (subs input 0 idx)
            next (subs input (inc idx))]
        (on-success
          next-parser (conj result res) next)               ;; Boiler plate - could be extracted by macro?
        ))
    (on-failure [_]
      (on-failure next-parser))                             ;; Boiler plate - could be extracted by macro?
    ))

(defn terminal
  []
  (reify Parser
    (on-success [_ result _] result)
    (on-failure [_] "Failed miserably")                     ;; TODO - Handling of error, like this is aweful
    ))

(defn parsing-test
  []
  (let [p (word-parser (terminal))]                         ;; TODO - A runParser method would be much nicer
    (println (on-success p [] "my first sentence"))
    ))


;; -------------------------------------------------------
;; Another attempt of design
;; -------------------------------------------------------

(defrecord ParseFailure [^String explanation])

(defrecord ParseInput [^String str ^int cursor])

(defrecord ParseSuccess [^ParseInput input result])

;; A parser is a function of type Input -> Failure | Success Input Result

(defprotocol ParseMonad
  (bind [mvalue mfunction] "The bind operator of Haskell"))

(defn run-parser [parser input]
  (parser input))

(extend-type ParseFailure
  ParseMonad
  (bind [mvalue _] mvalue))

(extend-type ParseSuccess
  ParseMonad
  (bind [{:keys [input result]} mfunction]
    (run-parser (mfunction result) input)
    ))

;; Trying the example out

(defn pure [a]
  (fn [input]
    (ParseSuccess. input a)
    ))

(defn parse-word
  [{:keys [str cursor]}]
  (let [next-cursor (str/index-of str " " cursor)]
    (ParseSuccess.
      (ParseInput. str (inc next-cursor))
      (subs str cursor next-cursor))
    ))

(defn mparsing-test []
  (bind
    (run-parser parse-word (ParseInput. "my first sentence" 0))
    (fn [lhs]
      (fn [input]
        (bind (parse-word input)
          (fn [rhs]
            (pure (str/join "," [lhs rhs]))
            ))
        ))
    ))



