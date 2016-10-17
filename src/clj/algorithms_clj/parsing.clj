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
