;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Released under the MIT license..
;

(ns darwin.core
  (:require [darwin.algorithms.sso :as sso]
            [darwin.evolution.core :as evolution]))

(defn evolve
  "A simple interface to get started. Runs an evolutionary optimisation of a single objective with a single mutation
  and single crossover function. The score function should take an individual and return a numeric score. Lower is taken
  to be better. The mutation function should return a single mutated individual. The crossover function should return a
  vector of two individuals. The function `random-individual` should return a single random individual and will be used
  to intialise the population. The evolution will be run for max-generations generations."
  [score crossover mutate generate-random-individual max-generations]
  (let [ea-config (sso/sso-ea-config {:unary-ops       [{:op mutate :repeat 5} {:op identity :repeat 45}]
                                      :binary-ops      [{:op crossover :repeat 25}]
                                      :tournament-size 4
                                      :goal            :score})
        config {:ea-config          ea-config
                :score-functions    {:score score}
                :reporting-function (fn [z] (when (= (mod (:age z) 10) 0) (print ".") (flush)))}
        initial-zg (evolution/make-zeitgeist (repeatedly 100 generate-random-individual))]
    (evolution/run-evolution config initial-zg (fn [zg gc] (>= (:age zg) max-generations)))))