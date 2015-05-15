;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Released under the MIT license..
;

(ns darwin.evolution.reproduction
  "The purpose of a reproduction step is to take a mating pool - a set of individuals that have
  somehow been selected from the population - and generate a new generation of the population.
  In the simplest case the mating pool is just the previous population, but in more complex
  algorithms it may also be made up from members of an archive etc.

  This implementation tracks the age of each individual. Individuals are represented by maps,
  which must have a :genotype key that contains the genetic material. The age will be tracked
  as an :age key on this map. It is permissible to store any other information you like on the
  individual maps, such as score information etc, but this will be destroyed in the reproduction
  step.")

(defn- unary-genotype-op-with-age-tracking
  "Applies an operation to the genotype of an individual, generating a new individual. The
  :age key of the individual is carried through and incremented."
  [op individual]
  (let [new-genotype (op (:genotype individual))
        new-age (inc (or (:age individual) 0))]
    {:genotype new-genotype :age new-age}))

(defn- binary-genotype-op-with-age-tracking
  "Applies an operation to the genotypes of two individuals, generating a pair of new individuals. The
  :age key of the new individual is the age of the eldest parent plus one."
  [op i1 i2]
  (let [new-genotypes (op (:genotype i1) (:genotype i2))
        new-age (inc (max (or (:age i1) 0) (or (:age i2) 0)))]
    [{:genotype (first new-genotypes) :age new-age}
     {:genotype (second new-genotypes) :age new-age}]))

(defn- apply-unary-operation
  "Takes a unary operation, the operation the requested number of times. Gathers all of the generated children
  into a list which it returns."
  [op reps pool selector]
  (repeatedly reps #(unary-genotype-op-with-age-tracking op (selector pool))))

(defn- apply-binary-operation
  "Takes a unary operation, the operation the requested number of times. Gathers all of the generated children
  into a list which it returns."
  [op reps pool selector]
  (reduce into []
          (repeatedly reps #(binary-genotype-op-with-age-tracking op (selector pool) (selector pool)))))

(defn reproduce
  "Generates a population from a mating pool. The config contains a selector function which will be used
  to pull individuals from the pool. The list of operations that will be applied are also in the config,
  in the keys :unary-ops for ops that act on one individual and :binary-ops that act on two individuals.
  Each operation is specified as a function :op, a :count of how many times to apply this operation. Unary
  operations are expected to return one individual, and binary operations a sequence of two individuals.
  It is up to the user to make sure that the total number of individuals returned gives the correct
  population size.

  The operations should be functions that operate directly on genetic material: this function will take
  care of extracting the genetic material from individuals and rebuilding new individuals after reproduction.
  During this process it will keep track of the age of each individual."
  [config pool]
  (let [{:keys [selector unary-ops binary-ops]} config
        unary-results (map #(apply-unary-operation (:op %) (:repeat %) pool selector) unary-ops)
        binary-results (map #(apply-binary-operation (:op %) (:repeat %) pool selector) binary-ops)]
    (doall (reduce into
                   [(reduce into [] unary-results)
                    (reduce into [] binary-results)]))))