;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Not for distribution.
;

(ns darwin.evolution.pareto
  "Functions for computing Pareto dominance, rank etc. Currently limited to two-objective
  comparison, but the API should be easy to extend to more.")

(defn dominates
  "Does i1 Pareto-dominate i2, as judged by the values associated with the given keys, k1 and k2.
  Lower scores are considered better."
  [[k1 k2] i1 i2]
  (or (and (<= (k1 i1) (k1 i2)) (< (k2 i1) (k2 i2)))
      (and (<= (k2 i1) (k2 i2)) (< (k1 i1) (k1 i2)))))

(defn dominated-set
  "Returns the individuals that i dominates wrt k1 and k2. Note that it doesn't return a set, rather a
  list (which I guess is a multiset) but `dominated-multiset` is too much of a mouthful."
  [[k1 k2] individuals i]
  (filter #(dominates [k1 k2] i %) individuals))

(defn dominator-set
  "Returns the individuals that dominate i wrt k1 and k2. As above, it doesn't return a set."
  [[k1 k2] individuals i]
  (filter #(dominates [k1 k2] % i) individuals))

(defn dominated-count
  "Count how many individuals i dominates wrt to k1 and k2."
  [[k1 k2] individuals i]
  (count (dominated-set [k1 k2] individuals i)))

(defn- individual-dominated?
  "Is an individual i dominated by any of the given individuals wrt the keys k1 and k2?"
  [[k1 k2] individuals i]
  (reduce #(or %1 %2) (map #(dominates [k1 k2] % i) individuals)))

(defn non-dominated-individuals
  "Returns the individuals that are non-dominated with respect to k1 and k2."
  [[k1 k2] individuals]
  (filter #(not (individual-dominated? [k1 k2] individuals %)) individuals))