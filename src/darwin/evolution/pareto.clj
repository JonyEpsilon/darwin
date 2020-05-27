;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Released under the MIT license..
;

(ns darwin.evolution.pareto
  "Functions for computing Pareto dominance, rank etc. Currently limited to two-objective
  comparison, but the API should be easy to extend to more.")

(defn dominates
  "Does i1 Pareto-dominate i2, as judged by the values associated with the given keys, k1 and k2.
  Lower scores are considered better."
  [ks i1 i2]
  (and (every? (fn [k]
                 (<= (k i1) (k i2)))
               ks)
       (some (fn [k]
               (< (k i1) (k i2)))
             ks)))

(defn dominated-set
  "Returns the individuals that i dominates wrt k1 and k2. Note that it doesn't return a set, rather a
  list (which I guess is a multiset) but `dominated-multiset` is too much of a mouthful."
  [ks individuals i]
  (filter #(dominates ks i %) individuals))

(defn dominator-set
  "Returns the individuals that dominate i wrt k1 and k2. As above, it doesn't return a set."
  [ks individuals i]
  (filter #(dominates ks % i) individuals))

(defn dominated-count
  "Count how many individuals i dominates wrt to k1 and k2."
  [ks individuals i]
  (count (dominated-set ks individuals i)))

(defn- individual-dominated?
  "Is an individual i dominated by any of the given individuals wrt the keys k1 and k2?"
  [ks individuals i]
  (reduce #(or %1 %2) (map #(dominates ks % i) individuals)))

(defn non-dominated-individuals
  "Returns the individuals that are non-dominated with respect to k1 and k2."
  [ks individuals]
  (filter #(not (individual-dominated? ks individuals %)) individuals))
