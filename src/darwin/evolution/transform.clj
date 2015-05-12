;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Not for distribution.
;

(ns darwin.evolution.transform
  "Functions for transforming individuals, and the population. These can be supplied to the EA
  to perform non-EA transformations before scoring (see `darwin.evolution.core` for details.
  This namespace contains general helper functions for constructing transformations, and generic
  transformations that are representation independent. A given representation might implement more
  specific transformations (think simplifying symbolic regression expressions, for instance)."
  (:refer-clojure :exclude [rand rand-nth rand-int])
  (:use [darwin.utility.random]))


;; * Helpers *

(defn apply-to-genotype
  [func individual]
  (assoc individual :genotype (func (:genotype individual))))

(defn apply-to-fraction-of-genotypes
  "Apply a transformation to a randomly selected fraction of the population. The transformation will
  be applied to the genotype of the individual. The rest of the information in the individual will be
  preserved."
  [transform fraction population]
  (map
    #(if (< (rand) fraction) ((partial apply-to-genotype transform) %) %)
    population))

(defn apply-to-all-genotypes
  "Apply a transformation to all of the population. The transformation will be applied to the genotype of
  the individual. The rest of the information in the individual will be preserved."
  [transform population]
  (map (partial apply-to-genotype transform) population))


;; * Generic transformations *

(defn hill-descent
  "Takes a genotype and applies the given tweak function, returns the new individual if it
  scores better on the score-function, otherwise returns the individual. The tweak function and
  the score function should operate on the genotype of the individual."
  [tweak-function score-function genotype]
  (let [tweaked (tweak-function genotype)
        score (score-function genotype)
        tweaked-score (score-function tweaked)]
    (if (< tweaked-score score)
      tweaked
      genotype)))

