;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Released under the MIT license..
;

(ns darwin.evolution.metrics
  "Functions for capturing metrics for the run.")

(def metrics (atom {}))

(defn clear!
  "Reset the metrics."
  []
  (reset! metrics {}))

(defn add!
  [key value]
  (swap! metrics #(update-in % [key] (fn [x] (apply vector (conj x value))))))

(defn- calculate-stats
  "Update a single population-level metric."
  [values]
  (let [mean-val (double (/ (apply + values) (count values)))
        min-val (apply min values)
        max-val (apply max values)]
    [mean-val min-val max-val]))

(defn- update-stat
  [key stat value]
  (swap! metrics #(update-in % [key stat] (fn [x] (apply vector (conj x value))))))

(defn add-stats!
  "Adds a metric derived from the statistics of a given set of values. Adds the mean, min and max of
  the given values to the metric with the given name."
  [key values]
  (mapv #(update-stat key %1 %2) [:mean :min :max] (calculate-stats values)))
