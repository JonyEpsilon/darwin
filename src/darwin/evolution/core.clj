;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Not for distribution.
;

(ns darwin.evolution.core
  "The functions in this namespace provide generic plumbing for an adaptive generational evolutionary algorithm.
  Supported features include explicit mating pool generation, optional maintenance of an elite population,
  optional pre-processing/transformation of the population before scoring, and optional flexible adaptation during
  the run. Central to these functions are the zeitgeist and generation-config data structures. The zeitgeist contains
  all of the state of the evolutionary algorithm at one generation. The generation-config gives a complete specification
  of how to evolve from one generation to the next. Adpatation is introduced by allowing the generation-config to change
  during the run.

  This plumbing is general enough to run simple GA, or more complex multi-objective algorithms like NSGA-II
  or SPEA2, along with hybrid algorithms that combine these with hill-descent and other non EA
  transformations. This namespace doesn't, though, implement any of those algorithms itself. See other
  namespaces in the algorithms package for specific implementations."
  (:require [darwin.evolution.reproduction :as reproduction]
            [darwin.evolution.scoring :as scoring]
            [darwin.evolution.metrics :as metrics]))

;; An atom to store the lastest generation which is useful for debugging etc
(def latest (atom {}))

(defn evolve
  "Runs one generation of the evolutionary algorithm. Takes a zeitgeist and a generation-config
  and returns the new zeitgeist.

  This function just provides the plumbing and calls out to functions provided in the config to do the actual work.
  The algorithm proceeds in a number of steps:
   - determine the new elite by taking the current rabble and elite, and applying a function
   - decide who is eligible to participate in reproduction, by applying a function to the rabble and elite
   - generate a new rabble from the mating pool, using a given selection procedure and given genetic operations
   - run a list of transformations on the rabble
   - update the scores of the new rabble
   - update metrics
   - run a reporting function to provide the user with information about this generation
   - run a checkpoint function to optionally save the state of the algorithm."
  [zeitgeist generation-config]
  (let [{:keys [ea-config transformations score-functions reporting-function checkpoint-function]} generation-config
        {:keys [elite-selector mating-pool-selector reproduction-config]} ea-config
        ;; we time each generations execution (against the wall-clock)
        start-time (System/currentTimeMillis)
        ;; the EA proper
        rabble (:rabble zeitgeist)
        elite (or (:elite zeitgeist) [])
        new-elite (elite-selector rabble elite)
        elite-selected-time (System/currentTimeMillis)
        mating-pool (mating-pool-selector rabble new-elite)
        new-rabble (reproduction/reproduce reproduction-config mating-pool)
        transformed-rabble (if (nil? transformations)
                             new-rabble
                             ((apply comp transformations) new-rabble))
        rabble-ready-time (System/currentTimeMillis)
        scored-transformed-rabble (scoring/update-scores transformed-rabble score-functions)
        scored-new-elite (scoring/update-scores new-elite score-functions)
        evolved-zg (assoc (assoc zeitgeist :rabble scored-transformed-rabble) :elite scored-new-elite)
        _ (reset! latest evolved-zg)
        end-time (System/currentTimeMillis)
        ;; track generation number
        final-zg (update-in evolved-zg [:age] (fn [x] (if (nil? x) 0 (inc x))))
        ;; update the timing metrics
        _ (metrics/add! :time (- end-time start-time))
        _ (metrics/add! :selection-time (- elite-selected-time start-time))
        _ (metrics/add! :reproduction-time (- rabble-ready-time elite-selected-time))
        _ (metrics/add! :scoring-time (- end-time rabble-ready-time))
        ;; add stats of all the scores to the metrics
        _ (mapv #(metrics/add-stats!
                 (first %)
                 (mapv (first %) (into scored-transformed-rabble new-elite)))
               score-functions)
        ;; report and checkpoint
        _ (when reporting-function (reporting-function final-zg))
        _ (when checkpoint-function (checkpoint-function final-zg))]
    final-zg))

(defn- no-adpatation
  "An adaptation function that does ... no adapation. Used as a default below."
  [zg gc]
  gc)

(defn run-evolution
  "Runs the evolutionary algorithm until the stopping-function is satisfied. The stopping function is passed both
  the current zeitgeist and the current generation-config (so it can stop in response to adaptive behaviour).
  Returns the final zeitgeist.

  The algorithm can adapt to the state of the run through the adapt function, if provided this will be called
  after each generation, with the previous generation's generation-config, and the new zeitgeist. It must return
  the generation-config for the next generation. If no adapt-function is provided, then the same generation-config
  will be used for each generation.

  Each iteration round the loop looks something like this:

    zg                              new-zg
  -----> evolve -------+-------------------->
           ^           |
   g-c     |           v           new-g-c
  ---------+----- adapt-function ----------->

  When viewed this way, it's natural to think of the generation-config as a kind of environment for the evolution.
  In an adaptive run, not only does the population change to fit the environment, the environment also changes,
  possibly in response to the population.

  Metrics are accumulated and stored in an atom. This allows one to monitor the run in realtime by watching
  the atom in another thread, if desired."
  ([initial-gc initial-zg stopping-function]
   (run-evolution initial-gc initial-zg stopping-function no-adpatation))
  ([initial-gc initial-zg stopping-function adapt-function]
  ;; score the initial zeitgeist
  (let [scored-initial-zg (update-in initial-zg [:rabble] #(scoring/update-scores % (:score-functions initial-gc)))
        adapt (if adapt-function adapt-function (fn [_ gc] gc))]
    ;; reset metrics
    (metrics/clear!)
    ;; run the main loop
    (loop [zg scored-initial-zg
           gc initial-gc]
      (if (not (stopping-function zg gc))
        ;; evolve a new generation and adapt
        (let [new-zg (evolve zg gc)
              new-gc (adapt new-zg gc)]
          (recur new-zg new-gc))
        ;; stopping condition met, return the final zeitgiest
        zg)))))


(defn make-zeitgeist
  "A helper function for making an initial zeitgeist from a list of genotypes."
  [genotypes-list]
  {:elite  []
   :rabble (map (fn [g] {:genotype g}) genotypes-list)
   :age    0})