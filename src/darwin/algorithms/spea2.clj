;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Not for distribution.
;

(ns darwin.algorithms.spea2
  "An implementation of the SPEA2 algorithm of Zitzler et al. Well, almost. There's a very slight
  difference in way the archive is constructed. Using my technique you can replicate SPEA2 exactly, but
  it runs slowly. You can approximate the behaviour of SPEA2 and have it run pretty fast. I've tested
  the approximation on a few problems and it doesn't seem to make any real difference. See the archive
  thinner functions for details of the difference.

  This code is currently limited to two objectives, but the API is arranged so that should be easy to
  generalise if needed."
  (:require [darwin.evolution.pareto :as pareto]
            [kdtree]
            [darwin.evolution.selection :as selection]))

;; These functions can be grouped into:
;; - functions for calculating the fitness values
;; - functions for constructing the archive from the population and previous archive
;; - functions for plugging the algorithm into darwin.evolution.core


;; * Fitness calculation functions *

(defn- calculate-strength
  "The strength of an individual is the count of how many individuals it dominates. This function
  calculates the strength for each member of the population, and assocs it into the individual."
  [keys population]
  (map
    #(assoc % :spea2-strength (pareto/dominated-count keys population %))
    population))

(defn- calculate-raw-fitness
  "The raw fitness of an individual i is the sum of the strengths of the individuals that dominate i.
  The smaller this fitness measure the better."
  [keys population i]
  (let [dominators (pareto/dominator-set keys population i)]
    (apply + (map :spea2-strength dominators))))


(defn- calculate-raw-fitnesses
  "Calculates raw fitnesses for each individual in a population and assocs it into the individual's
  under the :spea2-raw-fitness key"
  [keys population]
  (let [counted-pop (calculate-strength keys population)]
    (map
      #(assoc % :spea2-raw-fitness (calculate-raw-fitness keys counted-pop %))
      population)))

(defn coords-from-individual
  "Get the individuals coordinates in objective space as a vector."
  [[k1 k2] i]
  [(k1 i) (k2 i)])

(defn- calculate-density
  [distance]
  (/ 1 (+ distance 2)))

(defn- kth-nearest-distance
  "Get the distance to the kth nearest neighbour of p, given a set of points represented by the given kd-tree."
  [tree k p]
  (Math/sqrt (first (sort > (map :dist-squared (kdtree/nearest-neighbor tree p k))))))

(defn- calculate-densities
  "Calculate the 'densities' for each individual in a population. The density is defined as 1 / (distance to
  kth-nearest neighbour + 2). k is taken as the sqrt of the population size. Assocs the densities into the
  individuals."
  [[k1 k2] population]
  (let [k (Math/sqrt (count population))
        ;; we extract the coordinates of each individual in objective space and build a kd-tree from them
        ;; so we can efficiently find the nearest neighbours.
        coords (map (partial coords-from-individual [k1 k2]) population)
        tree (kdtree/build-tree coords)]
    (map #(assoc % :spea2-density
                   (calculate-density
                     (kth-nearest-distance tree k (coords-from-individual [k1 k2] %))))
         population)))

(defn calculate-fitnesses
  "Calculates the SPEA2 fitness values with respect to the given keys. Assocs the fitness values into
  a :spea2-fitness key."
  [keys population]
  (map #(assoc % :spea2-fitness (+ (:spea2-raw-fitness %) (:spea2-density %)))
       (->> population
            (calculate-densities keys)
            (calculate-raw-fitnesses keys))))


;; * Archive construction functions *

(defn- k-nearest-distances
  "Returns the distances to the k nearest neighbours, in ascending order. The first neighbour is really
  a neighbour, not the point itself."
  [tree k-max p]
  ;; the rest drops the distance from the point to itself, which is always zero
  (apply vector (rest (sort < (map #(Math/sqrt (:dist-squared %)) (kdtree/nearest-neighbor tree p k-max))))))

(defn- remove-one-item
  "One step of the archive thinning routine, removes one individual from the archive."
  [goals comparison-depth tree-and-archive]
  (let [tree (:tree tree-and-archive)
        oversized-archive (:archive tree-and-archive)
        measured-archive (map #(assoc % :spea2-distances
                                        (k-nearest-distances tree comparison-depth (coords-from-individual goals %)))
                              oversized-archive)
        ;; this next step relies on the sort being done lexicographically on the distance arrays.
        ;; That `sort-by` does this isn't mentioned in the docstring, but is explicitly stated
        ;; here http://clojure.org/data_structures
        sorted-archive (sort-by :spea2-distances measured-archive)]
    {:archive (rest sorted-archive)
     :tree    (kdtree/delete tree (coords-from-individual goals (first sorted-archive)))}))

(defn thin-archive
  "SPEA2 has a fairly complicated prescription for thinning out the archive if it's oversized. It specifies
  that one should look for the individual who has the least distance to its nearest neighbour. If this doesn't
  yield a unique individual, then we should rank on distance to the second nearest neighbour, recursing until
  the tie is broken. We remove this individual from the population and repeat until we have thinned the archive
  to the desired size.

  We implement a modification of this algorithm. Instead we calculate for each point the distance to its
  `comparison-depth` nearest neighbours. We then sort the points lexicographically by these distance lists
  and drop the first item. Then we recalculate the distances and repeat to drop the desired number of items.
  If `comparison-depth` is the same as the archive size then this is equivalent to the SPEA2 technique,
  although spectacularly inefficient. Experiments have shown that using a `comparison-depth` of 5 does not
  give appreciably different results than SPEA2 for common problems."
  [goals oversized-archive comparison-depth target-size]
  (let [coords (map (partial coords-from-individual goals) oversized-archive)
        tree (kdtree/build-tree coords)
        thinned-tree-and-archive (nth (iterate
                                        (partial remove-one-item goals comparison-depth)
                                        {:tree tree :archive oversized-archive})
                                      (- (count oversized-archive) target-size))]
    (:archive thinned-tree-and-archive)))

(defn- dedupe-individual
  "Checks whether an individual has been seen, wrt a seen-set, and if not adds it to the collection (and set)."
  [goals [seen-set deduped-coll] individual]
  (let [coords (coords-from-individual goals individual)
        dupe (contains? seen-set coords)]
    (if dupe
      [seen-set deduped-coll]
      [(conj seen-set coords) (conj deduped-coll individual)])))

(defn deduplicate-population
  "Remove duplicated with respect to the scores for `goals` from the given population."
  [goals archive]
  (last (reduce (partial dedupe-individual goals) [#{} []] archive)))

(defn make-new-archive
  "Implements the core step of the SPEA2 algorithm which is constructing a new archive of elite
  individuals from the old archive and a population of new individuals."
  [goals deduplicate comparison-depth archive-size population old-archive]
  (let [pool (into population old-archive)
        scored-pool (calculate-fitnesses goals pool)
        ;; individuals that are non-dominated will have a fitness less than 1. Duplicates are removed if the
        ;; dedupe parameter is true. It's not quite clear to me whether deduplication is part of the SPEA2 algorithm
        ;; as published. But I suppose it doesn't matter, so long as you can turn it on and off!
        dedupe-fn (if deduplicate (partial deduplicate-population goals) identity)
        new-elites (dedupe-fn (filter #(< (:spea2-fitness %) 1.0) scored-pool))
        new-size (count new-elites)
        ;;_ (println "New archive raw size: " new-size)
        ]
    ;; there are three cases here: either the new archive is exactly the right size, too big, or too small
    (cond
      ;; the easy one, if we happen to have the right number, then we're done.
      (= new-size archive-size) new-elites
      ;; if we have too few non-dominated individuals to fill the archive then we select the dominated
      ;; individuals with the lowest fitness scores to make up the difference. The easiest way to do this
      ;; is to just sort and select from the pool. If requested, the pool is deduplicated. See above for notes.
      (< new-size archive-size) (take archive-size (sort-by :spea2-fitness < (dedupe-fn scored-pool)))
      ;; and finally, if we've got too many non-dominated individuals then some of them are for the chop
      (> new-size archive-size) (thin-archive goals new-elites comparison-depth archive-size))))


;; * darwin.evolution.core configuration *

(defn spea2-config
  "A configuration implementing SPEA2. Needs to be fed the set of goal keys (which must be of
  length 2 currently) and the unary and binary genetic operations that will be used in reproduction.

  The SPEA2 algorithm preserves an elite population from generation to generation. On each iteration
  first the newly bred individuals will be pooled with the elite population, and all have their
  :spea2-fitness calculated. Then, a new elite population is constructed made up of the non-dominated
  individuals - with thinning if there are too many elite individuals, and promotion from the population
  if there are too few. Finally, a new generation is bred from the archived individuals, using binary
  tournament selection on the :spea2-fitness, and the cycle begins again.

  There are a couple of deviations from the SPEA2 algorithm as described in the literature. First, the
  thinning algorithm is slightly simplified (see above). The setting :comparison-depth controls this
  simplification. The default value should be fine. Second, there is the option to de-duplicate the elite
  and the population at every iteration. This is off by default, which I _think_ corresponds to the published
  algorithm, but can be controlled with the boolean :deduplicate option."
  [config]
  (let [{:keys [unary-ops binary-ops goals archive-size comparison-depth deduplicate]
         :or   {comparison-depth 5
                deduplicate false}} config]
    {:elite-selector       (fn [rabble elite]
                               (make-new-archive goals deduplicate comparison-depth archive-size rabble elite))
     :mating-pool-selector (fn [_ elite] elite)
     :reproduction-config  {:selector   (partial selection/tournament-selector 2 :spea2-fitness)
                            :unary-ops  unary-ops
                            :binary-ops binary-ops}}))