;
; This file is part of darwin.
;
; Copyright (C) 2014-, Imperial College, London, All rights reserved.
;
; Contributors: Jony Hudson
;
; Released under the MIT license..
;

(defproject darwin "1.0.1"
            :url "https://github.com/JonyEpsilon/darwin"
            :license {:name "MIT"}
            :dependencies [[org.clojure/clojure "1.6.0"]]
            :javac-options ["-target" "1.7" "-source" "1.7"]
            :plugins [[lein-gorilla "0.3.4"]]
            :jvm-opts ^:replace ["-server" "-Xmx4g"])
