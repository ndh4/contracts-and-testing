#lang configurable/config "../configurables.rkt"

(require "../../orchestration/experiment-info.rkt")

(configure! mutation                      code-mistakes)
(configure! mutant-sampling               pre-selected
            (build-path (current-experiment-dir) "dbs" "mutant-samples.rktdb"))
(configure! mutant-filtering              dont-care-about-blame-trails)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner              teco-run) ;; TODO Change this to interpret result possibly
(configure! blame-translation             configurable-ctc-middleman-mod-to-source)
(configure! blame-following               null)
(configure! bt-root-sampling              pre-selected
            (build-path (current-experiment-dir) "dbs" "pre-selected-bt-roots.rktdb"))
(configure! trail-completion              any-type-error/blamed-at-max)
(configure! configurations                teco-configs)
(configure! sql-data-collection           test-and-mutant-db  
            (build-path (current-experiment-dir) "db.sqlite"))

(configure! module-instrumentation    insert-test)
(configure! program-instrumentation   insert-test-in-main)
