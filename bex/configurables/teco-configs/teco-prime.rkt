#lang configurable/config "../configurables.rkt"

(require "../../orchestration/experiment-info.rkt")

;; NOTE it seems there's a reason why dbs: is not used here. For one, it does
;; not generalize across experiments; for two, I think it will cause issues on
;; the remote host

(configure! mutation                      code-mistakes)
(configure! mutant-sampling               pre-selected
            (build-path dbs:teco "mutant-samples.rktdb"))
(configure! mutant-filtering              dont-care-about-blame-trails)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner              teco-run) ;; TODO Change this to interpret result possibly
(configure! blame-translation             configurable-ctc-middleman-mod-to-source)
(configure! blame-following               null)
(configure! bt-root-sampling              pre-selected
            (build-path dbs:teco "pre-selected-bt-roots.rktdb"))
(configure! trail-completion              any-type-error/blamed-at-max)
(configure! configurations                teco-configs)
(configure! sql-data-collection           test-and-mutant-db  
            (build-path dbs:teco "sqlite/db.sqlite"))

(configure! module-instrumentation    insert-test)
(configure! program-instrumentation   insert-test-in-main)
