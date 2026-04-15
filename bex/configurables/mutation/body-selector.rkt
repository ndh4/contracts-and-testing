#lang at-exp racket/base

(require racket/contract)
(require racket/list
         racket/match
         mutate/traversal
         syntax/parse)

(provide select-define-body-or-define/contract-body)

(define (select-define-body-or-define/contract-body stx)
  (syntax-parse stx
    [def:contracted-definition (select-define/contract-body stx)]
    [_ (select-define-body stx)]))

;; Copied from
;; mutate/mutate-lib/private/top-level-selectors.rkt
;; because it was private to that library.
(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern ((~and (~datum define/contract)
                  def/c)
            id/sig ctc body ...)))