#lang at-exp racket/base

(require racket/contract)
(require racket/list
         racket/match
         syntax/parse)

(provide select-define-body-or-define/contract-body)

;; This is the new thing. Everything else in this file is copied from
;; Lukas's mutate/mutate-lib/private/mutate-program.rkt
(define (select-define-body-or-define/contract-body stx)
  (syntax-parse stx
    [def:contracted-definition (select-define/contract-body stx)]
    [_ (select-define-body stx)]))
;; End new thing

(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern ((~and (~datum define/contract)
                  def/c)
            id/sig ctc body ...)))

(define-syntax-class definition
  #:description "define form"
  (pattern (def-form id/sig body ...)
           #:when (regexp-match? #rx"define"
                                 (symbol->string (syntax->datum #'def-form)))))

(define (leftmost-identifier-in stx)
  (match (flatten (list (syntax->datum stx)))
    [(list* (? symbol? s) _) s]
    [else '<no-name-found>]))



(define (select-define-body stx)
  (syntax-parse stx
    #:datum-literals [define :]
    [({~and define def} id/sig
                        {~optional {~seq : type}}
                        body ...)
     (define function? (and (syntax->list #'id/sig) #t))
     (define body-stxs
       (if function?
           (list (syntax/loc stx (begin body ...)))
           (attribute body)))
     (define (reconstruct-definition body-stxs/mutated)
       (define body-stxs/no-begin
         (if function?
             (syntax-parse body-stxs/mutated
               [[{~describe "begin-wrapped function body (from select-define-body reconstructor)"
                            (begin mutated-body-e ...)}]
                (attribute mutated-body-e)])
             body-stxs/mutated))
       (quasisyntax/loc stx
         (def id/sig {~? {~@ : type}} #,@body-stxs/no-begin)))
     (list body-stxs
           (leftmost-identifier-in #'id/sig)
           reconstruct-definition)]
    [_ #f]))

(define (select-define/contract-body stx)
  (syntax-parse stx
    [def:contracted-definition
      (match-define (list to-mutate id reconstructor)
        (select-define-body #'(define def.id/sig def.body ...)))
      (define (reconstruct-definition body-stxs/mutated)
        (syntax-parse (reconstructor body-stxs/mutated)
          [(define _ mutated-body-e ...)
           (syntax/loc stx
             (def.def/c def.id/sig def.ctc
               mutated-body-e ...))]))
      (list to-mutate
            (leftmost-identifier-in #'def.id/sig)
            reconstruct-definition)]
    [_ #f]))