#lang racket
(require syntax/parse/define
         (for-syntax syntax/transformer)
         (rename-in racket/base [#%datum old-datum])
         "../main.rkt")
(provide define
         normalize
         fail
         (rename-out [mltt-module-begin #%module-begin]
                     [mltt-: :]

                     [mltt-Π Π]
                     [mltt-Π Pi]
                     [mltt-→ →]
                     [mltt-→ ->]
                     [mltt-lambda lambda]
                     [mltt-lambda λ]
                     [mltt-app #%app]

                     [mltt-× ×]
                     [mltt-cons cons]
                     [mltt-fst fst]
                     [mltt-snd snd]

                     [mltt-+ +]
                     [mltt-inl inl]
                     [mltt-inr inr]
                     [mltt-+-elim +-elim]

                     [mltt-⊥ ⊥]
                     [mltt-⊥ Bot]
                     [mltt-⊥-elim ⊥-elim]
                     [mltt-⊥-elim Bot-elim]
                     
                     [mltt-ℕ ℕ]
                     [mltt-ℕ Nat]
                     [mltt-zero zero]
                     [mltt-suc suc]
                     [mltt-datum #%datum]
                     [mltt-indℕ indℕ]
                     [mltt-indℕ ind-Nat]

                     [mltt-Type Type]))

(define (fail cs)
  (if (with-handlers ([exn:fail? (λ (e) #f)])
        (run-syn (type-infer cs)))
      (error "task didn't fail successfully!")
      (displayln "task failed successfully!")))

(define (run cs)
  (displayln (pp (cdr (run-syn (type-infer cs))))))

(define (run-normalize cs)
  (match-define (cons tp tm) (run-syn (type-infer cs)))
  (displayln (pp (normalize tm tp))))

(define-syntax-parse-rule (mltt-module-begin op ...)
  (#%module-begin (dispatch op) ...))

(define-syntax (dispatch stx)
  (syntax-parse stx
    [(_ ({~literal define} name:id tm))
     #'(begin
         (void (run-syn (type-infer tm)))
         (define name tm))]
    [(_ ({~literal normalize} tm)) #'(run-normalize tm)]
    [(_ ({~literal fail} tm)) #'(fail tm)]
    [(_ tm) #'(run tm)]))

(define-syntax-parse-rule (mltt-: e t)
  (cs-ann e t))

(define-syntax-parse-rule (mltt-Π ([x:id A]) B)
  (cs-Π 'x A
        (let-syntax ([x (make-variable-like-transformer #'(cs-var 'x))])
          B)))

(define-syntax-parse-rule (mltt-→ A B)
  (cs-→ A B))

(define-syntax-parse-rule (mltt-lambda (x:id ...) body)
  (cs-lam '(x ...)
          (let-syntax ([x (make-variable-like-transformer #'(cs-var 'x))]
                       ...)
            body)))

(define-syntax-parse-rule (mltt-app rator rand ...)
  (cs-app rator (list rand ...)))

(define-syntax-parse-rule (mltt-× A B)
  (cs-× A B))

(define-syntax-parse-rule (mltt-cons l r)
  (cs-cons l r))

(define-syntax-parse-rule (mltt-fst p)
  (cs-fst p))

(define-syntax-parse-rule (mltt-snd p)
  (cs-snd p))

(define-syntax-parse-rule (mltt-+ A B)
  (cs-+ A B))

(define-syntax-parse-rule (mltt-inl v)
  (cs-inl v))

(define-syntax-parse-rule (mltt-inr v)
  (cs-inr v))

(define-syntax-parse-rule (mltt-+-elim mot scrut l r)
  (cs-+-elim mot scrut l r))

(define-syntax mltt-⊥
  (make-variable-like-transformer #'(cs-⊥)))

(define-syntax-parse-rule (mltt-⊥-elim mot scrut)
  (cs-⊥-elim mot scrut))

(define-syntax mltt-ℕ
  (make-variable-like-transformer #'(cs-ℕ)))

(define-syntax mltt-zero
  (make-variable-like-transformer #'(cs-zero)))

(define-syntax-parse-rule (mltt-suc n)
  (cs-suc n))

(define-syntax (mltt-datum stx)
  (syntax-parse stx
    [(_ . i:number) #'(cs-num (old-datum . i))]
    [stx #'(old-datum . stx)]))

(define-syntax-parse-rule (mltt-indℕ mot scrut base step)
  (cs-indℕ mot scrut base step))

(define-syntax mltt-Type
  (make-variable-like-transformer #'(cs-Type)))
