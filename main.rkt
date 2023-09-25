#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Credits: I had a ton of help from Joshua ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/trace)
(module+ test
  (require rackunit)

  ; check-success intentionally removed
  (define-syntax-rule (check-error exp)
    (check-exn exn:fail? (thunk exp))))

;;;; CORE TERMS

;; A Syntax is one of:
;; - (syntax-local Number)
;; - (syntax-ann Syntax Syntax)
;;
;; - (syntax-Π Symbol Syntax Syntax)
;; - (syntax-lam Symbol Syntax)
;; - (syntax-app Syntax Syntax)
;;
;; - (syntax-× Syntax Syntax)
;; - (syntax-cons Syntax Syntax)
;; - (syntax-fst Syntax)
;; - (syntax-snd Syntax)
;;
;; - (syntax-+ Syntax Syntax)
;; - (syntax-inl Syntax)
;; - (syntax-inr Syntax)
;; - (syntax-+-elim Syntax Syntax Syntax Syntax)
;;
;; - (syntax-⊥)
;; - (syntax-⊥-elim Syntax Syntax)
;;
;; - (syntax-ℕ)
;; - (syntax-zero)
;; - (syntax-suc Syntax)
;; - (syntax-recℕ Syntax Syntax Syntax Syntax)
;; - (syntax-indℕ Syntax Syntax Syntax Syntax)
;;
;; - (syntax-Type)
(struct syntax-local (n) #:transparent)
(struct syntax-ann (tm tp) #:transparent)

(struct syntax-Π (var dom rng) #:transparent
  #:methods gen:equal+hash
  [;; ignore the variable name in equal?
   (define (equal-proc pi1 pi2 rec-equal?)
     (and (rec-equal? (syntax-Π-dom pi1) (syntax-Π-dom pi2))
          (rec-equal? (syntax-Π-rng pi1) (syntax-Π-rng pi2))))
   (define (hash-proc pi rec)
     (+ (rec (syntax-Π-dom pi))
        (rec (syntax-Π-rng pi))))
   (define (hash2-proc pi rec)
     (+ (rec (syntax-Π-dom pi))
        (rec (syntax-Π-rng pi))))])
(struct syntax-lam (var body) #:transparent
  #:methods gen:equal+hash
  [;; ignore the variable name in equal?
   (define (equal-proc lam1 lam2 rec-equal?)
     (rec-equal? (syntax-lam-body lam1) (syntax-lam-body lam2)))
   (define (hash-proc lam rec)
     (rec (syntax-lam-body lam)))
   (define (hash2-proc lam rec)
     (rec (syntax-lam-body lam)))])
(struct syntax-app (rator rand) #:transparent)

(struct syntax-× (lhs rhs) #:transparent)
(struct syntax-cons (fst snd) #:transparent)
(struct syntax-fst (pair) #:transparent)
(struct syntax-snd (pair) #:transparent)

(struct syntax-+ (left right) #:transparent)
(struct syntax-inl (value) #:transparent)
(struct syntax-inr (value) #:transparent)
(struct syntax-+-elim (motive scrut l-case r-case) #:transparent)

(struct syntax-⊥ () #:transparent)
(struct syntax-⊥-elim (motive scrut) #:transparent)

(struct syntax-ℕ () #:transparent)
(struct syntax-zero () #:transparent)
(struct syntax-suc (n) #:transparent)
(struct syntax-recℕ (motive scrut base step) #:transparent)
(struct syntax-indℕ (motive scrut base step) #:transparent)

(struct syntax-Type () #:transparent)

;; pp : Syntax -> Sexp
(define (pp stx [env '()])
  (match stx
    [(syntax-local n) (list-ref env n)]
    [(syntax-ann tm tp) `(: ,(pp tm env) ,(pp tp env))]
    [(syntax-Π var base fam)
     `(Π ([,var ,(pp base env)]) ,(pp fam (cons var env)))]
    [(syntax-lam var body)
     `(λ (,var) ,(pp body (cons var env)))]
    [(syntax-app rator rand) `(,(pp rator env) ,(pp rand env))]
    [(syntax-× fst snd)
     `(× ,(pp fst env) ,(pp snd env))]
    [(syntax-cons fst snd) `(cons ,(pp fst env) ,(pp snd env))]
    [(syntax-fst p) `(fst ,(pp p env))]
    [(syntax-snd p) `(snd ,(pp p env))]
    [(syntax-+ l r)
     `(+ ,(pp l env) ,(pp r env))]
    [(syntax-inl v) `(inl ,(pp v env))]
    [(syntax-inr v) `(inr ,(pp v env))]
    [(syntax-+-elim motive scrut l r) `(+-elim ,(pp motive env) ,(pp scrut env) ,(pp l env) ,(pp r env))]

    [(syntax-⊥) '⊥]
    [(syntax-⊥-elim motive scrut) `(⊥-elim ,(pp motive env) ,(pp scrut env))]

    [(syntax-ℕ) 'ℕ]
    [(syntax-zero) 0]
    [(syntax-suc n)
     (define res (pp n env))
     (cond [(number? res) (add1 res)]
           [else `(suc ,res)])]
    [(syntax-recℕ mot scrut base step)
     `(recℕ ,(pp mot env)
            ,(pp scrut env)
            ,(pp base env)
            ,(pp step env))]
    [(syntax-indℕ mot scrut base step)
     `(indℕ ,(pp mot env)
            ,(pp scrut env)
            ,(pp base env)
            ,(pp step env))]
    [(syntax-Type) 'Type]))


;;;; NORMALIZATION

;; A Value is one of:
;; - Cut
;;
;; - (value-→ Value Value)
;; - (value-Π Symbol Value Closure)
;; - (value-lam Symbol Closure)
;;
;; - (value-× Value Value)
;; - (value-cons Value Value)
;;
;; - (value-+ Value Value)
;; - (value-inl Value)
;; - (value-inr Value)
;;
;; - (value-⊥)
;;
;; - (value-ℕ)
;; - (value-zero)
;; - (value-suc Value)
;;
;; - (value-Type)
(struct value-→ (dom rng) #:transparent)
(struct value-Π (name A B-clo) #:transparent)
(struct value-lam (name clo) #:transparent)

(struct value-× (lhs rhs) #:transparent)
(struct value-cons (fst snd) #:transparent)

(struct value-+ (left right) #:transparent)
(struct value-inl (value) #:transparent)
(struct value-inr (value) #:transparent)

(struct value-⊥ () #:transparent)

(struct value-ℕ () #:transparent)
(struct value-zero () #:transparent)
(struct value-suc (n) #:transparent)

(struct value-Type () #:transparent)

;; A Cut is a (cut Value Head [ListOf Form])
(struct cut (tp head spine) #:transparent)

;; A Head is one of:
;; - (head-local PositiveInteger)
(struct head-local (lvl) #:transparent)

;; A Form is one of:
;; - (form-app Value Value)
;; - (form-fst)
;; - (form-snd)
;; - (form-+-elim Value Value Value Value Value)
;; - (form-⊥-elim Value)
;; - (form-recℕ Value Value Value)
;; - (form-indℕ Value Value Value)
(struct form-app (tp rand) #:transparent)
(struct form-fst () #:transparent)
(struct form-snd () #:transparent)
(struct form-+-elim (motive l-case-tp r-case-tp l-case r-case) #:transparent)
(struct form-⊥-elim (motive) #:transparent)
(struct form-recℕ (motive base step) #:transparent)
(struct form-indℕ (motive base step) #:transparent)

;; A Environment is a [ListOf Value]

;; A Closure is one of:
;; - (closure Syntax Environment)
;; - (h-closure [Value -> Value])
(struct closure (term env) #:transparent)
(struct h-closure (fn) #:transparent)

;; apply-closure : Closure Value -> Value
;; Applies the closure to the value.
(define (apply-closure clo rand)
  (match clo
    [(closure tm env) (evaluate tm (extend-env env rand))]
    [(h-closure fn) (fn rand)]))

;; empty-env : -> Environment
;; Produces an empty environment.
(define (empty-env)
  '())

;; extend-env : Environment Value -> Environment
;; Adds a value to the environment.
(define (extend-env env x)
  (cons x env))

;; apply-env : Environment Number -> Value
;; Gets the nth element out of the environment.
(define (apply-env env n)
  (list-ref env n))

;; fresh-cut : Value Number -> Cut
;; Given a type and a number, generate a fresh cut.
(define (fresh-cut tp n)
  (cut tp (head-local n) '()))

;; throw-type-error! : -> Void
(define (throw-type-error! fmt . stuff)
  (error (apply format (string-append "TYPE ERROR: " fmt) stuff)))

;; do-recℕ : Value Value Value Value -> Value
;; Performs fold on the scrutineé.
(define (do-recℕ mot scrut base step)
  (match scrut
    [(value-zero) base]
    [(value-suc n) (do-app
                    step
                    (do-recℕ mot n base step))]
    [(cut (value-ℕ) head spine)
     (cut mot head (cons (form-recℕ mot base step) spine))]))

;; do-indℕ : Value Value Value Value -> Value
;; Performs fold on the scrutineé.
(define (do-indℕ mot scrut base step)
  (match scrut
    [(value-zero) base]
    [(value-suc n) (do-app
                    (do-app step n)
                    (do-indℕ mot n base step))]
    [(cut (value-ℕ) head spine)
     (cut mot head (cons (form-indℕ mot base step) spine))]))

;; do-fst : Value -> Value
;; Retrieves the first out of a "cons" value.
(define (do-fst pair)
  (match pair
    [(value-cons fst snd) fst]
    [(cut (value-× A B) head spine)
     (cut A head (cons (form-fst) spine))]))

;; do-snd : Value -> Value
;; Retrieves the second value out of a cons.
(define (do-snd pair)
  (match pair
    [(value-cons fst snd) snd]
    [(cut (value-× A B) head spine)
     (cut B head (cons (form-snd) spine))]))

;; do-app : Value Value -> Value
;; Applies the rator to the rand.
(define (do-app rator rand)
  (match rator
    [(value-lam _ closure) (apply-closure closure rand)]
    [(cut (value-Π name A B-clo) head spine)
     (cut (apply-closure B-clo rand) head (cons (form-app A rand) spine))]))

;; do-+-elim : Type Value Value Value -> Value
;; Performs sum elimination.
(define (do-+-elim C scrut f g)
  (match scrut
    [(value-inl a) (do-app f a)]
    [(value-inr b) (do-app g b)]
    [(cut (value-+ A B) head spine)
     (cut C head (cons (form-+-elim
                        C
                        (value-Π '_ A (h-closure (λ (_) C)))
                        (value-Π '_ B (h-closure (λ (_) C)))
                        f
                        g) spine))]))

;; do-⊥-elim : Value Value -> Value
;; Performs ⊥ elimination.
(define (do-⊥-elim tp scrut)
  (match scrut
    [(cut (value-⊥) head spine)
     (cut tp head (cons (form-⊥-elim tp) spine))]))

;; evaluate : Syntax Environment -> Value
;; Evaluates the expression to a normal form.
(define (evaluate exp env)
  (match exp
    [(syntax-ann tm _) (evaluate tm env)]
    [(syntax-local n) (apply-env env n)]
    [(syntax-lam sym body) (value-lam sym (closure body env))]
    [(syntax-cons fst snd) (value-cons (evaluate fst env) (evaluate snd env))]
    [(syntax-inl sum) (value-inl (evaluate sum env))]
    [(syntax-inr sum) (value-inr (evaluate sum env))]
    [(syntax-fst pair) (do-fst (evaluate pair env))]
    [(syntax-snd pair) (do-snd (evaluate pair env))]
    [(syntax-+-elim tp scrut l r) (do-+-elim (evaluate tp env)
                                             (evaluate scrut env)
                                             (evaluate l env)
                                             (evaluate r env))]
    [(syntax-app rator rand) (do-app (evaluate rator env) (evaluate rand env))]
    [(syntax-⊥-elim tp tm) (do-⊥-elim tp (evaluate tm env))]
    [(syntax-Π name A B) (value-Π name (evaluate A env) (closure B env))]
    [(syntax-+ lhs rhs) (value-+ (evaluate lhs env) (evaluate rhs env))]
    [(syntax-× fst snd) (value-× (evaluate fst env) (evaluate snd env))]
    [(syntax-recℕ motive scrut base step) (do-recℕ (evaluate motive env) (evaluate scrut env)
                                                   (evaluate base env) (evaluate step env))]
    [(syntax-indℕ motive scrut base step) (do-indℕ (evaluate motive env) (evaluate scrut env)
                                                   (evaluate base env) (evaluate step env))]
    [(syntax-ℕ) (value-ℕ)]
    [(syntax-zero) (value-zero)]
    [(syntax-suc n) (value-suc (evaluate n env))]
    [(syntax-Type) (value-Type)]
    [(syntax-⊥) (value-⊥)]))

;; reify : Number Value Value -> Syntax
;; Turns a value under the given environment size into a syntax.
(define (reify size val tp)
  (match tp
    [(value-Π name A B-clo)
     (syntax-lam
      (match val
        [(value-lam name _) name]
        [_ (gensym)])
      (reify (add1 size)
             (do-app val (fresh-cut A size))
             (apply-closure B-clo (fresh-cut A size))))]
    [(value-× A B)
     (match val
       [(value-cons fst snd) (syntax-cons (reify size fst A) (reify size snd B))]
       [(cut _ _ _) (reify-cut size val)])]
    [(value-+ A B)
     (match val
       [(value-inl sum) (syntax-inl (reify size sum A))]
       [(value-inr sum) (syntax-inr (reify size sum B))]
       [(cut _ _ _) (reify-cut size val)])]
    [(value-⊥)
     (match val
       [(cut _ _ _) (reify-cut size val)])]
    [(value-ℕ)
     (match val
       [(value-zero) (syntax-zero)]
       [(value-suc n) (syntax-suc (reify size n (value-ℕ)))]
       [(cut _ _ _) (reify-cut size val)])]
    [(value-Type)
     (match val
       [(value-× fst snd) (syntax-× (reify size fst (value-Type))
                                    (reify size snd (value-Type)))]
       [(value-+ lhs rhs) (syntax-+ (reify size lhs (value-Type))
                                    (reify size rhs (value-Type)))]
       [(value-⊥) (syntax-⊥)]
       [(value-ℕ) (syntax-ℕ)]
       [(value-Π name A B-clo)
        (syntax-Π name
                  (reify size A (value-Type))
                  (reify (add1 size)
                         (apply-closure B-clo (fresh-cut A size))
                         (value-Type)))]
       [(value-Type) (syntax-Type)]
       [(cut _ _ _) (reify-cut size val)])]
    [(cut _ _ _)
     (reify-cut size val)]))


;; normalize : Syntax Value -> Syntax
;; Produces the normal form of the given syntax.
(define (normalize tm tp)
  (reify 0 (evaluate tm '()) tp))

;; reify-cut : Number Cut -> Syntax
;; Quotes a cut by quoting its head and adding back all its eliminators.
(define (reify-cut size c)
  (match-define (cut _ head spine) c)
  (reify-spine size (reify-head size head) spine))

;; reify-head : Number Head -> Syntax
;; Turns a head into a syntax.
(define (reify-head size head)
  (match head
    [(head-local lvl) (syntax-local (- size lvl 1))]))

;; reify-spine : Number Syntax [ListOf Form] -> Syntax
(define (reify-spine size exp spine)
  (match spine
    ['() exp]
    [(cons form rst)
     (reify-form size (reify-spine size exp rst) form)]))

;; reify-closure : Number Closure -> Syntax
(define (reify-closure size closure)
  (reify (add1 size) (apply-closure closure (cut (head-local size) '()))))

;; reify-form : Number Syntax Form -> Syntax
(define (reify-form size exp form)
  (match form
    [(form-app A rand) (syntax-app exp (reify size rand A))]
    [(form-fst) (syntax-fst exp)]
    [(form-snd) (syntax-snd exp)]
    [(form-+-elim motive lcase-tp rcase-tp f g)
     (syntax-+-elim (reify size motive (value-Type))
                    exp
                    (reify size f lcase-tp)
                    (reify size g rcase-tp))]
    [(form-⊥-elim tp) (syntax-⊥-elim tp exp)]
    [(form-recℕ motive base step)
     (syntax-recℕ (reify size motive (value-Type))
                  exp
                  (reify size base motive)
                  (reify size step
                         (value-Π '_ motive
                                  (h-closure (λ (_) motive)))))]
    [(form-indℕ motive base step)
     (syntax-indℕ (reify size motive (value-Π '_ (value-ℕ) (h-closure (λ (_) (value-Type)))))
                  exp
                  (reify size base (do-app motive (value-zero)))
                  (reify size step (value-Π 'k
                                            (value-ℕ)
                                            (h-closure
                                             (λ (k^)
                                               (value-Π '_
                                                        (do-app motive k^)
                                                        (h-closure
                                                         (λ (_)
                                                           (do-app motive (value-suc k^))))))))))]))

;; A Context is a [Assocof Symbol Cut]
;; *Interpretation*: This is kind of a Environment, but it's also a context,
;;                   because cuts preserve type information.

;; extend-context : Context Symbol Value -> Context
;; Adds a witness of the given type to the context.
(define (extend-context Γ name c)
  (cons (cons name c) Γ))

;; fresh-with-context : Context Value -> Cut
;; Generates a fresh cut under the given context.
(define (fresh-with-context Γ tp)
  (fresh-cut tp (length Γ)))

;; eval-with-context : Syntax Context -> Value
;; Performs evaluation under the given context.
(define (eval-with-context stx Γ)
  (evaluate stx (map cdr Γ)))

;; reify-with-context : Context Value Value -> Syntax
;; Performs reification under the input context.
(define (reify-with-context Γ val tp)
  (reify (length Γ) val tp))

;; type=? : Value Value Context -> Boolean
;; Does conversion checking: is the type t1 compatible with t2?
(define (type=? t1 t2 ctx)
  (equal? (reify-with-context ctx t1 (value-Type))
          (reify-with-context ctx t2 (value-Type))))

;; assert-type-equal! : Value Value Context -> Void
;; Asserts that types are convertible.
(define (assert-type-equal! t1 t2 ctx)
  (unless (type=? t1 t2 ctx)
    (error (format "non-convertible types: ~a is not ~a"
                   (reify-with-context ctx t1 (value-Type))
                   (reify-with-context ctx t2 (value-Type))))))

;; context-index-of : Context Symbol -> NaturalNumber
;; Returns the index of the symbol in the context.
(define (context-index-of Γ s)
  (match Γ
    ['() (error "not found")]
    [(cons (cons x _) rst)
     (cond [(eq? x s) 0]
           [else (add1 (context-index-of rst s))])]))

;; A ChkTactic is a (chk-tactic Symbol [Context Value -> Syntax])
;; A SynTactic is a (syn-tactic Symbol [Context -> [PairOf Value Syntax]])
(struct chk-tactic (name f)
  #:transparent
  #:property prop:procedure (struct-field-index f))
(struct syn-tactic (name f)
  #:transparent
  #:property prop:procedure (struct-field-index f))

;; run-chk : Value ChkTactic -> Syntax
(define (run-chk goal tac)
  (tac '() goal))

;; run-syn : SynTactic -> [PairOf Value Syntax]
(define (run-syn tac)
  (tac '()))

;; chk : SynTactic -> ChkTactic
(define/contract (chk tac)
  (syn-tactic? . -> . chk-tactic?)
  (chk-tactic
   'chk
   (λ (Γ goal)
     (match-define (cons tp tm) (tac Γ))
     (assert-type-equal! tp goal Γ)
     tm)))

;; ann : ChkTactic ChkTactic -> SynTactic
(define/contract (ann tac tp-tac)
  (chk-tactic? any/c . -> . syn-tactic?)
  (syn-tactic
   'ann
   (λ (Γ)
     (define tp-stx (tp-tac Γ (value-Type)))
     (define tp (eval-with-context tp-stx Γ))
     (cons tp (tac Γ tp)))))

;; var : Symbol -> SynTactic
(define/contract (var s)
  (symbol? . -> . syn-tactic?)
  (syn-tactic
   'var
   (λ (Γ)
     (match-define (cut tp _ _) (dict-ref Γ s))
     (cons tp (syntax-local (context-index-of Γ s))))))

;; ×-form : ChkTactic ChkTactic -> SynTactic
(define/contract (×-form l-tac r-tac)
  (chk-tactic? chk-tactic? . -> . syn-tactic?)
  (syn-tactic
   '×-form
   (λ (Γ)
     (define l-stx (l-tac Γ (value-Type)))
     (define r-stx (r-tac Γ (value-Type)))
     (cons (value-Type) (syntax-× l-stx r-stx)))))

;; +-form : ChkTactic ChkTactic -> SynTactic
(define/contract (+-form l-tac r-tac)
  (chk-tactic? chk-tactic? . -> . syn-tactic?)
  (syn-tactic
   '+-form
   (λ (Γ)
     (define l-stx (l-tac Γ (value-Type)))
     (define r-stx (r-tac Γ (value-Type)))
     (cons (value-Type) (syntax-+ l-stx r-stx)))))

;; ⊥-form : SynTactic
(define ⊥-form
  (syn-tactic
   '⊥-form
   (λ (Γ)
     (cons (value-Type) (syntax-⊥)))))

;; Π-form : Symbol ChkTactic ChkTactic -> SynTactic
;; Performs the well-formedness rule for Π types.
(define (Π-form sym A-tac B-tac)
  (syn-tactic
   'Π-form
   (λ (Γ)
     (define A-stx (A-tac Γ (value-Type)))
     (define A (eval-with-context A-stx Γ))
     (define new-Γ (extend-context Γ sym (fresh-with-context Γ A)))
     (define B-stx (B-tac new-Γ (value-Type)))
     (cons (value-Type) (syntax-Π sym A-stx B-stx)))))

;; f: A -> B, x: A => (f x) : B
;; λf.λx.(f x)
;; app : SynTactic ChkTactic -> SynTactic
;; Implements the function application type rule.
;;    rator-tac is a cons pair of the function type and its syntax.
;;    rand-tac is a function that returns ONLY the syntax of rand.
(define/contract (app rator-tac rand-tac)
  (-> syn-tactic? chk-tactic? syn-tactic?)
  (syn-tactic
   'app
   (λ (Γ)
     (match-define (cons rator-type rator-stx) (rator-tac Γ))
     (match rator-type
       [(value-Π name A B-clo)
        (define rand-stx (rand-tac Γ A))
        (define B (apply-closure B-clo (eval-with-context rand-stx Γ)))
        (cons B (syntax-app rator-stx rand-stx))]
       [else (throw-type-error! "app: type error")]))))

;; cons^ : ChkTactic ChkTactic -> ChkTactic
;; Implements the 'product rule', i.e., conjunction via cons,
;; which, given a witness checking as P and a witness checking as Q,
;; gives us a witness checking as P × Q.
(define/contract (cons^ p-tac q-tac)
  (-> chk-tactic? chk-tactic? chk-tactic?)
  (chk-tactic
   'cons^
   (λ (Γ goal)
     (match goal
       [(value-× lhs-× rhs-×)
        (syntax-cons (p-tac Γ lhs-×) (q-tac Γ rhs-×))]
       [else (throw-type-error! "cons^: rule applied incorrectly")]))))

;; fst : SynTactic -> SynTactic
;; Implements ×-elimination (left),
;; which, given a witness of P × Q,
;; gives us a witness of P.
(define/contract (fst pq-tac)
  (-> syn-tactic? syn-tactic?)
  (syn-tactic
   'fst
   (λ (Γ)
     (match-define (cons pq-tp pq-stx) (pq-tac Γ))
     (match pq-tp
       [(value-× p q) (cons p (syntax-fst pq-stx))]
       [else (throw-type-error! "fst: type error\n")]))))

;; snd : SynTactic -> SynTactic
;; Implements ×-elimination (right),
;; which, given a witness of P × Q,
;; gives us a witness of Q.
(define/contract (snd P×Q)
  (-> syn-tactic? syn-tactic?)
  (syn-tactic
   'snd
   (λ (Γ)
     (match-define (cons τpq stx-pq) (P×Q Γ))
     (match τpq
       [(value-× p q) (cons q (syntax-snd stx-pq))]
       [else (throw-type-error! "snd: type error\n")]))))

;; intro : Symbol ChkTactic -> ChkTactic
;; Implements *→-introduction/direct proof*,
;; which, given that having a witness of P means that we can get a witness checking as Q,
;; gives us a witness checking as P → Q.
(define/contract (intro sym imp-tac)
  (-> symbol? chk-tactic? chk-tactic?)
  (chk-tactic
   'intro
   (λ (Γ goal)
     (match goal
       [(value-Π name A B-clo)
        (define fresh (fresh-with-context Γ A))
        (define B (apply-closure B-clo fresh))
        (define new-Γ (extend-context Γ sym fresh))
        (define B-stx (imp-tac new-Γ B))
        (syntax-lam sym B-stx)]
       [else (throw-type-error! "intro: invalid goal ~a\n" goal)]))))

;; inl : ChkTactic -> ChkTactic
;; Implements +-introduction (left)/weakening,
;; which, given a witness checking as P,
;; gives us a witness checking as P + Q.
(define/contract (inl or-tac)
  (-> chk-tactic? chk-tactic?)
  (chk-tactic
   'inl
   (λ (Γ goal)
     (match goal
       [(value-+ p q)
        (syntax-inl (or-tac Γ p))]
       [else (throw-type-error! "inl: invalid goal\n")]))))

;; inr : ChkTactic -> ChkTactic
;; Implements +-introduction (right)/weakening,
;; which, given a witness checking as Q,
;; gives us a witness checking as P + Q.
(define/contract (inr or-tac)
  (-> chk-tactic? chk-tactic?)
  (chk-tactic
   'inr
   (λ (Γ goal)
     (match goal
       [(value-+ p q)
        (syntax-inr (or-tac Γ q))]
       [else (throw-type-error! "inr: invalid goal\n")]))))

;; +-elim : ChkTactic SynTactic ChkTactic ChkTactic -> SynTactic
;; Implements +-elimination/proof by cases.
(define/contract (+-elim mot-tac scrut-tac l-tac r-tac)
  (-> chk-tactic? syn-tactic? chk-tactic? chk-tactic? syn-tactic?)
  (syn-tactic
   '+-elim
   (λ (Γ)
     (define mot-stx (mot-tac Γ (value-Type)))          ; <- Extract motive syntax.
     (define mot (eval-with-context mot-stx Γ))         ; <- Get motive... what do I do with this? Do I even need it?
     (match-define (cons (value-+ A B) scrut-stx) (scrut-tac Γ))
     (define l-stx (l-tac Γ (value-Π '_ A (h-closure (λ (_) mot)))))
     (define r-stx (r-tac Γ (value-Π '_ B (h-closure (λ (_) mot)))))
     (cons mot (syntax-+-elim mot-stx scrut-stx l-stx r-stx)))))


;; ⊥-elim : ChkTactic ChkTactic -> SynTactic
;; Implements ex falso/the principle of explosion,
;; which, given a witness of ⊥ (false),
;; gives us a universal witness checking as anything.
(define/contract (⊥-elim mot-tac ⊥-tac)
  (-> chk-tactic? syn-tactic? syn-tactic?)
  (syn-tactic
   '⊥-elim
   (λ (Γ)
     (define mot-stx (mot-tac Γ (value-Type)))
     (define mot (eval-with-context mot-stx Γ))
     (define ⊥-stx (⊥-tac Γ))
     (cons mot (syntax-⊥-elim mot-stx ⊥-stx)))))

;; intros : [ListOf Symbol] ChkTactic -> ChkTactic
;;   Introduces a sequence of symbols using intro and returns the
;;   corresponding check tactic. Represents multi-arity fn. defs.
;;   (lambda (x y z) ...) => (lambda (x) (lambda (y) (lambda (z) ...)))
(define (intros los chk-tc)
  (foldr (λ (e acc) (intro e acc)) chk-tc los))

;; apps : SynTactic [ListOf ChkTactic] -> SynTactic
;;   Uses a SynTactic as the function and applies it to a list of
;;   check tactics representing the arguments. Represents multi-arity
;;   function application.
(define (apps syn-tc loct)
  (foldl (λ (e acc) (app acc e)) syn-tc loct))

;; ℕ-form : SynTactic
(define ℕ-form
  (syn-tactic
   'ℕ-form
   (λ (Γ)
     (cons (value-Type) (syntax-ℕ)))))

;; zero : ChkTactic
(define zero
  (chk-tactic
   'zero
   (λ (Γ goal)
     (match goal
       [(value-ℕ) (syntax-zero)]
       [_ (throw-type-error! "zero\n")]))))

;; suc : ChkTactic -> ChkTactic
;; Successor tactic.
(define (suc n-tac)
  (chk-tactic
   'suc
   (λ (Γ goal)
     (match goal
       [(value-ℕ) (syntax-suc (n-tac Γ (value-ℕ)))]
       [_ (throw-type-error! "suc\n")]))))

;;; type constructor
;; U : SynTactic
(define U
  (syn-tactic
   'U
   (λ (Γ)
     (cons (value-Type) (syntax-Type)))))

;; recℕ : ChkTactic ChkTactic ChkTactic ChkTactic -> SynTactic
;; Type-checks recursion on naturals.
(define/contract (recℕ mot-tac scrut-tac base-tac step-tac)
  (chk-tactic? chk-tactic? chk-tactic? chk-tactic? . -> . syn-tactic?)
  (syn-tactic
   'recℕ
   (λ (Γ)
     (define mot-stx (mot-tac Γ (value-Type)))
     (define mot (eval-with-context mot-stx Γ))
     (define scrut-stx (scrut-tac Γ (value-ℕ)))
     (define base-stx (base-tac Γ mot))
     (define step-stx (step-tac Γ (value-Π '_ mot (h-closure (λ (_) mot)))))
     (cons mot (syntax-recℕ mot scrut-stx base-stx step-stx)))))

;; indℕ : ChkTactic ChkTactic ChkTactic ChkTactic -> SynTactic
;; Type-checks recursion on naturals.
(define/contract (indℕ mot-tac scrut-tac base-tac step-tac)
  (chk-tactic? chk-tactic? chk-tactic? chk-tactic? . -> . syn-tactic?)
  (syn-tactic
   'indℕ
   (λ (Γ)
     (define mot-stx (mot-tac Γ (value-Π 'x (value-ℕ) (h-closure (λ (_) (value-Type))))))
     (define mot (eval-with-context mot-stx Γ))
     (define scrut-stx (scrut-tac Γ (value-ℕ)))
     (define scrut (eval-with-context scrut-stx Γ))
     (define base-stx (base-tac Γ (do-app mot (value-zero))))
     (define step-stx (step-tac Γ (value-Π 'k
                                           (value-ℕ)
                                           (h-closure (λ (k^)
                                                        (value-Π '_
                                                                 (do-app mot k^)
                                                                 (h-closure (λ (_)
                                                                              (do-app mot (value-suc k^))))))))))
     (cons (do-app mot scrut) (syntax-indℕ mot-stx scrut-stx base-stx step-stx)))))

;;;: TYPE-CHECK/TYPE-INFER

;; A ConcreteSyntax is one of:
;; - (cs-var Symbol)
;; - (cs-ann ConcreteSyntax ConcreteSyntax)
;;
;; - (cs-Π Symbol ConcreteSyntax ConcreteSyntax)
;; - (cs-→ ConcreteSyntax ConcreteSyntax)
;; - (cs-lam [ListOf Symbol] ConcreteSyntax)
;; - (cs-app ConcreteSyntax [ListOf ConcreteSyntax])
;;
;; - (cs-× ConcreteSyntax ConcreteSyntax)
;; - (cs-cons ConcreteSyntax ConcreteSyntax)
;; - (cs-fst ConcreteSyntax)
;; - (cs-snd ConcreteSyntax)
;;
;; - (cs-+ ConcreteSyntax ConcreteSyntax)
;; - (cs-inl ConcreteSyntax)
;; - (cs-inr ConcreteSyntax)
;; - (cs-+-elim ConcreteSyntax ConcreteSyntax ConcreteSyntax)
;;
;; - (cs-⊥)
;; - (cs-⊥-elim ConcreteSyntax)
;;
;; - (cs-zero)
;; - (cs-suc ConcreteSyntax)
;; - (cs-num NaturalNumber)
;; - (cs-recℕ ConcreteSyntax ConcreteSyntax ConcreteSyntax ConcreteSyntax)
;; - (cs-indℕ ConcreteSyntax ConcreteSyntax ConcreteSyntax ConcreteSyntax)
;; - (cs-Type)
(struct cs-var (name) #:transparent)
(struct cs-ann (tm tp) #:transparent)

(struct cs-Π (var dom rng) #:transparent)
(struct cs-→ (dom rng) #:transparent)
(struct cs-lam (vars body) #:transparent)
(struct cs-app (rator rands) #:transparent)

(struct cs-× (lhs rhs) #:transparent)
(struct cs-cons (fst snd) #:transparent)
(struct cs-fst (pair) #:transparent)
(struct cs-snd (pair) #:transparent)

(struct cs-+ (left right) #:transparent)
(struct cs-inl (value) #:transparent)
(struct cs-inr (value) #:transparent)
(struct cs-+-elim (mot scrut l-case r-case) #:transparent)

(struct cs-⊥ () #:transparent)
(struct cs-⊥-elim (mot scrut) #:transparent)

(struct cs-ℕ () #:transparent)
(struct cs-zero () #:transparent)
(struct cs-suc (n) #:transparent)
(struct cs-num (n) #:transparent)
(struct cs-recℕ (mot scrut base step) #:transparent)
(struct cs-indℕ (mot scrut base step) #:transparent)

(struct cs-Type () #:transparent)

;; type-check: ConcreteSyntax -> ChkTactic
;; Receives a ConcreteSyntax and produces its corresponding check tactic.
(define (type-check cs)
  (match cs
    [(cs-cons fst snd) (cons^ (type-check fst) (type-check snd))]
    [(cs-inl value) (inl (type-check value))]
    [(cs-inr value) (inr (type-check value))]
    [(cs-lam lovars body) (intros lovars (type-check body))]
    [(cs-zero) zero]
    [(cs-suc n) (suc (type-check n))]
    [(cs-num n)
     #:when (and (integer? n) (or (zero? n) (positive? n)))
     (define (n->chk n)
       (if (zero? n) zero (suc (n->chk (sub1 n)))))
     (n->chk n)]
    [else (chk (type-infer cs))]))

;; type-infer: ConcreteSyntax -> SynTactic
;; Receives a ConcreteSyntax and produces its corresponding synthesis tactic.
(define (type-infer cs)
  (match cs
    [(cs-var name) (var name)]
    [(cs-Π var dom rng) (Π-form var (type-check dom) (type-check rng))]
    [(cs-→ dom rng) (Π-form '_ (type-check dom) (type-check rng))]
    [(cs-fst pair) (fst (type-infer pair))]
    [(cs-snd pair) (snd (type-infer pair))]
    [(cs-+-elim mot scrut l-case r-case)
     (+-elim (type-check mot)
             (type-infer scrut)
             (type-check l-case)
             (type-check r-case))]
    [(cs-app rator lorands) (apps (type-infer rator) (map type-check lorands))]
    [(cs-ann tm tp)
     (ann (type-check tm)
          (type-check tp))]
    [(cs-⊥-elim mot target) (⊥-elim (type-check mot) (type-infer target))]
    [(cs-⊥) ⊥-form]
    [(cs-ℕ) ℕ-form]
    [(cs-× fst snd) (×-form (type-check fst) (type-check snd))]
    [(cs-+ left right) (+-form (type-check left) (type-check right))]
    [(cs-recℕ mot scrut base step)
     (recℕ (type-check mot)
           (type-check scrut)
           (type-check base)
           (type-check step))]
    [(cs-indℕ mot scrut base step)
     (indℕ (type-check mot)
           (type-check scrut)
           (type-check base)
           (type-check step))]
    [(cs-Type) U]
    [else (throw-type-error! "type-infer: invalid concrete syntax ~a" cs)]))


