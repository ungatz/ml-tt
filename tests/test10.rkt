#lang mltt

(define ¬
  (: (λ (A) (→ A ⊥))
     (→ Type Type)))

(define lem-not-false
  (: (λ (A lem-false)
       (lem-false
        (inr
         (λ (x)
           (lem-false (inl x))))))
     (Π ([A Type])
        (¬ (¬ (+ A (¬ A)))))))

;; lmao i can't come up with any better examples for this
(define bot-test
  (: (λ (A b) (⊥-elim A b))
     (Π ([A Type])
        (→ ⊥ A))))

(fail
 (: (λ (A b) (⊥-elim A b))
    (Π ([A Type])
       (→ A ⊥))))
