#lang mltt

(define Unit
  (Π ([A Type]) (→ A A)))
(define sole (: (λ (A a) a) Unit))

(define Maybe
  (: (λ (A) (+ A Unit))
     (→ Type Type)))
(define just
  (: (λ (_ x) (inl x))
     (Π ([A Type])
        (→ A (Maybe A)))))
(define nothing
  (: (λ (_) (inr sole))
     (Π ([A Type]) (Maybe A))))

(define Fin
  (: (λ (n)
       (indℕ (λ (_) Type)
             n
             ⊥
             (λ (_ x) (Maybe x))))
     (→ ℕ Type)))

(define fromℕ
  (: (λ (n)
       (indℕ (λ (x) (Fin (suc x)))
             n
             (inr sole)
             (λ (_ x) (inl x))))
     (Π ([n ℕ]) (Fin (suc n)))))

(fromℕ 3)
(normalize (fromℕ 3))
