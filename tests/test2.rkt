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
