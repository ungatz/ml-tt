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

(define Vect
  (: (λ (A k) (→ (Fin k) A))
     (→ Type (→ ℕ Type))))

(define nats
  (: (λ (fin3)
       (+-elim ℕ
               fin3
               (λ (fin2)
                 (+-elim ℕ
                         fin2
                         (λ (_) 888)
                         (λ (_) 88)))
               (λ (_) 8)))
     (Vect ℕ 3)))

(normalize nats)
