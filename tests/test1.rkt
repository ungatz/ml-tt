#lang mltt

(define plus
  (: (λ (m n)
       (indℕ (λ (_) ℕ)
             m
             n
             (λ (_ x) (suc x))))
     (→ ℕ (→ ℕ ℕ))))

(plus 4 4)
(normalize (plus 4 4))
