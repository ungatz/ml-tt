#lang mltt

;; primitive recursion? pfff

(define repeat
  (: (λ (f n)
       (indℕ (λ (_) ℕ)
             n
             (f 1)
             (λ (_ k) (f k))))
     (→ (→ ℕ ℕ) (→ ℕ ℕ))))

(define ack
  (: (λ (n)
       (indℕ (λ (_) (→ ℕ ℕ))
             n
             (λ (x) (suc x))
             (λ (_ k) (repeat k))))
     (→ ℕ (→ ℕ ℕ))))

(normalize (ack 3 3))
