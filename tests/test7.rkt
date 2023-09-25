#lang mltt

(fail
 (: (λ (A) (λ (x) x))
    (Π ([A Type]) (→ A (→ A A)))))

(fail
 (: (λ (A x) A)
    (Π ([A Type]) (→ A A))))
