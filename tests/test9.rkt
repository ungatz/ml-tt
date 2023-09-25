#lang mltt

(define ¬
  (: (λ (A) (→ A ⊥))
     (→ Type Type)))

(define iii
  (: (λ (A B n)
       (cons (λ (a) (n (inl a)))
             (λ (b) (n (inr b)))))
     (Π ([A Type])
        (Π ([B Type])
           (→ (¬ (+ A B)) (× (¬ A) (¬ B)))))))

(define v
  (: (λ (A B f ¬b a)
       (¬b (f a)))
     (Π ([A Type])
        (Π ([B Type])
           (→ (→ A B) (→ (¬ B) (¬ A)))))))

(fail
 (: (λ (A B f ¬b a)
      (¬b (f a)))
    (Π ([A Type])
       (Π ([B Type])
          (→ (→ A B) (→ (¬ B) (¬ B)))))))

(fail
 (: (λ (A B f ¬b a)
      (¬b (a f)))
    (Π ([A Type])
       (Π ([B Type])
          (→ (→ A B) (→ (¬ B) (¬ A)))))))

(fail
 (: (λ (A B f ¬b a)
      (f (¬b a)))
    (Π ([A Type])
       (Π ([B Type])
          (→ (→ A B) (→ (¬ B) (¬ A)))))))
