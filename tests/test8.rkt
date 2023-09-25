#lang mltt

(define i
  (: (λ (A B C s)
       (+-elim (× (+ A C) (+ B C))
               s
               (λ (p) (cons (inl (fst p)) (inl (snd p))))
               (λ (c) (cons (inr c) (inr c)))))
     (Π ([A Type])
        (Π ([B Type])
           (Π ([C Type])
              (→ (+ (× A B) C)
                 (× (+ A C) (+ B C))))))))

(define ii
  (: (λ (A B C p)
       (+-elim (+ (× A C) (× B C))
               (fst p)
               (λ (a) (inl (cons a (snd p))))
               (λ (b) (inr (cons b (snd p))))))
     (Π ([A Type])
        (Π ([B Type])
           (Π ([C Type])
              (→ (× (+ A B) C)
                 (+ (× A C) (× B C))))))))

(fail
 (: (λ (A B C p)
      (+-elim (+ (× A C) (× B C))
              (fst p)
              (λ (a) (inl (cons a (snd p))))
              (λ (b) (inr (cons b (snd p))))))
    (Π ([A Type])
       (Π ([B Type])
          (Π ([C Type])
             (→ (× (+ A B) C)
                (+ (× A C) (× C C))))))))

(fail
 (: (λ (A B C p)
      (+-elim (+ (× A C) (× C C))
              (fst p)
              (λ (a) (inl (cons a (snd p))))
              (λ (b) (inr (cons b (snd p))))))
    (Π ([A Type])
       (Π ([B Type])
          (Π ([C Type])
             (→ (× (+ A B) C)
                (+ (× A C) (× B C))))))))

(fail
 (: (λ (A B C p)
      (+-elim (+ (× A C) (× C C))
              (fst A)
              (λ (a) (inl (cons a (snd p))))
              (λ (b) (inr (cons b (snd p))))))
    (Π ([A Type])
       (Π ([B Type])
          (Π ([C Type])
             (→ (× (+ A B) C)
                (+ (× A C) (× B C))))))))
