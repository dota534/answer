#lang plai-typed

(define l '(+ 1 2))

(define f (first (s-exp->list l)))

f

(symbol->string (s-exp->symbol f))



(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])


(define (parse [s : s-expression])
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]  ;无效的表输入
    [else (error 'parse "invalid input")]))  ;无效的输入


(parse '(+ (* 1 2) (+ 2 3)))