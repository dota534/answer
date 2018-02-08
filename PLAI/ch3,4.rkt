#lang plai-typed

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

(define-type ArithC  ;具体算术
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])


(define (interp [a : ArithC]) : number
  (type-case ArithC a
             [numC (n) n]
             [plusC (l r) (+ (interp l) (interp r))]
             [multC (l r) (* (interp l) (interp r))]))

(interp (parse '(+ (* 1 2) (+ 2 3))))


(define-type ArithS  ;表层算术
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)])

(define (desugar [as : ArithS]) : ArithC
      (type-case ArithS as
        [numS (n) (numC n)]
        [plusS (l r) (plusC (desugar l)
                            (desugar r))]
        [multS (l r) (multC (desugar l)
                            (desugar r))]
        [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
        [uminusS (e) (desugar (bminusS (numS 0) e))]))  ;二元减法子句

