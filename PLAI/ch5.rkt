#lang plai-typed
(define-type FunDefC
      [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
      [numC (n : number)]
      [idC (s : symbol)]  ;标识符
      [appC (fun : symbol) (arg : ExprC)]  ;调用
      [plusC (l : ExprC) (r : ExprC)]
      [multC (l : ExprC) (r : ExprC)])

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]  ;解释标识符子句
    [appC (f a) (local ([define fd (get-fundef f fds)]) ;; 这里其实就是递归的替换所有变量并求值，即ｓｉｃｐ上的正则序，ｌａｍｂｄａ验算应该也有讲到替换
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]  ;解释调用子句
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]　;;函数体为数值，直接返回
    [idC (s) (cond　;;函数体为标识符，等于ａｒｇｓ则返回ａｒｇｓ，否则返回ｉｎ
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

;获取函数定义
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]  ;引用未定义的函数
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)] ;;first和rest就是car,cdr...
                   [else (get-fundef n (rest fds))])]))


(interp (appC 'quadruple (numC 3))
                  (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
                        (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))))