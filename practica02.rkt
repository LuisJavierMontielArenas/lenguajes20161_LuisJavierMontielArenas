#lang plai

(define-type Array
  [MArray (n number?) 
          (lst list?)])

(define-type MList
  [MEmpty]
  [MCons (n number?)
         (lst MList?)])

(define-type NTree
  [TLEmpty]
  [NodeN (n number?) (list list?)])

(define-type Position
  [2D-Point (primero number?)
            (segundo number?)])

(define-type Figure
  [Circle (centro Position?)
          (radio number?)]
  [Square (esquinasi Position?)
          (longitudlado number?)]
  [Rectangle (esquinasi Position?)
             (ancho number?)
             (largo number?)])
             
(define setvalueA
  (lambda (arre pos num)
    (type-case Array arre
      [MArray(n l)
             (cond
               [(<= n pos) error ". . setvalueA: Out of bounds"]
               [else (MArray n (MReplace l pos num))])])))

(define MReplace
  (lambda (lst pos num)
    (cond
      [(null? lst) lst]
      [(zero? pos) (cons num (cdr lst))]
      [else (cons (first lst)(MReplace (cdr lst) (- pos 1) num))])))

(define MArray2MList
  (lambda (arre)
    (type-case Array arre
      [MArray(n l)
             (cond
               [(zero? n) (MEmpty)]
               [(null? l) (MEmpty)]
               [else (MCons (first l) (MArray2MList (MArray (- n 1) (cdr l))))])])))
