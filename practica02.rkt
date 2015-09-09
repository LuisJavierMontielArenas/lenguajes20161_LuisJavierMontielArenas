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
  (cond
    [(null? arre) ". . setvalueA: Out of bounds"]
    [(> pos numElementos(arre)) ". . setvalueA: Out of bounds"])))

(define numElementos
  (lambda (lista)
    (cond
     [(null? lista) 0]
     [else (+ 1 (numElementos (cdr lista)))])))
