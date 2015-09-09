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
