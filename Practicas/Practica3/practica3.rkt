#lang plai

(require "practica3-base.rkt")

;;ninBT
(define ninBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () 0]
      [BNode (comp left elem right) 
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) 0]
               [else (+ 1 (+ (ninBT left) (ninBT right)))])])))

;;Pruebas de la función ninBT
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (EmptyBT) 1 (EmptyBT))) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (bns (bns (bns ebt "pow" ebt) "ext" (bns ebt "trial" ebt)) "typed" (bns ebt "lambda" ebt))) 2)
(define example1 (bns (bns (bns ebt "pow" ebt) "ext" (bns ebt "trial" ebt)) "typed" (bns ebt "lambda" ebt)))
(define bigger-example (bns example1 "functional" example1))
(test (ninBT bigger-example) 5)

;;nlBT
(define nlBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () 0]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) 1]
               [else (+ (nlBT left) (nlBT right))])])))
