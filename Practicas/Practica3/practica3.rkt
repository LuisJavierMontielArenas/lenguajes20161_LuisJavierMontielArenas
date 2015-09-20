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
