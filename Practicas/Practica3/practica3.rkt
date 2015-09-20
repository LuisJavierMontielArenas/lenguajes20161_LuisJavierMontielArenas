#lang plai

(require "practica3-base.rkt")

;;Sección 1
;;Ejercicio 1.- Zones
(define zones
  (lambda (rest max)
    (define range (- max rest))
    (define (min i rest range) (let ([x (+ rest (* range (+ 0.5 (* 0.1 i))))]) x))
    (define (maxi i rest range) (let ([x (- (+ rest (* range (+ 0.5 (* 0.1 (+ i 1))))) 1)]) x))
    (define r (resting rest (+ (- (* range 0.5) 1) rest)))
    (define w (warm-up
               (min 0 rest range)
               (maxi 0 rest range)))
    (define f (fat-burning
               (min 1 rest range)
               (maxi 1 rest range)))
    (define ae (aerobic
                (min 2 rest range)
                (maxi 2 rest range)))
    (define an (anaerobic
                (min 3 rest range)
                (maxi 3 rest range)))
    (define m (maximum
               (min 4 rest range)
               (+ 1 (maxi 4 rest range))))
    (list r w f ae an m)))
         
(define my-zones (zones 50 180))

;;Ejercicio 2.- Get-zone
(define get-zone
  (lambda (simbolo lista)
    (cond
      [(null? lista) error "Lista vacía"]
      [(equal? simbolo 'resting) (list-ref lista 0)]
      [(equal? simbolo 'warm-up) (list-ref lista 1 )]
      [(equal? simbolo 'fat-burning) (list-ref lista 2)]
      [(equal? simbolo 'aerobic) (list-ref lista 3 )]
      [(equal? simbolo 'anaerobic) (list-ref lista 4)] ;;list-ref regresa el elemnto de "lista" en la posición dada.
      [(equal? simbolo 'maximum) (list-ref lista 5)]
      [else (error "No se encontró el simbolo")]))) ;;Si la lista tiene menos elementos, se lanza una excepción, o un error en caso que no se encuentre el simbolo en la lista.

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

;;nnBT
(define nnBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () 0]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) 1]
               [else (+ 1 (+ (nnBT left) (nnBT right)))])])))
