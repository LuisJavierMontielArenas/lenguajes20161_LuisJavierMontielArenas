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
    
;;Test de zones
(test (zones 50 180) (list (resting 50 114.0)
                           (warm-up 115.0 127.0)
                           (fat-burning 128.0 140.0)
                           (aerobic 141.0 153.0)
                           (anaerobic 154.0 166.0)
                           (maximum 167.0 180.0)))
      
(test (zones 80 120) (list (resting 80 99.0)
                           (warm-up 100.0 103.0)
                           (fat-burning 104.0 107.0)
                           (aerobic 108.0 111.0)
                           (anaerobic 112.0 115.0)
                           (maximum 116.0 120.0)))

(test (zones 90 160) (list (resting 90 124.0)
                           (warm-up 125.0 131.0)
                           (fat-burning 132.0 138.0)
                           (aerobic 139.0 145.0)
                           (anaerobic 146.0 152.0)
                           (maximum 153.0 160.0)))

(test (zones 110 200) (list (resting 110 154.0)
                            (warm-up 155.0 163.0)
                            (fat-burning 164.0 172.0)
                            (aerobic 173.0 181.0)
                            (anaerobic 182.0 190.0)
                            (maximum 191.0 200.0)))

(test (zones 50 100) (list (resting 50 74.0)
                           (warm-up 75.0 79.0)
                           (fat-burning 80.0 84.0)
                           (aerobic 85.0 89.0)
                           (anaerobic 90.0 94.0)
                           (maximum 95.0 100.0)))

(define my-zones (zones 50 180))

;;Ejercicio 2.- Get-zone
(define get-zone
  (lambda (simbolo lista)
    (cond
      [(null? lista) error "Lista vacía"]
      [(equal? simbolo 'resting) (list-ref lista 0)] ;;list-ref regresa el elemnto de "lista" en la posición dada.
      [(equal? simbolo 'warm-up) (list-ref lista 1 )]
      [(equal? simbolo 'fat-burning) (list-ref lista 2)]
      [(equal? simbolo 'aerobic) (list-ref lista 3 )]
      [(equal? simbolo 'anaerobic) (list-ref lista 4)]
      [(equal? simbolo 'maximum) (list-ref lista 5)]
      [else (error "No se encontró el simbolo")]))) ;;Si la lista tiene menos elementos, se lanza una excepción, o un error en caso que no se encuentre el simbolo en la lista.

;;Test de Get-zone
(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))


;;Sección 2
;;Árbol que se usará en varias pruebas
(define example1 (bns (bns (bns ebt "pow" ebt) "ext" (bns ebt "trial" ebt)) "typed" (bns ebt "lambda" ebt)))
(define bigger-example (bns example1 "functional" example1))

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
;;Pruebas de la función nlBT
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (EmptyBT) 1 (EmptyBT))) 1)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT bigger-example) 6)
(test (nlBT (bns (bns (bns ebt "pow" ebt) "ext" (bns ebt "trial" ebt)) "typed" (bns ebt "lambda" ebt))) 3)


;;nnBT
(define nnBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () 0]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) 1]
               [else (+ 1 (+ (nnBT left) (nnBT right)))])])))
;;Pruebas de nnBT
(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT bigger-example) 11)
(test (nnBT (bns (bns (bns ebt "pow" ebt) "ext" (bns ebt "trial" ebt)) "typed" (bns ebt "lambda" ebt))) 5)
(test (nnBT (BNode < (EmptyBT) 1 (EmptyBT))) 1)

;;mapBT
(define mapBT
  (lambda (funct tree)
    (type-case BTree tree
      [EmptyBT () tree]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) (BNode comp left (funct elem) right)]
               [else (BNode comp (mapBT funct left) (funct elem) (mapBT funct right))])])))
;;Nota, el árbol que se regresa en vez de tener ">", tiene "#<procedure:>>", y en vez de tener "string<?", tiene "#<procedure:string<?>"
(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT (lambda (x) (- x 1)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 1 (EmptyBT))))
(test (mapBT (lambda (x) (* x 100)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 300 (BNode < (EmptyBT) 200 (EmptyBT))))

;;preorderBT
(define preorderBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () '()]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) (cons elem '())]
               [else (cons elem (append (preorderBT left) (preorderBT right)))])])))
               
;;inorderBT
(define inorderBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () '()]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) (cons elem '())]
               [else (append (inorderBT left) (cons elem '()) (inorderBT right))])])))
               
;;postorderBT
(define postorderBT
  (lambda (tree)
    (type-case BTree tree
      [EmptyBT () '()]
      [BNode (comp left elem right)
             (cond
               [(and (EmptyBT? left) (EmptyBT? right)) (cons elem '())]
               [else (append (postorderBT left) (postorderBT right) (cons elem '()))])])))
