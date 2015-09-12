#lang plai

;;Definir any para que se puedan hacer estructuras de todo tipo
(define (any? x) #t)

;;Sección 1
;;01.- Definir Array
(define-type Array
  [MArray (n any?) 
          (lst list?)])

;;02.- Definir List
(define-type MList
  [MEmpty]
  [MCons (n any?)
         (lst MList?)])

;;03.- Definir NTree
(define-type NTree
  [TLEmpty]
  [NodeN (n number?) (list list?)])

;;04.- Definir Position
(define-type Position
  [2D-Point (primero number?)
            (segundo number?)])

;;05.- Definir Figure
(define-type Figure
  [Circle (centro Position?)
          (radio number?)]
  [Square (esquinasi Position?)
          (longitudlado number?)]
  [Rectangle (esquinasi Position?)
             (ancho number?)
             (largo number?)])

;;Sección 2
;;06.- Definir setvalueA
(define setvalueA
  (lambda (arre pos num)
    (type-case Array arre
      [MArray(n l)
             (cond
               [(<= n pos) error ". . setvalueA: Out of bounds"]
               [else (MArray n (MReplace l pos num))])])))
;;Función auxiliar MReplace
;;Se le pasa una lista, una posición y un valor, cambia el valor que haya en la posición que se pasa por el valor.
(define MReplace
  (lambda (lst pos num)
    (cond
      [(null? lst) lst]
      [(zero? pos) (cons num (cdr lst))]
      [else (cons (first lst)(MReplace (cdr lst) (- pos 1) num))])))

;;07.- Definir MArray2MList
(define MArray2MList
  (lambda (arre)
    (type-case Array arre
      [MArray(n l)
             (cond
               [(zero? n) (MEmpty)]
               [(null? l) (MEmpty)]
               [else (MCons (first l) (MArray2MList (MArray (- n 1) (cdr l))))])])))

;;08.- Definir printML
(define printML
  (lambda (lst)
    (~a "[" (restoDelPrint lst))))

(define restoDelPrint
  (lambda (lst)
    (type-case MList lst
      [MEmpty () "]"]
      [MCons (x xs)
             (cond
               [(MEmpty? xs) (~a x "]")]
               [else (~a (~a x ",") (restoDelPrint xs))])])))

;;09.- concatML
(define concatML
  (lambda (lst1 lst2)
    (type-case MList lst1
      [MEmpty () lst2]
      [MCons(x xs)
            (cond
              [(MEmpty? lst1) lst2]
              [else (MCons x (concatML xs lst2))])])))

;;10.- Definir lenghtML
(define lengthML
  (lambda (lst)
    (type-case MList lst
      [MEmpty () 0]
      [MCons (x xs)
             (cond
               [(MEmpty? lst) 0]
               [else (+ 1 (lengthML xs))])])))

;;11.- Definir mapML
(define mapML
  (lambda (lst funcion)
    (type-case MList lst
      [MEmpty () MEmpty]
      [MCons (x xs)
             (cond
             [MCons (funcion x) (mapML xs funcion)])])))
             
;;12.- Definir filterML
(define filterML
  (lambda (f l)
    (cond
      [(MEmpty? l) (MEmpty)]
      [(if (f(MCons-n l)) (MCons (MCons-n l) (filterML f (MCons-lst l))) (filterML f (MCons-lst l)))])))

;;17.- Definir area
(define (area figure)
  (type-case Figure figure
    [Circle (c r)
            (* pi (expt r 2))]
    [Square (e l)
            (* l l)]
    [Rectangle (e a l)
               (* a l)]))

;;18.- Definir in-figure?
(define (in-figure? fig point)
(type-case Figure fig
  [Circle (c r)
          (if(<= (+ (expt(- (2D-Point-primero c)(2D-Point-primero point)) 2)
                    (expt(- (2D-Point-segundo c)(2D-Point-segundo point)) 2))
                 (* r r)) #t #f)]
  [Square (e l)
          (if (and (and (>= (2D-Point-primero point) (2D-Point-primero e))
                        (<= (2D-Point-primero point) (+ l (2D-Point-primero e))))
                   (and (>= (2D-Point-segundo point) (2D-Point-segundo e))
                        (<= (2D-Point-segundo point) (+ l (2D-Point-segundo e))))) #t #f)]
  [Rectangle (e l a)
             (if(and
                 (and (>= (2D-Point-primero point) (2D-Point-primero e))
                      (<= (2D-Point-primero point) (+ l (2D-Point-primero e)) ))
                 (and (>= (2D-Point-segundo point) (2D-Point-segundo e))
                      (<= (2D-Point-segundo point) (+ a (2D-Point-segundo e)))))
                #t #f)]))
