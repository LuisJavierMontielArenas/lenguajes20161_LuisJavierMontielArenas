#lang plai
;Ejercicio 01.- Función pow y pruebas
(define pow
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (* n (pow n (- m 1)))])))

;(test (pow 100000 0) 1)
;(test (pow 100000 1) 100000)
;(test (pow 5 2) 25)
;(test (pow 1 100) 1)
;(test (pow 5 5) 3125)

;Ejercicio 02.- Función average
;Implementamos las funciones auxiliares de sumaElementos y numElementos para sacar el promedio
(define sumaElementos
  (lambda (lista)
    (cond
      [(null? lista) 0]
      [else (+ (first lista) (sumaElementos (cdr lista)))])))

(define numElementos
  (lambda (lista)
    (cond
     [(null? lista) 0]
     [else (+ 1 (numElementos (cdr lista)))])))

(define average
  (lambda (lista)
    (cond
      [(null? lista) 0]
      [else (/ (sumaElementos lista) (numElementos lista))])))

;(test (average '()) 0)
;(test (average '(10)) 10)
;(test (average '(10 10 10 20 20 20)) 15)
;(test (average '(1 2 3 4 5 6 7 8 9 10)) 5.5)
;(test (average '(1 1 1 1 1 1 1 1 1 1 1 1)) 1)

;Ejercicio 03.- Función primes


;Ejercicio 04.- Función zip
(define zip
  (lambda (lista1 lista2)
    (cond
      [(null? lista1) '()]
      [(null? lista2) '()]
      [else (cons (list (first lista1) (first lista2)) (zip (cdr lista1) (cdr lista2)))])))

;Ejercicio 05.- Función reduce
(define reduce
  (lambda (funcion lista)
    (cond
      [(null? lista) lista]
      [(symbol=? (numElementos lista) 1) (first lista)]
      [else (funcion (funcion (first lista) (second lista)) (reduce funcion (cdr (cdr lista))))])))

;Sección II
;Ejercicio 06.- Función mconcat
(define mconcat
  (lambda (lista1 lista2)
    (cond
      [(null? lista1) lista2]
      [else (cons (car lista1) (mconcat (cdr lista1) lista2))])))

;Ejercicio 07.- Función mmap
(define mmap
  (lambda (funcion lista)
    (cond
      [(null? lista) '()]
      [else (cons (funcion (car lista)) (mmap funcion (cdr lista)))])))


;Ejercicio 08.- Función mfilter
(define mfilter
  (lambda (predicate lista)
    (cond
      [(null? lista) '()]
      )))
