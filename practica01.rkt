#lang plai
;Ejercicio 01.- Función pow y pruebas
(define pow
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (* n (pow n (- m 1)))])))

(test (pow 100000 0) 1)
(test (pow 100000 1) 100000)
(test (pow 5 2) 25)
(test (pow 1 100) 1)
(test (pow 5 5) 3125)

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

(test (average '()) 0)
(test (average '(10)) 10)
(test (average '(10 10 10 20 20 20)) 15)
(test (average '(1 2 3 4 5 6 7 8 9 10)) 5.5)
(test (average '(1 1 1 1 1 1 1 1 1 1 1 1)) 1)

;Ejercicio 03.- Función primes
;Implementamos las funciones auxiliares isPrime, primAux y reverse, que son las que hacen que funcion la funcion primes
(define primes
  (lambda (n)
    (primAux n 2 '())))

(define primAux
  (lambda (n m acc)
    (cond
      [(= (+ n 1) m) (reverse acc '())]
      [(isPrime 2 m) (primAux n (+ m 1) (cons m acc))]
      [else (primAux n (+ m 1) acc)])))

(define isPrime
  (lambda (n p)
    (cond
      [(= n p) #t]
      [(= (modulo p n) 0) #f]
      [else (isPrime (+ n 1) p)])))

(define reverse
  (lambda (lista acc)
    (cond
      [(null? lista) acc]
      [else (reverse (cdr lista) (cons (car lista) acc))])))

<<<<<<< Updated upstream
(test (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(test (primes 11) '(2 3 5 7 11))
(test (primes 1) '())
(test (primes 2) '(2))
(test (primes 50) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
=======
>>>>>>> Stashed changes

;Ejercicio 04.- Función zip
(define zip
  (lambda (lista1 lista2)
    (cond
      [(null? lista1) '()]
      [(null? lista2) '()]
      [else (cons (list (first lista1) (first lista2)) (zip (cdr lista1) (cdr lista2)))])))
<<<<<<< Updated upstream

(test (zip '(1 2) '(3 4)) '((1 3) (2 4)))
(test (zip '(1 2 3) '()) '())
(test (zip '() '(4 5 6)) '())
(test (zip '(8 9) '(3 2 1 4)) '((8 3) (9 2)))
(test (zip '(8 9 1 2) '(3 4)) '((8 3) (9 4)))      

;Ejercicio 05.- Función reduce
(define reduce (lambda (funcion lista base)
                 (cond
                   [(null? lista) base]
                   [(eqv? (numElementos lista) 1) (first lista)]
                   [else (funcion (first lista) (reduce funcion (cdr lista) base))])))

(test (reduce + '(1 2 3 4 5 6 7 8 9 10) 0) 55)
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9)) '()) '((1 (4 7)) (2 (5 8)) (3 (6 9))))
(test (reduce * '(1 2 3 4 5) 0) 120)
(test (reduce + '(10 10 10 10 10 10 10 10 10 10) 0) 100)
(test (reduce mconcat '(() (1 2) (a b)) '()) '(1 2 a b))
=======

;Ejercicio 05.- Función reduce
(define reduce
  (lambda (funcion lista)
    (cond
      [(null? lista) lista]
      [(symbol=? (numElementos lista) 1) (first lista)]
      [else (funcion (funcion (first lista) (second lista)) (reduce funcion (cdr (cdr lista))))])))
>>>>>>> Stashed changes

;Sección II
;Ejercicio 06.- Función mconcat
(define mconcat
  (lambda (lista1 lista2)
    (cond
      [(null? lista1) lista2]
      [else (cons (car lista1) (mconcat (cdr lista1) lista2))])))

<<<<<<< Updated upstream
(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '(4 5 6)) '(4 5 6))
(test (mconcat '(1 2 3) '()) '(1 2 3))
(test (mconcat '() '()) '())
(test (mconcat '(1 2 3 4) '(a b c d)) '(1 2 3 4 a b c d))

=======
>>>>>>> Stashed changes
;Ejercicio 07.- Función mmap
(define mmap
  (lambda (funcion lista)
    (cond
      [(null? lista) '()]
      [else (cons (funcion (car lista)) (mmap funcion (cdr lista)))])))
<<<<<<< Updated upstream
(test (mmap add1 '(1 2 3 4)) '(2 3 4 5))
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
=======

>>>>>>> Stashed changes
(test (mmap add1 '()) '())
(test (mmap add1 '(1000000)) '(1000001))

;Ejercicio 08.- Función mfilter
(define mfilter
  (lambda (fun lista)
    (cond
      [(null? lista) '()]
      [(fun (car lista)) (cons (car lista ) (mfilter fun (cdr lista)))]
      [else (mfilter fun (cdr lista))])))

;;Pruebas con la funcion mfilter
(test (mfilter (lambda (x) (not (zero? x))) '(2 0 1 4 0)) '(2 1 4))
(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))
(test (mfilter (lambda (n) (= (modulo n 2) 0)) '(1 2 3 4 5 6)) '(2 4 6))
(test (mfilter (lambda (k) (not (number? k))) '()) '())
(test (mfilter (lambda (a) (zero? a)) '(1 2 3 4 0 12)) '(0))

<<<<<<< Updated upstream
;Ejercicio 09.- Función any? y pruebas
(define any?
  (lambda (propocision lista)
    (cond 
      [(null? lista) #f]
      [(propocision (first lista)) #t]
      [else (any? propocision (cdr lista))])))
      
(test (any? number? '()) #f)
(test (any? number? '(a b c d 1)) #t)
(test (any? symbol? '(1 2 3 4)) #f)
(test (any? symbol? '(1 2 3 4 q 1 2)) #t)
(test (any? number? '(a b c 0 d e f)) #t)

;Ejercicio 10.- Función every? y pruebas
(define every?
  (lambda (propocision lista)
    (cond
      [(null? lista) #t]
      [(propocision (first lista)) (every? propocision (cdr lista))]
      [else #f])))
(test (every? number? '()) #t)
(test (every? number? '(1 2 3)) #t)
(test (every? number? '(1 2 3 a)) #f)
(test (every? symbol? '(1 2 3 a)) #f)
(test (every? symbol? '(a b c d e f g h)) #t)

;Ejercicio 11.- Función mpowerset
=======
;;Ejercicio 11.- Función mpowerset
>>>>>>> Stashed changes

(define mpowerset
  (lambda (lista)
    (cond
      [(null? lista) '(())]
<<<<<<< Updated upstream
      [else (let ((cdr (mpowerset (cdr lista)))) (mconcat (mmap ( lambda (listatemp) (cons (car lista) listatemp)) cdr) cdr))])))
=======
      [else (let ((cdr (mpowerset (cdr lista)))) (mconcat (mmap (lambda (listatemp) (cons (car lista) listatemp)) cdr) cdr))])))
>>>>>>> Stashed changes

;;Pruebas de la función mpowerset
(test (mpowerset '()) '(()))
(test (mpowerset '(1)) '((1) ()))
(test (mpowerset '(1 2)) '((1 2) (1) (2) ()))
(test (mpowerset '(1 2 3)) '((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ()))
(test (mpowerset '((1 2 3) 1 2 3)) '(((1 2 3) 1 2 3) ((1 2 3) 1 2) ((1 2 3) 1 3) ((1 2 3) 1) ((1 2 3) 2 3) ((1 2 3) 2) ((1 2 3) 3) ((1 2 3)) (1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ()))
