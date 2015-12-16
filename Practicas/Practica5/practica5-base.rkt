#lang plai

(define-type RCFAEL
  [id (name symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [rec (bindings (listof bind?)) (body RCFAEL?)]
  [fun (params (listof symbol?)) (body RCFAEL?)]
  [if0 (test-exp RCFAEL?) (then-exp RCFAEL?) (else-exp RCFAEL?)]
  [isequal? (l RCFAEL?) (r RCFAEL?)]
  [op (f procedure?) (expr RCFAEL?)]
  [binop (f procedure?) (l RCFAEL?) (r RCFAEL?)]
  [opB (f procedure?) (o RCFAEL?)]
  [binopB (f procedure?) (l RCFAEL?) (r RCFAEL?)]
  [app (fun RCFAEL?) (args (listof RCFAEL?))])

(define-type RCFAELS
  [idS (name symbol?)]
  [numS (n number?)]
  [boolS (b boolean?)]
  [withS (bindings (listof bindS?)) (body RCFAELS?)]
  [recS (bindings (listof bindS?)) (body RCFAELS?)]
  [funS (params (listof symbol?)) (body RCFAELS?)]
  [if0S (test-exp RCFAELS?) (then-exp RCFAELS?) (else-exp RCFAELS?)]
  [isequal?S (a RCFAELS?) (b RCFAELS?)]
  [opS (f procedure?) (expr RCFAELS?)]
  [binopS (f procedure?) (l RCFAELS?) (r RCFAELS?)]
  [opBS (f procedure?) (expr RCFAELS?)]
  [binopBS (f procedure?) (l RCFAELS?) (r RCFAELS?)]
  [appS (fun RCFAELS?) (args (listof RCFAELS?))])

(define-type Binding
  [bindS (name symbol?) (val RCFAELS?)]
  [bind (name symbol?) (val RCFAEL?)])

(define-type RCFAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [closureV (param (listof symbol?))
            (body RCFAEL?)
            (env Env?)])

(define (boxed-RCFAEL-Value? v)
  (and (box? v) (RCFAEL-Value? (unbox v))))

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value RCFAEL-Value?) 
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAEL-Value?)
           (env Env?)])

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (boolean? bindRep)
        (map (lambda (b) (bindS (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))
  
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))


(define (proced s)
  (case s
    [(inc) add1]
    [(dec) sub1]
    [(zero?) zero?]
    [(num?) number?]
    [(neg) not]
    [(bool?) boolean?]
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) quotient]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) And]
    [(or) Or]))

(define (And x y) (and x y))
(define (Or x y) (or x y))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> FAES
(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(eq? sexp 'true) (boolS true)]
    [(eq? sexp 'false) (boolS false)]
    [(symbol? sexp) (idS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(with) (if (= 3 (length sexp))
                   (withS (parse-bindings (cadr sexp)) (parse (caddr sexp)))
                   (error "Sintaxis de with incorrecta."))]
       [(rec) (if (= 3 (length sexp))
                  (recS (parse-bindings (cadr sexp)) (parse (caddr sexp)))
                  (error "Sintaxis de rec incorrecta."))]
       [(fun) (if (< 1 (length sexp))
                  (funS (cadr sexp) (parse (caddr sexp)))
                  (error "Sintaxis de fun incorrecta."))]
       [(if) (if (= 4 (length sexp))
                  (if0S (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))
                  (error "Sintaxis de if incorrecta."))]
       [(isequal? =)
        (if (= 3 (length sexp))
            (isequal?S (parse (cadr sexp)) (parse (caddr sexp)))
            (error "Sintaxis de operador isequal? incorrecta."))]
       [(inc dec)
        (if (= 2 (length sexp))
            (opS (proced (car sexp)) (parse (cadr sexp)))
            (error "Sintaxis de operador unario incorrecta."))]
       [(+ - / *)
        (if (= 3 (length sexp))
            (binopS (proced (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))
            (error "Sintaxis de operador binario incorrecta."))]
       [(zero? num? neg bool?)
        (if (= 2 (length sexp))
            (opBS (proced (car sexp)) (parse (cadr sexp)))
            (error "Sintaxis de operador unario incorrecta."))]
       [(< > <= >= and or)
        (if (= 3 (length sexp))
            (binopBS (proced (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))
            (error "Sintaxis de operador binario incorrecta."))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))