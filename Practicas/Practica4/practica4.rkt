#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

;;Desugar
(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [withS (bindings body) (app (fun (map (lambda (x) (bind-name x)) bindings)
                                     (desugar body))
                                (map (lambda (x)(desugar (bind-val x))) bindings))]
    [with*S (bindings body) (matryoshka bindings body)]
    [idS (name) (id name)]
    [funS (params body) (fun params (desugar body))]
    [appS (f l) (app (desugar f) (desugar l))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]))

;;Funcion Matryoshka
(define (matryoshka bindings body)
  (cond
    [(empty? bindings) (desugar body)]
    [else (app (fun(list (bind-name(car bindings))
                         (matryoshka (cdr bindings) body))
                   (list (desugar (bind-val (car bindings))))))]))

;;Funcion interp: FAE -> FAE-Value
(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [binop (f x y) (numV (f (numV-n (interp x env)) (numV-n (interp y env))))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr args-expr)
         (local([define funV (interp fun-expr env)])
           (if (closureV? funV)
               (let* ([funV-param (closureV-param funV)]
                      [num-params (length funV-param)]
                      [num-args (length args-expr)])
                 (interp (closureV-body funV)
                         (if (= num-params num-args)
                             (multi-param funV-param args-expr env (closureV-env funV))
                             (error 'interp (~a "Numero de parametros incorrecto: ")))))
               (error 'interp (string-append (~a funV) "No FAE"))))]))


;;Multi-param
(define (multi-param params args args-env a-env)
  (if (empty? params) a-env
      (multi-param (cdr params) (cdr args) args-env
                   (aSub (car params)
                         (interp (car args) args-env) a-env))))

;;Lookup
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup (~a "no hay identificador ligado: " name))]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name) bound-value (lookup name rest-env))]))


(define (cparse sexp)
  (desugar (parse sexp)))

(define (rinterp expr)
  (interp expr (mtSub)))

#|====TESTS====|#
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))