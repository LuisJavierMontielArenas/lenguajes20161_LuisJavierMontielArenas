#lang plai

(require "practica5-base.rkt")
(print-only-errors true)

(define (desugar expr)
  (type-case RCFAELS expr
    [idS (s) (id s)]
    [numS (n) (num n)]
    [boolS (n) (bool n)]
    [withS (bindings body) (app (fun (map bindS-name bindings)
                                     (desugar body))
                                (map desugar
                                     (map bindS-val bindings)))]
    [recS (bindings body) (rec (map rebind bindings) (desugar body))]
    [funS (p b) (fun p (desugar b))]
    [if0S (test truth falsity) (if0 (desugar test) (desugar truth) (desugar falsity))]
    [isequal?S (a b) (isequal? (desugar a) (desugar b))]
    [opS (f l) (op f (desugar l))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [opBS (f l) (opB f (desugar l))]
    [binopBS (f l r) (binopB f (desugar l) (desugar r))]
    [appS (f e) (app (desugar f)
                     (map desugar e))]))


(define (rebind b)
  (bind (bindS-name b) (desugar (bindS-val b))))

(define (interp expr env)
  (type-case RCFAEL expr
    [id (v) (lookup v env)]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [op (f l) (wrapOp f (interp l env))]
    [binop (f l r) (wrapBin f (interp l env) (interp r env))]
    [opB (f l) (wrapOpB f (interp l env))]
    [binopB (f l r) (wrapBinB f (interp l env) (interp r env))]
    [if0 (test truth falsity)
         (if (boolV-b (interp test env))
             (interp truth env)
             (interp falsity env))]
    [isequal? (a b) (equalV (interp a env) (interp b env))]
    [rec (bindings bound-body)
      (interp bound-body
              (rec-multi-param bindings env))]
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
                             (error 'interp (string-append "Error de aridad: Se esperaban "
                                                           (~a num-params)
                                                           " argumentos y se recibieron "
                                                           (~a num-args))))))
               (error 'interp (string-append (~a funV) " No es una función."))))]))

; lookup
; Busca el valor de un identificador en un ambiente dado.
(define(lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup (string-append "El identificador (" (~a name)
                                            ") es libre y no tiene un valor"
                                            " definido para ser evaluado"))]
    [aSub (bound-name bound-value rest-env)
          (if(symbol=? bound-name name) bound-value
             (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name) (unbox boxed-bound-value)
                 (lookup name rest-env))]))

; wrapB
; Devuelve el FAE-Value del valor de la operacion aritmética dada entre dos
; números numV dados.
(define (wrapBin f l r)
  (if (and (numV? l) (numV? r)) (numV (f (numV-n l) (numV-n r)))
      (errorOpBin f l r)))

; wrap
; Devuelve el FAE-Value del valor de la operacion aritmética dada entre dos
; números numV dados.
(define (wrapOp f o)
  (if (numV? o) (numV (f (numV-n o)))
      (errorOp f o)))

; wrapB
; Devuelve el FAE-Value del valor de la operacion aritmética dada entre dos
; números numV dados.
(define (wrapBinB f l r)
  (cond
    [(and (numV? l) (numV? r))
     (case (~a f)
       [("#<procedure:And>" "#<procedure:Or>") (errorOpBin f l r)]
       [else (boolV (f (numV-n l) (numV-n r)))])]
    [(and (boolV? l) (boolV? r))
     (case (~a f)
       [("#<procedure:And>" "#<procedure:Or>") (boolV (f (boolV-b l) (boolV-b r)))]
       [else (errorOpBin f l r)])]
    [else (errorOpBin f l r)]))

; wrap
; Devuelve el FAE-Value del valor de la operacion aritmética dada entre dos
; números numV dados.
(define (wrapOpB f o)
  (cond
    [(numV? o)
     (case (~a f)
       [("#<procedure:not>") (errorOp f o)]
       [else (boolV (f (numV-n o)))])]
    [(boolV? o)
     (case (~a f)
       [("#<procedure:zero?>") (errorOp f o)]
       [else (boolV (f (boolV-b o)))])]
    [else (errorOp f o)]))

(define (equalV a b)
  (cond
    [(and (numV? a) (numV? b))
     (boolV (eq? (numV-n a) (numV-n b)))]
    [(and (boolV? a) (boolV? b))
     (boolV (eq? (boolV-b a) (boolV-b b)))]
    [else (error "Uso inadecuado de equal.")]))

(define (rec-multi-param bindings env)
  (if (empty? bindings) env
      (let ([bound-id (bind-name (car bindings))]
            [named-expr (bind-val (car bindings))])
        (rec-multi-param (cdr bindings)
                         (cyclically-bind-and-interp bound-id named-expr env)))))


;; multi-param
;; Agrega una lista de parámetros con sus valores al ambiente dado4.
(define (multi-param params args args-env ac-env)
  (if (empty? params) ac-env
      (let ([arg-val (interp (car args) args-env)])
        (multi-param (cdr params) (cdr args) (aSub (car params) arg-val args-env)
                     (aSub (car params) arg-val ac-env)))))

(define (errorOpBin f l r)
  (error (string-append "Los operandos " (~a l) " y " (~a r)
                        " no pueden ser operados por " (~a f))))

(define (errorOp f o)
  (error (string-append "Él operando " (~a o)
                        " no puede ser operado por " (~a f))))

(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))
