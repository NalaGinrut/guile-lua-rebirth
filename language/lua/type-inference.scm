;;  Copyright (C) 2014
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (language lua type-inference)
  #:use-module (language lua utils)
  #:use-module (language lua peval)
  #:use-module (language lua types)
  #:use-module (ice-9 match)
  #:export (::define
            type-inference))

(define *function-type-table* (make-hash-table))
(define (ftt-add! f t) (hash-set! *function-type-table* f t))
(define (ftt-ref f) (hash-ref *function-type-table* f))

(define (get-types x . y)
  (apply lua-type-map lua-typeof x y))

;; define an implemantation related low-level operation with type checking.
;; e.g:
;; (::define (add x y)
;;  ((int int) -> int)
;;  (+ x y))
(define-syntax ::define
  (syntax-rules (->)
    ((_ (op x x* ...) ((type type* ...) -> func-type) body ...)
     (::define op ((type type* ...) -> func-type) (lambda (x x* ...) body ...)))
    ((_ op ((type type* ...) -> func-type) (lambda (x x* ...) body ...))
     (begin
       (ftt-add! 'op `((type type* ...) -> func-type))
       (define (op x x* ...)
         (let ((ret (match (get-types x x* ...)
                      ('(type type* ...)
                       body ...)
                    (else (error "Error while args type checking!"
                                 op 'type 'type* ... x x* ...)))))
           (if (eq? (lua-typeof ret) 'func-type)
               ret
               (error "Error while ret type checking!"
                      op 'func-type ret (lua-typeof ret)))))))))

;; --- Runtime Type Checking ---
;; 1. We use DFA for the type checking.
;; 2. Because all the ops should be translate to primitives, so we
;;    consider the type checking for primitives only.
;; 3. For intermediate functions, we use ::define for helping.

(define (make-type-ops from to ops)
  (list from (map (lambda (op) (list to op)) ops)))

(define arith-ops '(+ - * / ^ %))
(define-syntax-rule (arith-op? op) (memq op arith-ops))

(define string-ops '(concat hash))

(define number->number-ops
  (make-type-ops 'number 'number arith-ops))

(define string->number-ops
  (make-type-ops 'string 'number arith-ops))

(define string->string-ops
  (make-type-ops 'string 'string string-ops))

(define *type/inference-table*
  `((number ,number->number-ops ,string->number-ops)
    (string ,number->number-ops ,string->string-ops)
    ))

;; (number (number (+ number) (- number) (* number) (/ number) ( (string number))
;; (string (string string) (number number))

;; -- Compile time type checking --

(define (err o)
  (error "Type-checking error: attempt to perform arithmetic on a ~a value" (lua-typeof o)))

(define (lua-arith/number op x y)
  `(number ,(peval (list (symbol-append 'arith- op) x y))))

(define (lua-arith/string op x y)
  (let ((xx (string->number x))
        (yy (string->number y)))
    (and (or xx (err xx)) (or yy (err yy)))
    (lua-arith/number op xx yy)))

;; NOTE: x is always string
(define (lua-arith/numstr op x y)
  (let ((xx (string->number x)))
    (or xx (err xx))
    (lua-arith/number op xx y)))

(define-syntax-rule (->lua-op category type)
  (primitive-eval (symbol-append 'lua- category '/ type)))

(define (call-arith-op/typed tx ty op x y)
  (match (list tx ty)
    ((number number)
     ((->lua-op 'arith 'number) op x y))
     ((string string)
      ((->lua-op 'arith 'string) op x y))
     ((string number)
      ((->lua-op 'arith 'numstr) op x y))
     ((number string)
      ((->lua-op 'arith 'numstr) op y x))
     (else
      ;; Shouldn't be here! Because all the arith-op should be registered.
      (error "Type-checking: invalid type for arithmetic" (list tx ty) op))))

(define (check/arith-op op x y)
  (match (ftt-ref op)
    ((tx ty '-> tf)
     (and (or (eq? tx (lua-typeof x)) (err x))
          (or (eq? ty (lua-typeof y)) (err y)))
     `(,tf ,(call-arith-op/typed tx ty op (peval x) (peval y))))
    (else (error "Type-checking: No type inference rule was registered!" op)))) 

(define-syntax-rule (->lua-type? t)
  (primitive-eval (symbol-append '<lua- t '>?)))

(define-syntax-rule (expect type x)
  ((->lua-type? type) x))

;; NOTE: Type inference pass should never break the AST. It should keep the
;;       structure of AST as possible. The work of this function is to
;;       mark & infer all the types, include expr/literal/id. 
(define (type-inference node)
  ;;(display src)(newline)
  (match node
    ;; Literals
    ('(marker nil) (gen-nil))
    ('(boolean true) (gen-true))
    ('(boolean false) (gen-false))
    (`(number ,x) (gen-number x))
    (`(string ,x) (gen-string x))
    (`(scope ,n) `(scope ,(type-inference n)))
    
    ;; Primitives
    (((? arith-op? op) x y)
     (type-inference (check/arith-op op (type-inference x) (type-inference y))))

  ))
