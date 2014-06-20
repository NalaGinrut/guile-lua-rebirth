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
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:export (::define
            lua-<type>-name
            lua-<type>-value

            ))

;; Lua supports duck typing as part of the Metatable weak-typing system.
;; Any reference to a table's member function is checked dynamically at run-time.
;; If an object does not implement the requested function, a run-time error is
;; produced. If a data member is requested but does not exist, a nil value is
;; returned.

;; There're 8 types in Lua:
;; ----------------------------
;; Nil
;; Booleans
;; Numbers
;; Strings
;; Tables
;; Functions
;; Userdata and Threads
;; ----------------------------

(define-record-type <lua-type>
  (fields name value))

(define-record-type <lua-nil> (parent <lua-type>))
(define-record-type <lua-boolean> (parent <lua-type>))
(define-record-type <lua-number> (parent <lua-type>))
(define-record-type <lua-string> (parent <lua-type>)
  (fields size))
(define-record-type <lua-table> (parent <lua-type>)
  (fields size))
(define-record-type <lua-function> (parent <lua-type>)
  (fields arity args return-type))

;; NULL is a special type to indicate `Nothing' or `unspecified'
;; NOTE: Don't confuse with Nil
(define-record-type <lua-null> (parent <lua-type>))

(define-macro (new-type type)
  `(lambda args
     (apply ,(symbol-append 'make-<lua- type '>) (quote #,type) args)))

;; NOTE: Nil and Booleans are optimized to be unique in the whole environment.
(define gen-nil (make-parameter ((new-type 'nil))))
(define gen-true (make-parameter ((new-type 'boolean) 'true)))
(define gen-false (make-parameter ((new-type 'boolean) 'false)))
;; and Null
(define gen-null (make-parameter ((new-type 'null))))

(define gen-number (new-type 'number))
(define gen-string (new-type 'string))
(define gen-table (new-type 'table))
(define gen-function (new-type 'function))

(define (lua-typeof obj)
  (if (<lua-type>? obj)
      (<lua-type>-name obj)
      (error lua-typeof "Fatal error! Blame compiler writer or modifier!" obj)))

(define (lua-type-map proc x . y)
  (let lp((n y) (ret (list (proc x))))
    (cond
     ((null? n) (reverse! ret))
     (else (lp (cdr n) (cons (proc (car n)) ret))))))

(define (get-types/vals x . y)
  (apply lua-type-map lua-type/value x y))

(define (get-types x . y)
  (apply lua-type-map lua-typeof x y))

(define *function-type-table* (make-hash-table))
(define (ftt-add! f t) (hash-set! *function-type-table* f t))
(define (ftt-ref f) (hash-ref *function-type-table* f))

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
;; 2. Because all the ops should be translate to primitives, so we only
;;    consider the primitives type checking.
;; 3. For intermediate functions, we use ::define for helping.

(define (make-type-ops from to ops)
  (list from (map (lambda (op) (list to op)) ops)))

(define arith-ops '(+ - * / ^ %))
(define-syntax-rule (arith-op? op) (memq op arith-ops))

(define string-ops '(.. #))

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
     ;; TODO: add partial evaluator
     (call-arith-op/typed tx ty op x y))
    (else (call-arith-op op x y))))

(define-macro (expect type x)
  #`(let ((t (lua-typeof x)))
      (eq? (#,(symbol-append '<lua- t '>?) x) type)))

;; NOTE: Type inference pass should never break the AST. It should keep the
;;       structure of AST as possible. The work of this function is to
;;       mark & infer all the types, include expr/literal/id. 
(define (type-inference node)
  (define-syntax-rule (expect who mate)
    ;; TODO
    )
  (define (arith-op-ti op x y)
    (let ((tx (type-inference x))
          (ty (type-inference y)))
      (and (<lua-number>? tx)
           (expect <lua-number>? ty))
      ;; TODO
      ))
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
     (check/arith-op op (type-inference x) (type-inference y)))
  ))
