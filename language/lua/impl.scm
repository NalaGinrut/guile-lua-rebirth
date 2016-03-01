;;  Copyright (C) 2013,2014,2016
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

(define-module (language lua impl)
  #:use-module (language lua utils)
  ;;#:use-module (language lua base)
  #:use-module (language lua type-inference)
  #:use-module (language lua optimize)
  #:use-module (language lua types)
  #:use-module (language lua irregex)
  #:use-module (ice-9 match)
  #:export (;; Arith operation
            lua-add lua-minus lua-multi lua-div lua-mod lua-expt
            ;; Logical operation
            lua-lt lua-eq lua-gt lua-geq lua-leq

            lua-print
            
            is-lua-builtin-func?))

;; TODO: Although it's reasonable to implement new primitives for better type checking,
;;       we have no any chance to do that. Because it's necessary to modify VM to add
;;       new primitives. So we have to convert all the lua-specific-primitives to proper
;;       Guile primitives.

;; TODO: how to print table and function?
(define (lua-print x)
  `(call (primitive display) ,x))

(define (emit-tree-il-from-primitive op x y)
  `(call (primitive ,op) ,x ,y))

(define-macro (define-primitive-emitter op)
  `(define (,(symbol-append 'lua-primiitve- op) x y)
     (emit-tree-il-from-primitive ',op x y)))

(define-primitive-emitter +)
(define-primitive-emitter -)
(define-primitive-emitter *)
(define-primitive-emitter /)
(define-primitive-emitter modulo)
(define-primitive-emitter expt)

(define *strnum-re* (string->sre "0x([a-zA-Z0-9]+)"))
(define (str->num x)
  ;; NOTE: According to Lua actual activity, if a string can't be converted to a number,
  ;;       say, "0xaz", it will produce a string "0xaz".
  ;; NOTE: Guile will check the type again in the low-level, so we will not check it here.;
  (define (%string->number s)
    (cond
     ((number? s) s)
     (else
      (let ((sn (irregex-replace *strnum-re* s "#x" 1)))
        (cond
         ((string->number sn) => identity)
         (else s))))))
  (match x
    (('const v) `(const ,(%string->number v)))
    (else (error str->num "Invalid AST node!" x))))

(define (lua-arith emitter x y)
  (match (get-ast-types x y)
    ('(number number)
     (emitter x y))
    ('(string number)
     (emitter (str->num x) y))
    ('(number string)
     (emitter x (str->num y)))
    ('(string string)
     (emitter (str->num x) (str->num y)))
    ((('lexical-ref name rename) 'number)
     (emitter `(call (@@ (language lua impl) str->num)
                     (lexical-ref ,name ,rename))
              y))
    (('number ('lexical-ref name rename))
     (emitter x `(call (@@ (language lua impl) str->num)
                       (lexical-ref ,name ,rename))))
    (('string ('lexical-ref name rename))
     (emitter (str->num x)
              `(call (@@ (language lua impl) str->num)
                     (lexical-ref ,name ,rename))))
    ((('lexical-ref name rename) 'string)
     (emitter `(call (@@ (language lua impl) str->num)
                     (lexical-ref ,name ,rename))
              (str->num y)))
    (else (error lua-arith "Fatal: invalid pattern!" (get-ast-types x y)))))

(define (lua-add x y) (lua-arith lua-primiitve-+ x y))
(define (lua-minus x y) (lua-arith lua-primiitve-- x y))
(define (lua-multi x y) (lua-arith lua-primiitve-* x y))
(define (lua-div x y) (lua-arith lua-primiitve-/ x y))
(define (lua-mod x y) (lua-arith lua-primiitve-modulo x y))
(define (lua-expt x y) (lua-arith lua-primiitve-expt x y))

;; return value is guile-boolean 
;; FIXME: add type checking
(define (lua-compare compr x y)
  `(call (primitive ,compr) ,x ,y))

(define (lua-lt x y) (lua-compare '< x y))
(define (lua-eq x y) (lua-compare 'equal? x y))
(define (lua-gt x y) (lua-compare '> x y))
(define (lua-geq x y) (lua-compare '>= x y))
(define (lua-leq x y) (lua-compare '<= x y))

;; NOTE: built-in functions are unnecessariely
(define *built-in-functions*
  `(("print" . ,lua-print)))

(define (is-lua-builtin-func? x)
  (assoc-ref *built-in-functions* x))
