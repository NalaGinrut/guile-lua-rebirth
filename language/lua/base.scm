;;  Copyright (C) 2013
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
  #:use-module (language lua runtime)
  #:export (->number get-compr-of-type ->type ->lua-boolean
            ->guile-boolean ->object))

(define *number-compr*
  `((eq  . ,=)
    (lt  . ,<)
    (gt  . ,>)
    (leq . ,<=)
    (geq . ,>=)
    (neq . ,(lambda (x y) (not (= x y))))))

(define *string-compr*
  `((eq  . ,string=?)
    (lt  . ,string<?)
    (gt  . ,string>?)
    (leq . ,string<=?)
    (geq . ,string>=?)
    (neq . ,(lambda (x y) (not (string=? x y))))))  

;; NOTE: since I use hash-table and recor-type to implement Lua table,
;;       so I think eq? is suitable for such comparing. Correct me if
;;       I'm wrong.
(define (get-table-compr kind)
  (case kind 
    ((eq) eq?) 
    ((neq) (lambda (x y) (not (eq? x y))))
    (else (error "attempt to compare table with table"))))
 
(define (get-compr-of-type t kind)
  (case t
    (("number") (assoc-ref *number-compr* kind))
    (("string") (assoc-ref *string-compr* kind))
    (("table") (get-table-compr kind)
    (else (error get-compr-of-type "Invalid type for comparing" t))))

(define (->type x) (value-type->string x))

;; Lua support "1"+1 or "1"+"1"
(define (->number x)
  (cond
   ((number? x) x)
   ((string? x) (string->number x))
   (else 
    (error "attempt to perform arithmetic on a ~a value: ~a"
           (->type x) x))))

;; only nil and false are FALSE
(define (->guile-boolean x)
  (not (or (eqv? (cadr x) 'false) (eqv? (cadr x) 'nil))))

(define (->lua-boolean x)
  (if x 'true 'false))

(define (->object x)
  (case (lua-type x)
    (("number" "string") x)
    (("boolean" "nil") (->boolean x))
    (("function") (->function x))
    (("corotine" "thread") (error "Doesn't support yet" x))
    (else (error "Unknown object type" x))))
