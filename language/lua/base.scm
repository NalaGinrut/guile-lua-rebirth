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

(define-module (language lua base)
  #:use-module (language lua utils)
  ;;#:use-module (language lua runtime)
  #:export (->number
            get-compr-of-type
            ->type
            ->lua-boolean
            ->guile-boolean
            ->object))

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
    (else (error get-table-compr "attempt to compare table with table"))))

(define (get-compr-of-type t kind)
  (cond
   ((string=? t "number") (assoc-ref *number-compr* kind))
   ((string=? t "string") (assoc-ref *string-compr* kind))
   ((string=? t "table") (get-table-compr kind))
   (else (error get-compr-of-type "Invalid type for comparing" t))))

(define (->type x) (value-type->string x))

;; Lua support "1"+1 or "1"+"1"
(define (->number x)
  (cond
   ((number? x) x)
   ((string? x) (string->number x))
   (else 
    (error ->number "attempt to perform arithmetic on a ~a value: ~a"
           (->type x) x))))

;; only nil and false are FALSE
(define (->guile-boolean x)
  (not (or (eqv? (cadr x) 'false) (eqv? (cadr x) 'nil))))

(define (->lua-boolean x)
  (if x 'true 'false))

(define (->object x)
  (match (lua-type x)
    ((or "number" "string") x)
    ((or  "boolean" "nil") (->boolean x))
    ((or "function") (->function x))
    ((or "corotine" "thread") (error "Doesn't support yet" x))
    (else (error ->object "Unknown object type" x))))
