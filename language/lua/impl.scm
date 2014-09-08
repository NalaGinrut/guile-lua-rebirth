;;  Copyright (C) 2013,2014
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
  #:use-module (language lua base)
  #:use-module (language lua type-checking)
  #:use-module (ice-9 match)
  #:export (lua-type 
            ;; Arith operation
            lua-add lua-minus lua-multi lua-div lua-mod lua-expt
            ;; Logical operation
            lua-lt lua-eq lua-gt lua-geq lua-leq

            lua-print))

;; TODO: Although it's reasonable to implement new primitives for better type checking,
;;       we have no any chance to do that. Because it's necessary to modify VM to add
;;       new primitives. So we have to convert all the lua-specific-primitives to proper
;;       Guile primitives.

;; TODO: how to print table and function?
(define (lua-print x)
  ;;(display x)(newline)
  `(call (primitive display) ,x))

(define (emit-tree-il-from-function obj)
  (match obj
    (($ <lua-function> ($ <lua-type> _ name value)  
`(call (primitive ,op) (const ,x) (const ,y))))

(define-macro (make-low-level-op op)
  (::define (%lua-num-num-arith x y)
   ((number number) -> (number))
   (try-partial-evaluate op x y)))

(define (lua-arith op x y)
  (match (get-types/vals x y)
    (`((number ,n1) (number ,n1))
     (%lua-num-num-arith op n1 n2))
    ((or '(string number) '(number string))
     (

(define (lua-add x y) (lua-arith '+ x y))
(define (lua-minus x y) (lua-arith '- x y))
(define (lua-multi x y) (lua-arith '* x y))
(define (lua-div x y) (lua-arith '/ x y))
(define (lua-mod x y) (lua-arith 'modulo x y))
(define (lua-expt x y) (lua-arith 'expt x y))

;; return value is guile-boolean 
;; FIXME: add type checking
(define (lua-compare compr x y)
  `(call (primitive ,compr) ,x ,y))

(define (lua-lt x y) (lua-compare '< x y))
(define (lua-eq x y) (lua-compare 'equal? x y))
(define (lua-gt x y) (lua-compare '> x y))
(define (lua-geq x y) (lua-compare '>= x y))
(define (lua-leq x y) (lua-compare '<= x y))
