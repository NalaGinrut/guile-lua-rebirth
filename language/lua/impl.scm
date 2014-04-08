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
  #:export (lua-type 
            ;; Arith operation
            lua-add lua-minus lua-multi lua-div lua-mod lua-expt
            ;; Logical operation
            lua-lt lua-eq lua-gt lua-geq lua-leq))

(define (lua-type x) (->type x))

(define (lua-arith op x y)
  (op (->number x) (->number y)))

(define (lua-add x y) (lua-arith + x y))
(define (lua-minus x y) (lua-arith - x y))
(define (lua-multi x y) (lua-arith * x y))
(define (lua-div x y) (lua-arith / x y))
(define (lua-mod x y) (lua-arith modulo x y))
(define (lua-expt x y) (lua-arith expt x y))

;; return value is guile-boolean 
(define (lua-compare compr x y) 
  (let ((xt (lua-type x))
        (yt (lua-type y)))
    (cond
     ((equal? xt yt) ; x and y has same type
      ((get-compr-of-type xt compr) (->object x) (->object y)))
     (else
      (error "attempt to compare ~a with ~a" xt yt)))))

(define (lua-lt x y) (lua-compare 'lt x y))
(define (lua-eq x y) (lua-compare 'eq x y))
(define (lua-gt x y) (lua-compare 'gt x y))
(define (lua-geq x y) (lua-compare 'geq x y))
(define (lua-leq x y) (lua-compare 'leq x y))
