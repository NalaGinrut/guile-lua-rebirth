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

(define-module (language lua type-checking)
  #:use-module (language lua utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:export (::define
            make-<nil>
            make-<boolean>
            make-<number>
            make-<string>
            make-<table>
            make-<function>))

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

(define-record-type <lua-nil>)
(define-record-type <lua-boolean>)
(define-record-type <lua-number>)
(define-record-type <lua-string> (fields size))
(define-record-type <lua-table> (fields size))
(define-record-type <lua-function> (fields arity args return-type))

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
     (define (op x x* ...)
       (let ((ret (match (get-types x x* ...)
                    ('(type type* ...)
                     body ...)
                    (else (error "Error while args type checking!"
                                 op 'type 'type* ... x x* ...)))))
         (if (eq? (lua-typeof ret) 'func-type)
             ret
             (error "Error while ret type checking!"
                    op 'func-type ret)))))))
