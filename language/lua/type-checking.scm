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

(define (get-types x . y)
  (let lp((n y) (ret (list (lua-typeof x))))
    (cond
     ((null? n) (reverse! ret))
     (else (lp (cdr n) (cons (lua-typeof (car n)) ret))))))

;; define an implemantation related low-level operation with type checking.
;; e.g:
;; (::define (add x y)
;;  ((int int) -> int)
;;  (apply + x y))
(define-syntax ::define
  (syntax-rules (->)
    ((_ (op x y) ((type type* ...) -> func-type) body ...)
     (::define op ((type type* ...) -> func-type) (lambda (x y) body ...)))
    ((_ op ((type type* ...) -> func-type) (lambda (x y) body ...))
     (define (op x . y)
       (let ((ret (match (apply get-types x y)
                    ((type type* ...)
                     body ...)
                    (else (error "Error while args type checking!"
                                 op type type* ... x y)))))
         (if (eq? (lua-typeof ret) 'func-type)
             ret
             (error "Error while ret type checking!"
                    op func-type ret)))))))
