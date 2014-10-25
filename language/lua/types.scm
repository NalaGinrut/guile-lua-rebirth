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

(define-module (language lua types)
  #:use-module (language lua utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:export (<lua-type>?
            <lua-type>-name
            <lua-type>-value

            <lua-variable>?
            <lua-variable>-name
            <lua-variable>-tag

            make-<lua-nil>
            make-<lua-boolean>
            make-<lua-number>
            make-<lua-string>
            make-<lua-table>
            make-<lua-function>
            make-<lua-variable>
            make-<lua-unknown>

            <lua-nil>?
            <lua-boolean>?
            <lua-number>?
            <lua-string>?
            <lua-table>?
            <lua-function>?
            <lua-variable>?
            <lua-unknown>?

            ;; gen types
            gen-nil
            gen-true
            gen-false
            gen-number
            gen-string
            gen-table
            gen-function
            gen-unknown

            lua-typeof
            lua-type-map

            lua-true?
            lua-false?
            
            lua-number?
            lua-string?
            lua-boolean?
            lua-nil?

            is-immediate-object?))

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
  (fields fname arity args return-type))

;; NULL is a special type to indicate `Nothing' or `unspecified'.
;; NOTE: Unknown is not a standard Lua type, it's just used internally by this compiler.
;; NOTE: Don't be confused with Nil.
(define-record-type <lua-unknown> (parent <lua-type>))

;; Lua variable
(define-record-type <lua-variable>
  (fields vname tag))

(define (is-immediate-object? obj)
  (and (<lua-type>? obj)
       (memq (lua-typeof obj) '(number string booleans table nil))))

(define-macro (new-type type)
  #`(lambda args
     (apply #,(symbol-append 'make-<lua- type '>) (quote #,type) args)))

;; NOTE: Nil and Booleans should be unique in the whole environment.
;; NOTE: `const' will generate a const directly, rather than eval it each time.
;;       So it's different from thunk, which will become call-by-name.
(define gen-nil (const ((new-type nil))))
(define gen-true (const ((new-type boolean) 'true)))
(define gen-false (const ((new-type boolean) 'false)))
;; and Unknown
(define gen-unknown (const ((new-type unknown))))

(define gen-number (new-type number))
(define gen-string (new-type string))
(define gen-table (new-type table))
(define gen-function (new-type function))

(define (lua-typeof obj)
  (if (<lua-type>? obj)
      (<lua-type>-name obj)
      (error lua-typeof "Fatal error! Blame compiler writer or modifier!" obj)))

(define (lua-type-map proc x . y)
  (let lp((n y) (ret (list (proc x))))
    (cond
     ((null? n) (reverse! ret))
     (else (lp (cdr n) (cons (proc (car n)) ret))))))

(define-syntax-rule (lua-boolean-check obj val)
  (and (<lua-boolean>? obj) (eq? (<lua-type>-value obj) val)))

(define (lua-true? obj) (lua-boolean-check obj 'true))
(define (lua-false? obj) (lua-boolean-check obj 'false))

(define (lua-number? obj)
  (number? obj))
(define (lua-string? obj)
  (string? obj))
(define (lua-boolean? obj)

;;(define (get-types/vals x . y)
;;  (apply lua-type-map lua-type/value x y))
