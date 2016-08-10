;;  Copyright (C) 2014,2015,2016
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

(define-module (language lua scope)
  #:use-module (language lua utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (lua-env
            lua-env?
            lua-env-upper-frame
            lua-env-upper-frame-set!
            lua-env-symbol-table

            current-top-level-environment
            reinit-lua-toplevel

            lua-local-set!
            lua-local-ref

            lua-static-scope-set!
            lua-static-scope-ref
            is-lexical-bind?
            
            lua-global-set!
            lua-global-ref
            
            new-scope

            get-proper-func
            get-val-from-scope
            print-lua-env

            get-rename
            %lexical-var-set!))

;; NOTE: Since Lua is not FP, we're not going to implement it as the functional
;;       static scope which means the upper level scope will be immutable. That
;;       would be little complicated to be implemented efficiently.
;;       Now it'll be an imperitive static scope implementation. That's easier.

(define-record-type lua-env
  (fields 
   (mutable upper-frame)
   symbol-table))

(define (symbol-table-set! t sym val)
  (hash-set! t sym val))

(define (symbol-table-ref t sym)
  (hash-ref t sym))

(define new-symbol-table make-hash-table)

(define (new-lua-toplevel)
  (make-lua-env #f ; top-level has no upper frame
                (new-symbol-table)))

;; NOTE: fetch top-level directly
(define current-top-level-environment
  (make-parameter (new-lua-toplevel)))

(define (lua-local-set! e sym val)
  (symbol-table-set! (lua-env-symbol-table e) sym val))

(define (lua-local-ref e sym)
  (symbol-table-ref (lua-env-symbol-table e) sym))

(define (is-top-level? e)
  (not (lua-env-upper-frame e)))

(define (lua-static-scope-set! e sym val)
  (lua-local-set! e sym val))

;; test if it's lexical bind, include toplevel
(define (lua-static-scope-ref e sym)
  (or (lua-local-ref e sym)
      (and (not (is-top-level? e))
           (lua-static-scope-ref (lua-env-upper-frame e) sym))))

;; test if it's lexical bind, but not in toplevel
(define (is-lexical-bind? e sym)
  (and (not (is-top-level? e))
       (or (lua-local-ref e sym)
           (is-lexical-bind? (lua-env-upper-frame e) sym))))

(define (lua-global-set! sym val)
  (symbol-table-set! (lua-env-symbol-table (current-top-level-environment)) sym val))

(define (lua-global-ref sym)
  (symbol-table-ref (lua-env-symbol-table (current-top-level-environment)) sym))

(define (new-scope e)
  (make-lua-env e (new-symbol-table)))

(define (get-proper-func fname env)
  (get-val-from-scope fname env))

;; Should be used in %rename only.
(define (get-val-from-scope name env)
  (or (lua-static-scope-ref env name)
      (error 'get-val-from-scope "BUG: No such identifier in the scope" name)))

(define (print-lua-env e)
  (define (%print-lua-env ee)
    (hash-for-each (lambda (k v) (format #t "~a => ~a~%" k v)) (lua-env-symbol-table ee))
    (if (not (is-top-level? ee))
        (%print-lua-env (lua-env-upper-frame ee))
        (display "=========end=========\n\n")))
  (display "\n\n=========ENV=========\n")
  (%print-lua-env e))

(define-syntax-rule (get-rename e k)
  (let ((ll (assoc-ref (lua-static-scope-ref e k) 'rename)))
    (if ll
        (car ll)
        (error 'get-rename "No rename!" k))))

;; Only for special var in guile-lua compiler, don't use it if you're not sure.
(define-syntax-rule (%lexical-var-set! e k v)
  (lua-static-scope-set! e k `((rename ,(newsym k)) (value ,v))))
