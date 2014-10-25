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

(define-module (language lua scope)
  #:use-module (language lua utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:use-module (nashkel rbtree)
  #:export (lua-env
            lua-env?
            lua-env-upper-frame
            lua-env-upper-frame-set!
            lua-env-symbol-table

            current-top-level-environment

            lua-local-set!
            lua-local-ref

            lua-static-scope-set!
            lua-static-scope-ref

            lua-global-set!
            lua-global-ref
            
            new-scope

            get-proper-func))

(define-record-type lua-env
  (fields 
   (mutable upper-frame)
   symbal-table))

(define (symbol-table-pred t sym)
  (rbt-make-PRED t = > < (string-hash-ci sym)))

(define (symbol-table-set! t sym val)
  (rb-tree-add! t sym val #:PRED symbol-table-pred))

(define (symbol-table-ref t sym)
  (rb-tree-search t sym #:PRED symbol-table-pred))

;; symbol table is Red Black Tree
(define new-symbol-table new-rb-tree)

(define *top-level-environment*
  (make-lua-env #f ; top-level has no upper frame
                (new-symbol-table)))

;; NOTE: fetch top-level directly
(define current-top-level-environment
  (make-parameter *top-level-environment*))

(define (lua-local-set! e sym val)
  (symbol-table-set! (lua-env-symbol-table e) sym val))

(define (lua-local-ref e sym)
  (symbol-table-ref (lua-env-symbol-table e) sym))

(define (is-top-level? e)
  (not (lua-env-upper-frame e)))

(define (lua-static-scope-set! e sym val)
  (lua-local-set! e sym val))

(define (lua-static-scope-ref e sym)
  (or (lua-local-ref e sym)
      (and (not (is-top-level? e))
           (lua-static-scope-ref (lua-env-upper-frame e) sym))))

(define (lua-global-set! sym val)
  (symbol-table-set! *top-level-environment* sym val))

(define (lua-global-ref sym)
  (symbol-table-ref *top-level-environment* sym))

(define (new-scope e)
  (make-lua-env e (new-symbol-table)))

(define (get-proper-func fname env)
  (get-val-from-scope fname env))

(define (get-val-from-scope idname env)
  (match idname
    (`(id ,name)
     (or (lua-static-scope-ref env name)
         (throw 'lua-error "No such identifier in the scope" name)))
    (else (error get-val-from-scope "Invalid func name pattern!" idname))))
