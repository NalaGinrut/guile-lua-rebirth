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
  #:export (lua-env
            lua-env?
            lua-env-higher-level-env
            lua-env-higher-level-env-set!
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
   (mutable higher-level-env)
   symbal-table))

(define symbol-table-set! hash-set!)
(define symbol-table-ref hash-ref)
;; TODO: maybe implement a better tree?
(define new-symbol-table make-hash-table)

(define *top-level-environment*
  (make-lua-env #f ; top-level has no higher level
                (new-symbol-table)))

;; FIXME: do we really need this??
(define current-top-level-environment
  (make-parameter *top-level-environment*))

(define (lua-local-set! e k v)
  (symbol-table-set! (lua-env-symbol-table e) k v))

(define (lua-local-ref e k)
  (symbol-table-ref (lua-env-symbol-table e) k))

(define (is-top-level? e)
  (not (lua-env-higher-level-env e)))

(define (lua-static-scope-set! e k v)
  (lua-local-set! e k v))

(define (lua-static-scope-ref e k)
  (or (and (lua-local-ref e k) identity)
      (and (not (is-top-level? e))
           (lua-static-scope-ref (lua-env-higher-level-env e) k))))

(define (lua-global-set! k v)
  (symbol-table-set! *top-level-environment* k v))

(define (lua-global-ref k)
  (symbol-table-ref *top-level-environment* k))

(define (new-scope e)
  (make-lua-env e (new-symbol-table)))

(define (get-proper-func fname env)
  (match fname
    (`(id ,fn)
     (or (lua-static-scope-ref env fn)
         (throw 'lua-error "No such identifier in the scope" fn)))
    (else (error get-proper-func "Invalid func name pattern!" fname))))
