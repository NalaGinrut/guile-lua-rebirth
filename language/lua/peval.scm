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

;; --------------------------------------------------------------------
;; The partial evaluator
;; NOTE: evaluate the expr as possible, but don't break the AST structure.

(define-module (language lua peval)
  #:use-module (language lua utils)
  #:use-module (language lua types)
  #:use-module ((language lua compile-tree-il)
                #:select (->lambda))
  #:use-module (ice-9 match)
  #:export (peval))

(define *peval-arith-op*
  `((arith-add . ,+)
    (arith-minus . ,-)
    (arith-multi . ,*)
    (arith-div . ,/)
    (arith-expt . ,expt)
    (arith-modulo . ,modulo)))

(define (get-arith-op op)
  (assq-ref *peval-arith-op* op))

(define *peval-string-op*
  `((str-concat . ,string-append)
    ;; NOTE: # in Lua can operate either string or table.
    ;;       So it's the duty of type-inference to confirm if
    ;;       it's str-hash or tbl-hash.
    (str-hash . ,string-length)))

(define (get-string-op op)
  (assq-ref *peval-string-op* op))

;; NOTE:
;; We have to do type checking in peval, because some node can't be fixed in
;; compile time. The purpose of partial-evaluator is to evaluate the node in
;; compile time as possible, but leave it alone when encountered the impossible
;; one.
(define (peval pattern)
  (define (try-to-reduce-op op x y)
    ;; NOTE: all the ops are primitives
    (let ((xx (peval x))
          (yy (peval y)))
      (if (and (is-immediate-object? xx) (is-immediate-object? yy))
          ;; NOTE: Won't compile it to tree-il here, should be reduced here.
          (op (<lua-type>-value xx) (<lua-type>-value yy)) 
          ;;`(call (lexical ,op ,op) (const ,(<lua-type>-value xx)) (const ,(<lua-type>-value yy)))
          ;; Can't be reduced, return the similar pattern.
          ;; NOTE: xx and yy may be evaluated, so we should return them rather than the orginal one.
          (list op xx yy))))
  (match pattern
    ((and (? <lua-type>?) (? is-immediate-object?))
     ;; For immediate type, return it directly.
     pattern)
    (((? get-arith-op op) x y)
     (try-to-reduce-op (get-arith-op op) x y))
    ;; TODO
    ;; Nothing to be reduced, return the original pattern.
    (((? get-string-op op) x y)
     (try-to-reduce-op (get-string-op op) x y))
     ;; TODO
    (else "no")))
