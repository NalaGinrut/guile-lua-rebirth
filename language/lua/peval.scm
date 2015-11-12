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
;; Valuable refs:
;; "Tutorial on Online Partial Evaluation" by William R. Cook. 

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

(define *peval-string-op*
  `((str-concat . ,string-append)
    ;; NOTE: # in Lua can operate either string or table.
    ;;       So it's the duty of type-inference to confirm if
    ;;       it's str-hash or tbl-hash.
    (str-hash . ,string-length)))

(define *lua-primitives*
  (append *peval-arith-op* *peval-string-op*))

(define (get-primitive op)
  (assq-ref *lua-primitives* op))

;; NOTE:
;; We have to call type checking in peval, because some node can't be fixed in
;; compile time. The purpose of partial-evaluator is to evaluate the node in
;; compile time as possible, but leave it alone when encountered the impossible
;; one.
(define* (peval pattern env #:key (cond-true? #f))
  (define (is-variable? x)
    (case (car x)
      ((id sp-id) #t)
      (else #f)))
  (define is-primitive? get-primitive)
  (define (try-to-reduce-primitive op x y)
    ;; NOTE: all the ops should be primitives
    (let ((xx (peval x env))
          (yy (peval y env)))
      (if (and (is-immediate-object? xx) (is-immediate-object? yy))
          ;; NOTE: Won't compile it to tree-il here, should be reduced here.
          (op (<lua-type>-value xx) (<lua-type>-value yy)) 
          ;;`(call (lexical ,op ,op) (const ,(<lua-type>-value xx)) (const ,(<lua-type>-value yy)))
          ;; Can't be reduced, return the similar pattern.
          ;; NOTE: xx and yy may be evaluated, so we should return them rather than the orginal one.
          (list op xx yy))))

  (match pattern
    ;; Constant
    ((? is-immediate-object? obj)
     ;; Immediate object is constant.
     ;; A constant is partially evaluated to itself (just like in the interpreter).
     obj)

    ;; Variable 
    ((? <lua-variable>? v)
     ;; A variable is partially evaluated to the value (constant) according to the variable’s binding
     ;; in the environment (just like in the interpreter), if there is a binding.
     ;; Otherwise, the variable is partially evaluated to itself.
     ;; (The interpreter failed in this case, but partial evaluator would not.)
     (let* ((var (<lua-variable>-name v))
            (val (lua-static-scope-ref var env)))
       (or (lua-static-scope-ref var env) v)))
    
    ;; Primitives
    ;; A primitive operation is applied to the (partially) evaluated arguments (just like in the
    ;; interpreter), if these are all values (constants). Otherwise, a primitive operation form of
    ;; expression is reconstructed from the partially evaluated arguments.
    (((? is-primitive? op) x y)
     (try-to-reduce-primitive (get-primitive op) x y))

    ;; Conditions
    ;; A conditional can be eliminated such that one of the two branches is chosen for recursive (partial)
    ;; evaluation (just like in the interpreter), if the condition is (partially) evaluated to a value (constant).
    ;; Otherwise, both branches are partially evaluated, and the conditional is reconstructed.
    ;; NOTE: Every `block' after 'then is a s-exp starting with 'scope, which alleviates our parsing work.
    ;; NOTE: We didn't generate a recursive s-exp, say, if...elseif... => (if exp ... (if ... )),
    ;;       but a flat one: (if exp then ... eleseif ...). *Maybe* the former is better, but the I'm comfortable
    ;;       while the latter works fine.
    (`(if ,exp then ,block)
     (match (peval exp env)
       ((? is-immediate-object? e)
        ;; Can be reduced
        (if (lua-true? e)
            (peval block env)
            '(marker nil))) ; keep the structure of AST
       (else ; Can't be reduced
        pattern)))
    (('if exp 'then rest ...)
     (match (peval exp env)
       ((? is-immediate-object? e)
        (peval rest env #:cond-true? (lua-true? e)))
       (else pattern)))
    ((block 'elseif exp 'then rest ...)
     (cond
      (cond-true? (peval block env))
      (else 
       (match (peval exp env)
         ((? is-immediate-object? e)
          (peval rest env #:cond-true? (lua-true? e)))
         (else pattern)))))
    (('if exp 'then ('scope block ...) rest  ...)
     (match (peval exp env)
       ((? is-immediate-object? e)
        (if (lua-true? e)
            (peval block (new-scope env))
            (peval rest env)))
       (else pattern)))
  
    ;; A function application is partially evaluated just like in the interpreter—modulo the changed environment
    ;; type. (Alpha renaming should be applied to avoid any name confusion but this is omitted here for brevity.)
    ;; 1. The applied function is looked up and the arguments are evaluated—just like in the interpreter.
    ;; 2. The partially evaluated arguments are partitioned into static and dynamic ones. Static arguments
    ;;    are values (constants); dynamic arguments leverage other expression forms.
    ;; 3. The “identity” (the name) of the specialized function derives from the applied function and the
    ;;    static arguments. Here, we assume that values can be compared for equality. This is essential for
    ;;    remembering (say, memoizing) previous function specializations.
    ;; 4. The body of the specialized function is obtained by partially evaluating the original body in the
    ;;    variable environment of the static variables. The argument list of the specialized function only
    ;;    includes variables for the dynamic positions.
    ;; 5. The specialized function is ultimately applied to the dynamic arguments. The expression for that
    ;;    application serves as the result of partial evaluation.
    (((? <lua-function>? func) x y)
     ;; TODO: Should apply this func as possible.
     (let ((func (get-proper-func
     pattern)

    ;; TODO
    ;; Nothing to be reduced, return the original pattern.
    (else pattern)))
