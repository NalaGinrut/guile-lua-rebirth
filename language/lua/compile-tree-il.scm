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

(define-module (language lua compile-tree-il)
  #:use-module (language lua utils)
  #:use-module (language lua parser)
  #:use-module (language lua scope)
  ;;#:use-module (language lua impl)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:export (compile-tree-il
            ->lambda
            ->gensym
            tree-il-define))

(define (lua-init)
  #t) ;; nothing to do yet.

(define (compile-tree-il exp env opts)
  (values
   (parse-tree-il
    (begin (lua-init)
           (comp exp (current-top-level-environment))))
   env
   env))

(define (lookup name env)
  (hash-ref env name))

(define (store name value env)
  (hash-set! env name value))

(define (let1 what proc)
  (let ((sym (gensym))) 
    (-> (let (list sym) (list sym) (list what)
             (proc sym)))))

(define (begin1 what proc)
  (let1 what (lambda (v)
               (-> (begin (proc v)
                          (-> (lexical v v)))))))
;; for emacs:
;; (put 'match 'scheme-indent-function 1)

(define (comp src e)
  ;;(display src)(newline)
  (match src
    ;; Literals
    ('(marker nil)
     '(const nil))
    ('(boolean true)
     '(const true))
    ('(boolean false)
     '(const false))
    (`(number ,x)
     (-> (const x)))
    (`(string ,x)
     (-> (const x)))

    ;; variables
    (`(variable ,x)
     (%> lookup x e))
    (`(store ,x ,v)
     (%> store x (comp v e) e))

    ;; scope and statment
    ;; FIXME: we need lexical scope
    (('scope rest)
     ;;(display rest)(newline)
     (comp rest (new-scope e)))
    (('begin form ...)
     (comp form e))
    (('begin forms ...)
     `(begin ,@(map (lambda (x) (comp x e)) forms)))

    ;; arithmatic op
    (`(add ,x ,y)
     (lua-add (comp x e) (comp y e)))
    (`(minus ,x ,y)
     (lua-minus (comp x e) (comp y e)))
    (`(multi ,x ,y)
     (lua-multi (comp x e) (comp y e)))
    (`(div ,x ,y)
     (lua-div (comp x e) (comp y e)))
    (`(mod ,x ,y)
     (lua-mod (comp x e) (comp y e)))
    (`(expt ,x ,y)
     (@impl 'lua-expt (comp x e) (comp y e)))

    ;; logical op
    (`(lt ,x ,y)
     (lua-lt (comp x e) (comp y e)))
    (`(gt ,x ,y)
     (lua-gt (comp x e) (comp y e)))
    (`(eq ,x ,y)
     (lua-eq (comp x e) (comp y e)))
    (`(geq ,x ,y)
     (lua-geq (comp x e) (comp y e)))
    (`(leq ,x ,y)
     (lua-lt (comp x e) (comp y e)))

    ;; functions
    (`(func-call (id ,func) (args ,args))
     ;; FIXME: Should detect if it's lib function
     ;;(display func)(newline)
     (@impl (->lua func) (comp args e)))
    ;; TODO: finish the rest
    (else (error comp "invalid src" src))))

(define (->gensym x)
  `(const ,(newsym x)))

;; NOTE: But Lua is imperitive language, so I'm afraid the binding (with let)
;;       is useless for it. Since imperitive languages uses assignment instead.
(define (->body body name-list rename-list)
  ;; TODO: find out all the lexical bindings to be replaced with lexical syntax of tree-il.
  ;;       This may need recursive and calling `comp' function.
  #t)

;; <let>
;; GRAMMAR:
;; (let (name-list) (rename-list) (values-to-bind) body-in-tree-il)
;; e.g:
;; (let (x y) (x321 y123) ((const 1) (const 2)) (call (toplevel +) (lexical x x321) (lexical y y123))) ==> 3
;; NOTE: only x321 and y123, say, rename-list matters, so keep them identical in the body,
;;       name-list seems useless here, dunno...
(define (->let name-list val-list body env)
  (let ((rename-list (map newsym name-list)))
    `(let ,name-list ,rename-list ,val-list ,(->body body name-list rename-list env))))

;; There's no let* in tree-il, so we have to handle it.
(define (->let* name-list val-list body env)
  (if (null? name-list)
      `(let () () () ,body)
      `(let ,(list (car name-list)) ,(map newsym name-list) ,(list (car val-list))
            (->let* (cdr name-list) (cdr var-list) body))))

(define *toplevel-ops* '())

(define (is-toplevel-op? o)
  ;; NOTE: the-root-module only contains primitives, which is proper for this function.
  (or (defined? o the-root-module)
      (memq o *toplevel-ops*)))

(define *default-toplevel-module* (resolve-module '(guile-user)))

;; FIXME: It's useless to check toplevel/primitives.
;;        First, Guile does this for you. Second, the info is nothing at the end.
;; 1. maintain lexical envs.
;; 2. pass it in to help to check lexical symbols.
;; 3. We DON'T need to check toplevel but lexical to confirm what instruction to generate.

(define (is-toplevel-var? v)
  ;; NOTE: If you define a var in toplevel, you can't find it in the-root-module,
  ;;       which is for primitives.
  (defined? v *default-toplevel-module*))

(define (->scope id pred)
  (cond
   ((pred id) `(toplevel ,id))
   (else `(lexical ,id ,(newsym id)))))

(define (->op op) (->scope op is-toplevel-op?))
(define (->var var) (->scope var is-toplevel-var?))
(define (->val v)
  (match v
    ((? self-evaluating? val) `(const ,val))
    (else (->var v))))

(define (body-expander . body)
  (match body
    (() '())
    (((op args ...) rest ...) `((call ,(->op op) ,@(map ->val args) ...) ,@(apply body-expander rest)))
    (else "no")))

;; <lambda>
;; GRAMMAR:
;; (lambda () (lambda-case ((req opt rest kw inits gensyms) body) [alternate]))
;; NOTE: you have to use (lambda () ...) to create a closure first, than define the lambda-case in it.
;; e.g:
;; (define func (lambda () (lambda-case (((o) #f #f #f () (o123)) (call (toplevel +) (const 1) (lexical o o123)))))
;; o123 matters, so they have to be the same.
;; (func 2) ==> 3
;; (define func (lambda () (lambda-case (((x y) #f #f #f () (x123 y321)) (call (toplevel +) (lexical x x123) (lexical y y321))))))
;; (func 2 3) ==> 5
;; e.g, a thunk:
;; (lambda () (lambda-case ((() #f #f #f () ()) (const 123))))
;;
;; 1. (lambda () 1)
;; e.g: (lambda () (lambda-case ((() #f #f #f () ()) (const 1))))
;;
;; 2. (lambda (x) 3) 
;; e.g: (lambda () (lambda-case (((x) #f #f #f () ((call (primitive gensym) (const "x ")))) (const 3))))
;;
;; 3. (lambda (. y) 3)
;; e.g: (lambda () (lambda-case ((() #f y #f () ((call (primitive gensym) (const "x ")))) (const 3))))
;;
;; 4. (lambda (x y . z) 3)
;; e.g: (lambda () (lambda-case (((x y) #f z #f () ((const x1) (const y1) (const z2))) (const 3))))
;;
;; *Maybe* we don't need to support kargs (in Lua), hmm...dunno
(define-syntax ->lambda
  (syntax-rules ()
    ((_ (arg arg* ...) body ...)
     `(lambda ()
        (lambda-case
         (((arg arg* ...) #f #f #f () ,(map ->gensym `(arg arg* ...))) (body-expander body ...)))))
    ((_ (arg arg* ... . args) body ...)
     `(lambda ()
        (lambda-case
         (((arg arg* ...) #f args #f () ,(map ->gensym `(arg arg* ... args))) (body-expander body ...)))))))

(define-syntax-rule (tree-il-define name what)
  `(define name ,what))
