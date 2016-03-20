;;  Copyright (C) 2013,2014,2016
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
  #:use-module (language lua impl)
  #:use-module (language lua optimize)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (compile-tree-il))

(define (lua-init)
  #t) ; nothing to do yet.

;; NOTE: For Guile-Lua-rebirth, we combine Lua and Scheme environment as possible,
;;       then you may define a var in Scheme, but reference it in Lua.
(define (predefine-all-toplevels e)
  (filter-map
   identity
   (hash-map->list (lambda (k v)
                     (format #t "PRE: ~a~%" v)
                     (and (not (defined? k))
                          `(define ,k ,(and=> (assoc-ref v 'value) car))))
                   (lua-env-symbol-table e))))

(define (compile-tree-il exp env opts)
  (values
   (parse-tree-il
    (begin
      (lua-init)
      (let* ((xxx (comp (lua-optimize exp env)
                        (current-top-level-environment)))
             (tree-il `(begin
                         ,@(predefine-all-toplevels (current-top-level-environment))
                         ,xxx)))
        (format #t "Tree-IL: ~%")
        (pretty-print tree-il)
        tree-il)))
   env
   env))

(define (lookup name env)
  (hash-ref env name))

(define (store name value env)
  (hash-set! env name value))

(define (let1 what proc)
  (let ((sym (gensym))) 
    (-> (let (list sym) (list sym) (list what) (proc sym)))))

(define (begin1 what proc)
  (let1 what (lambda (v)
               (-> (begin (proc v)
                          (-> (lexical v v)))))))

(define (id-gensym x)
  (match x
    ('(void) #f)
    (('id id) (gensym id))
    (else (error id-gensym "Invalid id pattern!" x))))

;; NOTE: But Lua is imperitive language, so I'm afraid the binding (with let)
;;       is useless for it. Since imperitive languages uses assignment instead.
(define (->body body name-list rename-list env)
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

(define (gen-let-syntax ast e)
  (define (wrap-let node symtab)
    (call-with-values
        (lambda ()
          (unzip3
           (hash-map->list
            (lambda (k v) (list k (car (assoc-ref v 'rename)) (car (assoc-ref v 'value))))
            symtab)))
      (lambda (vars renames vals)
        `(let ,vars ,renames ,vals ,node))))
  (match ast
    (('scope rest)
     (wrap-let (gen-let-syntax rest (lua-env-upper-frame e)) (lua-env-symbol-table e)))
    (else ast)))

(define *toplevel-ops* '())

(define (is-toplevel-op? o)
  ;; NOTE: the-root-module only contains primitives, which is proper for this function.
  (or (defined? o the-root-module)
      (memq o *toplevel-ops*)))

(define (extract-ids ids)
  (map (lambda (x)
         (match x
           ('(void) 'void)
           (`(id ,id) (string->symbol id))
           (else (error extract-ids "Invalid ids pattern!" x))))
       ids))

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

(define (multi-exps? x)
  (match x
    (('multi-exps rest ...) #t)
    (else #f)))

(define (->scope id pred)
  (cond
   ((pred id) `(toplevel ,id))
   (else `(lexical ,id ,(newsym id)))))

(define (->toplevel-op op) `(toplevel ,op))
(define (->op op) (->scope op is-toplevel-op?))
(define (->var var) (->scope var is-toplevel-var?))

(define (->call f trans args)
  (cond
   ((null? args) `(call ,(trans f)))
   (else `(call ,(trans f) ,@args))))

(define (body-expander e . body)
  (match body
    (() '())
    ((expr rest ...) `(,(comp expr e) ,@(apply body-expander rest)))
    (else (error body-expander "Invalid body pattern!" body))))

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
;; e.g: (lambda () (lambda-case (((x) #f #f #f () (x1)) (const 3))))
;;
;; 3. (lambda (. y) 3)
;; e.g: (lambda () (lambda-case ((() #f y #f () ()) (const 3))))
;;
;; 4. (lambda (x y . z) 3)
;; e.g: (lambda () (lambda-case (((x y) #f z #f () (x1 y1 z2)) (const 3))))
;;
;; NOTE: *Maybe* we don't need to support kargs (in Lua), hmm...dunno
;; NOTE: There's no actual lambda-case syntax in Lua, so `alternate' is useless.
;; TODO: use procedual Tree-IL to keep source information.
(define-syntax ->lambda
  (syntax-rules ()
    ((_ e () body ...) ; thunk
     `(lambda-case
       ((() #f #f #f () ()) ,(comp body ... e))))
    ((_ e (arg arg* ...) body ...) ; common lambda
     (let ((renames (filter-map (lambda (s)
                                  (match s
                                    ('void #f)
                                    ((? symbol? id)
                                     (let ((ss (newsym id))
                                           (ssv (lua-static-scope-ref e id)))
                                       (format #t "->lambda[1]: Add `~a' to ENV~%" ss)
                                       (when (not (assoc-ref ssv 'rename))
                                             (lua-static-scope-set! e id (cons `(rename ,ss) (if ssv ssv '()))))
                                       ss))
                                    (else (error "->lambda: [1] Invalid id pattern!" s))))
                                arg arg* ...)))
       (cond
        ((null? renames) (->lambda e () body ...))
        (else
         `(lambda-case
           (((,@arg ,@arg* ...) #f #f #f () ,renames) ,(comp body ... e)))))))
    ((_ e (arg arg* ... . args) body ...) ; optional-args lambda
     (let ((renames (filter-map (lambda (s)
                                  (match s
                                    ('void #f)
                                    ((? symbol? id)
                                     (let ((ss (newsym id))
                                           (ssv (lua-static-scope-ref e id)))
                                       (format #t "->lambda[2]: Add `~a' to ENV~%" ss)
                                       (when (not (assoc-ref ssv 'rename))
                                             (lua-static-scope-set! e id (cons `(rename ,ss) (if ssv ssv '()))))
                                       ss))
                                    (else (error "->lambda: [2] Invalid id pattern!" s))))
                                arg arg* ... args)))
       (cond
        ((null? renames) (->lambda e () body ...))
        (else
         `(lambda-case
           (((,@arg ,@arg* ...) #f args #f () ,renames) ,(comp body ... e)))))))))

(define (->return vals)
  (match vals
    (() '(const nil))
    (((x1 ...) (x2 ...) rest ...)
     `(call (primitive values) ,@vals))
    (else vals)))

;; for emacs:
;; (put 'match 'scheme-indent-function 1)

(define* (comp src e #:key (local-bind? #f))
  (define (%rename x)
    (let ((ssv (get-val-from-scope x e)))
      (or (and=> (assoc-ref ssv 'rename) car)
          (error %rename "%rename: Invalid pattern!" ssv))))
  (display "----------------------[Enter]-------------------------\n")
  (display src)(newline)
  (print-lua-env e)
  (match src
    ;; Literals
    ('(marker nil)
     '(const nil))
    ('(boolean true)
     '(const #t))
    ('(boolean false)
     '(const #f))
    (`(number ,x)
     `(const ,x))
    (`(string ,x)
     `(const ,x))

    (('multi-exps exps ...)
     (format #t "EEE: ~a~%" exps)
     (map (lambda (exp) (comp exp e #:local-bind? local-bind?)) exps))

    ;; ref and assignment
    (`(variable ,_vars) ; global ref
     (let ((vars (fix-if-multi _vars (comp _vars e))))
       vars))
    ;; (`(assign (id ,id) ,v) ; global assignment
    ;;  (let ((symid (string->symbol id))
    ;;        (vv (comp v e)))
    ;;    (when (not (lua-global-ref symid))
    ;;          (format #t "assign: Add var `~a' to ENV~%" symid)
    ;;          (lua-global-set! symid vv))
    ;;    `(begin
    ;;       (define ,symid ,vv)
    ;;       (set! (toplevel ,symid) ,vv))))
    (`(local (variable ,_vars)) ; local ref
     (let ((vars (fix-if-multi _vars (comp _vars e #:local-bind? #t))))
       (format #t "LOC: ~a~%" vars)
       (for-each (lambda (v)
                   (lua-static-scope-set! e v `((rename ,(newsym v)) (value (const nil)))))
                 vars)
       '(void)))
    (`(assign ,_vars ,_vals)
     (let ((vars (fix-if-multi _vars (comp _vars e)))
           (vals (fix-if-multi _vals (comp _vals e))))
       `(begin
          ,@(map (lambda (k v)
                   (match k
                     (`(toplevel ,name)
                      (lua-global-set! name `((value ,v))))
                     (`(lexical ,name ,rename)
                      (lua-static-scope-set! e name `((rename ,rename) (value ,v))))
                     (else (error `(assign ,_vars ,_vals) "name match failed!" k)))
                   `(set! ,k ,v))
                 vars vals))))
    (`(local (assign ,_vars ,_vals))
     (let ((vars (fix-if-multi _vars (comp _vars e #:local-bind? #t)))
           (vals (fix-if-multi _vals (comp _vals e))))
       (for-each (lambda (k v)
                   (let ((lst (lua-static-scope-ref e k))
                         (rid (newsym k)))
                     (when (not lst)
                           (format #t "local assign: Add var `~a' to ENV~%" k)
                           (lua-static-scope-set! e k `((rename ,rid) (value ,v))))))
                 vars vals)
       ;; NOTE: Instead emit lexical assignment, we just use let for binding,
       ;;       or there'll be redundant assigment
       '(void)))
    (`(id ,id)
     ;; NOTE: here we will exploit
     (let ((symid (string->symbol id)))
       (cond
        (local-bind? (format #t "local binding: ~a~%" id) symid)
        ((and (not (lua-global-ref symid)) (lua-static-scope-ref e symid))
         `(lexical ,symid ,(%rename symid)))
        ((lua-global-ref symid)
         (format #t "exist global var: `~a'~%" symid)
         `(toplevel ,symid))
        (else
         (lua-global-set! symid '((value (const nil))))
         (format #t "non-exist global var: `~a'~%" symid)
         `(toplevel ,symid)))))

    ;; scope and statment
    (('scope rest)
     (cond
      ((eq? (car rest) 'func-def)
       ;; NOTE: Don't produce `let' instruction just outside the function definition.
       (comp rest (new-scope e)))
      (else
       (let ((new-env (new-scope e)))
         (gen-let-syntax `(scope ,(comp rest new-env)) new-env)))))
    (('begin form)
     (comp form e))
    (('begin forms ...)
     `(begin ,@(map (lambda (x) (comp x e)) forms)))
    (('if cnd 'then b1)
     (and (comp cnd e) (comp b1 e)))
    (('if cnd 'then b1 else b2)
     (if (comp cnd e)
         (comp b1 e)
         (comp b2 e)))
    (('if cnd 'then b1 'elseif c2 'then b2 . rest)
     (define (->if-rest x)
       (match x
         (() #t)
         (('elseif c 'then b . r)
          (if (comp c e)
              (comp b e)
              (->if-rest r)))
         (else error ->if-rest "BUG: Shouldn't be here!" x)))
     (if (comp cnd e)
         (comp b1 e)
         (if (comp c2 e)
             (comp b2 e)
             (->if-rest rest))))

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
     (lua-expt (comp x e) (comp y e)))

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
    (`(not ,x)
     (lua-not (comp x e)))

    ;; functions
    (('func-call ('id func) ('args args ...))
     (format #t "func-call: ~a~%" args)
     (cond
      ((is-lua-builtin-func? func)
       => (lambda (f)
            (if (null? args)
                (f)
                (f (comp (car args) e)))))
      (else
       (->call (string->symbol func)
               ->toplevel-op
               (if (null? args)
                   args
                   (let ((x (car args)))
                     (if (multi-exps? x)
                         (comp x e)
                         (list (comp x e)))))))))
    (('func-def `(id ,func) ('params p ...) body)
     (format #t "func-def: ~a~%" p)
     `(define
        ,(string->symbol func)
        (lambda ((name . ,(string->symbol func)))
          ,(->lambda e ((extract-ids p)) body))))
    (`(local (func-def (id ,func) (params ,p ...) ,body))
     `(define
        (lexical ,(string->symbol func))
        (lambda ((name . ,(string->symbol func)))
          ,(->lambda e ((extract-ids p)) body))))
    (('anon-func-def body ...)
     ;; TODO: implement anonymous function
     #t)
    (('return vals ...)
     (format #t "return: ~a~%" vals)
     (->return (if (null? vals) vals (comp (car vals) e))))

    ;; TODO: finish the rest
    (else (error comp "invalid src" src))))
