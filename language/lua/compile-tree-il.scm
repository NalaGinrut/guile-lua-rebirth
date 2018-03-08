;;  Copyright (C) 2013,2014,2016,2017,2018
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
  #:use-module (language lua table)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (compile-tree-il))

(define (enable-issue1)
  (format (current-error-port) "ISSUE-1 fix enabled!~%")
  (lua-global-set! 'GUILE_LUA_ISSUE1 '((value (const #t))))
  (enable-lua-feature 'ISSUE-1))

(define (enable-guile-lua-extension)
  (format (current-error-port) "Guile Lua extension enabled!~%")
  (lua-global-set! 'GUILE_LUA_EXTENSION '((value (const #t))))
  (enable-lua-feature 'guile-lua-extension))

(define* (is-os-env-set? name #:optional (v "yes"))
  (cond
   ((getenv name)
    => (lambda (ev) (string= ev v)))
   (else #f)))

(define (lua-init)
  (if (is-os-env-set? "GUILE_LUA_ISSUE1")
      (enable-issue1)
      (format (current-error-port) "ISSUE-1 fix disabled!~%"))
  (if (is-os-env-set? "GUILE_LUA_EXTENSION")
      (enable-guile-lua-extension)
      (format (current-error-port) "Guile Lua extension disabled!~%")))

;; NOTE: For Guile-Lua-rebirth, we combine Lua and Scheme environment as possible,
;;       then you may define a var in Scheme, but reference it in Lua.
(define (predefine-all-toplevels e)
  (filter-map
   identity
   (hash-map->list (lambda (k v)
                     ;;(format #t "PRE: ~a~%" v)
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
        ;;(format #t "Tree-IL: ~%")
        ;;(pretty-print tree-il)
        tree-il)))
   env
   env))

(define (id-gensym x)
  (match x
    ('(void) #f)
    (('id id) (gensym id))
    (else (error id-gensym "Invalid id pattern!" x))))

;; NOTE: But Lua is imperitive language, so I'm afraid the binding (with let)
;;       is useless for it. Since imperitive languages uses assignment instead.
(define-syntax-rule (->body body name-list rename-list env)
  ;; TODO: find out all the lexical bindings to be replaced with lexical syntax of tree-il.
  ;;       This may need recursive and calling `comp' function.
  body)

;; <let>
;; GRAMMAR:
;; (let (name-list) (rename-list) (values-to-bind) body-in-tree-il)
;; e.g:
;; (let (x y) (x321 y123) ((const 1) (const 2)) (call (toplevel +) (lexical x x321) (lexical y y123))) ==> 3
;; NOTE: only x321 and y123, say, rename-list matters, so keep them identical in the body,
;;       name-list seems useless here, dunno...
(define-syntax-rule (->let name-list val-list body env)
  (let ((rename-list (map newsym name-list)))
    (for-each
     (lambda (var rename val)
       (lua-static-scope-set! env var `((rename ,rename) (value ,val))))
     name-list rename-list val-list)
    `(let ,name-list ,rename-list ,val-list ,(->body body name-list rename-list env))))

;; There's no let* in tree-il, so we have to handle it.
(define-syntax-rule (->let* name-list val-list body env)
  (if (null? name-list)
      `(let () () () ,body)
      (let ((rename-list (map newsym name-list)))
        (for-each
         (lambda (var rename val)
           (lua-static-scope-set! env var `((rename ,rename) (value ,val))))
         name-list rename-list val-list)
        `(let ,(list (car name-list)) ,rename-list ,(list (car val-list))
              (->let* (cdr name-list) (cdr val-list) body)))))

;; Generic letrec
(define-syntax-rule (->letrec name-list val-list body env)
  (let ((rename-list (map newsym name-list)))
    (for-each
     (lambda (var rename val)
       (lua-static-scope-set! env var `((rename ,rename) (value ,val))))
     name-list rename-list val-list)
    `(letrec ,name-list ,rename-list ,val-list ,(->body body name-list rename-list env))))

;; NOTE: only for rep expand, DON'T use it in other context
(define-syntax-rule (->letrec/rep name-list val-list body env)
  (let ((rename-list (map newsym name-list)))
    (for-each
     (lambda (var rename)
       (lua-static-scope-set! env var `((rename ,rename) (value (const nil)))))
     name-list rename-list)
    `(letrec ,name-list ,rename-list ,val-list ,(->body body name-list rename-list env))))

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

(define (multi-exps? x)
  (match x
    (('multi-exps rest ...) (display "multi-exps!\n") #t)
    (else (display "not multi-exps!\n") #f)))

(define* (extract-ids ids #:optional (no-void? #f))
  (filter-map
   (lambda (x)
     (match x
       ('(void)
        (if no-void?
            #f
            (if (check-lua-feature 'ISSUE-1)
                'void
                #f)))
       (`(id ,id) (string->symbol id))
       (else (error extract-ids "Invalid ids pattern!" x))))
   (if (multi-exps? (car ids))
       (cdar ids)
       (car ids))))

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

(define (->lexical-op renmaed-op)
  (lambda (op) `(lexical ,op ,renmaed-op)))
(define (->toplevel-op op) `(toplevel ,op))
(define (->op op) (->scope op is-toplevel-op?))
(define (->var var) (->scope var is-toplevel-var?))

(define (->call f trans args)
  (DEBUG "->call ~a ~a ~a~%" f trans args)
  (cond
   ((null? args) `(call ,(trans f)))
   (else `(call ,(trans f) ,@args))))

(define (body-expander e . body)
  (match body
    (() '())
    ((expr rest ...) `(,(comp expr e) ,@(apply body-expander rest)))
    (else (error body-expander "Invalid body pattern!" body))))

(define (->table-ref-func ns)
  (match ns
    (('namespace _ `(colon-ref (id ,func)))
     (display "has cref!\n")
     (values #t (string->symbol func)))
    (('namespace _ `(id ,func))
     (display "no cref!\n")
     (values #f (string->symbol func)))
    (else (error '->table-ref-func "BUG: Shouldn't be here!" ns))))

;; <lambda>
;; GRAMMAR:
;; (lambda () (lambda-case ((req opt rest kw inits gensyms) body) [alternate]))
;; NOTE: you have to use (lambda () ...) to create a closure first, than define the lambda-case in it.
;; e.g:
;; (define func (lambda () (lambda-case (((o) #f #f #f () (o123)) (call (toplevel +) (const 1) (lexical o o123))))))
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
;; NOTE: "body" shouldn't be applied "comp" within ->lambda, since we need to decide when to apply it in "comp".
(define-syntax ->lambda
  (syntax-rules ()
    ((_ e () body ...) ; thunk
     `(lambda-case
       ((() #f #f #f () ()) ,body ...)))
    ((_ e (arg arg* ...) body ...) ; common lambda
     (let ((renames (filter-map (lambda (s)
                                  (match s
                                    ('void #f)
                                    ((? symbol? id)
                                     (let ((ss (newsym id))
                                           (ssv (lua-local-ref e id)))
                                       ;;(format #t "->lambda[1]: Add `~a' to ENV~%" ss)
                                       (when (not (assoc-ref ssv 'rename))
                                         (DEBUG "Changing ~a to ~a~%" id ss)
                                         (lua-local-set! e id (list `(rename ,ss) (if ssv ssv '(value (const nil))))))
                                       ss))
                                    (else (error "->lambda: [1] Invalid id pattern!" s))))
                                `arg `arg* ...)))
       (cond
        ((null? renames) (->lambda e () body ...))
        (else
         `(lambda-case
           (((,@`arg ,@`arg* ...) #f #f #f () ,renames) ,body ...))))))
    ((_ e (arg arg* ... . args) body ...) ; optional-args lambda
     (let ((renames (filter-map (lambda (s)
                                  (match s
                                    ('void #f)
                                    ((? symbol? id)
                                     (let ((ss (newsym id))
                                           (ssv (lua-local-ref e id)))
                                       ;;(format #t "->lambda[2]: Add `~a' to ENV~%" ss)
                                       (when (not (assoc-ref ssv 'rename))
                                         (lua-local-set! e id (list `(rename ,ss) (if ssv ssv '(value (const nil))))))
                                       ss))
                                    (else (error "->lambda: [2] Invalid id pattern!" s))))
                                `(,@`arg ,@`arg* ... args))))
       (cond
        ((null? renames) (->lambda e () body ...))
        (else
         `(lambda-case
           (((,@`arg ,@`arg* ...) #f args #f () ,renames) ,body ...))))))))

(define (->return vals)
  (match vals
    (() '(const nil))
    (((x1 ...) (x2 ...) rest ...)
     `(call (primitive values) ,@vals))
    (else vals)))

(define (rep/cnd env cnd step body)
  ;; NOTE: To implement loop-break, we setup two continuations:
  ;; 1. "break-loop" continuation for jumping out of all mess
  ;; 2. "continue-loop" continuation for jumping back to the next round loop
  `(prompt
    #f
    (lexical %break-tag ,(get-rename env '%break-tag))
    (lambda ()
      ,(->lambda
        env ()
        (->letrec/rep
         '(%break-loop)
         `((lambda ((name . %break-loop))
             ,(->lambda
               env ()
               `(prompt
                 #f
                 (lexical %continue-tag ,(get-rename env '%continue-tag))
                 (lambda ()
                   ,(->lambda
                     env ()
                     (->letrec/rep
                      '(%continue-loop)
                      `((lambda ((name . %continue-loop))
                          ,(->lambda
                            env ()
                            `(begin
                               (if (call (primitive not) ,cnd)
                                   (void)
                                   (begin
                                     ,(comp body env) ; do the block
                                     ,step
                                     (call (lexical %continue-loop ,(get-rename env '%continue-loop)))))))))
                      `(call (lexical %continue-loop ,(get-rename env '%continue-loop))) ; continue to loop
                      env))) ; end (->lambda (->letrec ... continue-loop
                 (lambda ()
                   ,(->lambda
                     env ((kc))
                     ;; NOTE: If this prompt handler were triggered, it meant "abort" happened somewhere in the continue-loop.
                     ;;       Then we should call break-loop for jumping out of continue-loop. This will abandon the current
                     ;;       loop and setup a new loop with the current context.
                     `(begin
                        ,step
                        (call (lexical break-loop ,(get-rename env '%break-loop)))))))))) ; end (->lambda ... call-with-prompt
         ;; break-loop body, which is used to help to setup a new continue-loop with the current environment.
         `(call (lexical %break-loop ,(get-rename env '%break-loop)))
         env))) ; end (->lambda (->letrec ... break-loop
    (lambda () ,(->lambda env ((kb)) '(void))))) ; In Lua, the `for' statement doesn't return anything, so we just return unspecified value.

(define *special-var* '(self _G))

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

    ;; Tables
    ;; NOTE: All the keys in string are stored as symbols
    (('table rest ...)
     (let ((tbn (gensym "tb")))
       (->let
        (list tbn)
        (list (new-lua-table))
        `(begin
           ,@(map
              (lambda (pattern)
                (match pattern
                  (('tb-key-set! `(id ,k) v)
                   `(call (@ (language lua table) lua-table-set!)
                          (const ,tbn)
                          (lexical ,tbn ,(get-rename e tbn))
                          (const ,(string->symbol k))
                          ,(comp v e)))
                  (else (error 'table-operation "BUG: Shouldn't be here!" pattern))))
              rest)
           (lexical ,tbn ,(get-rename e tbn)))
        e)))

    ;; Namespaces
    (('namespace rest ...)
     (let-values (((t n p) (->tnp `(namespace ,@rest))))
       (let ((tt (if (lua-global-ref t)
                     `(toplevel ,t)
                     `(lexical ,t ,(get-rename e t)))))
         ;;(format #t "TT: ~a, ~a, ~a~%" tt n p)
         (cond
          ((and (not n) (not p)) ; return table
           tt)
          ((not p) ; table with one ref
           `(call (@ (language lua table) lua-table-ref)
                  ,tt
                  ,(id->key n)
                  (const ,t)))
          (else
           (cdr
            (fold (lambda (x y)
                    (match x
                      (`(id ,id)
                       (cons
                        `(const ,(string->symbol id))
                        `(call (@ (language lua table) lua-table-ref)
                               ,(cdr y)
                               ,(id->key x)
                               ,(car y))))
                      (else 'namespace "BUG[1]: Shouldn't be here!" x)))
                  (cons `(const ,t) tt) p)))))))

    (('multi-exps exps ...)
     ;;(format #t "EEE: ~a~%" exps)
     (map (lambda (exp) (comp exp e #:local-bind? local-bind?)) exps))

    ;; ref and assignment
    (`(print-var ,var)
     ;; EXTRA: print the value of variable in REPL, guile-lua specified
     (comp var e))
    (`(local (variable ,_vars)) ; local ref
     (let ((vars (fix-if-multi _vars (comp _vars e #:local-bind? #t))))
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
                      (lua-global-set! name `((value ,v)))
                      `(set! ,k ,v))
                     (`(lexical ,name ,rename)
                      (lua-static-scope-set! e name `((rename ,rename) (value ,v)))
                      `(set! ,k ,v))
                     (('call '(@ (language lua table) lua-table-ref) t kk tsym)
                      `(call (@ (language lua table) lua-table-set!)
                             ,tsym
                             ,t
                             ,kk
                             ,v)) ; table set
                     (else (error 'assignment
                                  (format #f "Assign ~a to ~a, name match failed" k v)))))
                 vars vals))))
    (`(local (assign ,_vars ,_vals))
     (let ((vars (fix-if-multi _vars (comp _vars e #:local-bind? #t)))
           (vals (fix-if-multi _vals (comp _vals e))))
       (for-each (lambda (k v)
                   (let (#;(lst (lua-static-scope-ref e k))
                         (rid (newsym k)))
                     #;(when (not lst))
                     (lua-static-scope-set! e k `((rename ,rid) (value ,v)))))
                 vars vals)
       ;; NOTE: Instead emit lexical assignment, we just use let for binding,
       ;;       or there'll be redundant assigment
       '(void)))
    (`(id ,id)
     ;; NOTE: here we will exploit
     (let ((symid (string->symbol id)))
       (cond
        (local-bind? #;(format #t "local binding: ~a~%" id) symid)
        ((is-lexical-bind? e symid)
         (format #t "exist lexical var: `~a'~%" symid)
         `(lexical ,symid ,(%rename symid)))
        ((lua-global-ref symid)
         (format #t "exist global var: `~a'~%" symid)
         `(toplevel ,symid))
        (else
         (cond
          ((and (memq symid *special-var*) ; special var, need to be lexical
                (lua-static-scope-ref e symid)) ; and was initialized
           `(lexical ,symid ,(%rename symid)))
          (else ; undefined global var, set to '(const nil)))))
           (lua-global-set! symid '((value (const nil))))
           ;;(format #t "non-exist global var: `~a'~%" symid)
           `(toplevel ,symid)))))))

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
     `(if ,(comp cnd e) ,(comp b1 e) (void)))
    (('if cnd 'then b1 else b2)
     `(if ,(comp cnd e)
          ,(comp b1 e)
          ,(comp b2 e)))
    (('if cnd 'then b1 'elseif c2 'then b2 . rest)
     (define (->if-rest x)
       (match x
         (() #t)
         (('elseif c 'then b . r)
          `(if ,(comp c e)
               ,(comp b e)
               ,(->if-rest r)))
         (else error ->if-rest "BUG: Shouldn't be here!" x)))
     `(if ,(comp cnd e)
          ,(comp b1 e)
          (if ,(comp c2 e)
              ,(comp b2 e)
              ,(->if-rest rest))))

    ;; control
    (('rep rest)
     ;; repeat tagging, just for debug
     (comp rest e))
    (('do-block ,body)
     (comp body e))
    ('(break)
     (let ((rename (get-rename e '%break-tag)))
       (when (not rename) (error 'break "BUG: Impossible here!"))
       `(abort (lexical %break-tag ,rename) () (const ()))))
    ('(continue)
     ;; NOTE: guile-lua-rebirth supports 'continue' keyword for better experience
     (let ((rename (get-rename e '%continue-tag)))
       (when (not rename) (error 'continue "BUG: Impossible here!"))
       `(abort (lexical %continue-tag ,rename) () (const ()))))
    (('for ('assign `(id ,v) ('range range ...)) body)
     ;; NOTE: We have to add prompts tags here, or there's no chance to add them later.
     (%lexical-var-set! e '%break-tag '(const break))
     (%lexical-var-set! e '%continue-tag '(const continue))
     (match range
       ((val1 val2)
        (comp `(local (assign (id ,v) ,val1)) e) ; set local var
        (let* ((vv (string->symbol v))
               (rename (get-rename e vv))
               (cnd (comp `(leq (id ,v) ,val2) e))
               (step `(set! (lexical ,vv ,rename) (call (primitive +) (lexical ,vv ,rename) (const 1)))))
          (rep/cnd e cnd step body)))
       ((val1 val2 val3)
        (comp `(local (assign (id ,v) ,val1)) e) ; set local var
        (let* ((vv (string->symbol v))
               (rename (get-rename e vv))
               (v3 (comp val3 e))
               (cnd  (comp `(leq (id ,v) ,val2) e))
               (step `(set! (lexical ,vv ,rename) (call (primitive +) (lexical ,vv ,rename) ,v3))))
          (rep/cnd e cnd step body)))
       (else error 'range "Invalid range syntax!" range)))

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
    (`(neq ,x ,y)
     (lua-neq (comp x e) (comp y e)))
    (`(geq ,x ,y)
     (lua-geq (comp x e) (comp y e)))
    (`(leq ,x ,y)
     (lua-leq (comp x e) (comp y e)))
    (`(not ,x)
     (lua-not (comp x e)))

    ;; functions
    (('func-call ('id func) ('args args ...))
     ;;(format #t "func-call: ~a~%" args)
     (cond
      ((is-lua-builtin-func? func)
       => (lambda (f)
            (if (null? args)
                (f)
                (f (comp (car args) e)))))
      (else
       (let ((symfunc (string->symbol func)))
         (->call symfunc
                 (cond
                  ((and (not (lua-global-ref symfunc))
                        (lua-static-scope-ref e symfunc))
                   (->lexical-op (%rename symfunc)))
                  (else ->toplevel-op))
                 (if (null? args)
                     args
                     (let ((x (car args)))
                       (if (multi-exps? x)
                           (comp x e)
                           (list (comp x e))))))))))
    (('func-colon-call ('namespace ns ...) ('args args ...))
     (let-values (((_ func) (->table-ref-func `(namespace ,@ns))))
       (let* ((self (comp (->drop-func-ref `(namespace ,@ns)) e))
              (args-list (cond
                          ((null? args) args)
                          ((multi-exps? (car args)) (comp (car args) e))
                          (else (list (comp (car args) e)))))
              ;; The original Lua colon-ref, passing `self' as the first argument.
              ;; This argument is hidden since it doesn't apppear in users code directly.
              ;; The hidden parameter has already been inserted when defining the Lua function.
              ;; We still check if the first parameter name is `self' here, if it's false, then
              ;; there's fatal bug.
              (nargs-list (if (check-lua-feature 'ISSUE-1)
                              args-list
                              (cons self args-list)))
              ;; NOTE: tf is to get the function from self which is actually a table in Lua
              (tv `(call (@ (language lua table) lua-table-ref)
                         ,self
                         (const ,func)
                         ,(get-nearest-namespace self)))
              (self-rename `(call (toplevel car) ,tv))
              (tf `(call (toplevel cdr) ,tv)))
         (->call func (lambda (_) tf) nargs-list))))
    (('func-call ('namespace ns ...) ('args args ...))
     (let-values (((_ func) (->table-ref-func `(namespace ,@ns))))
       (let* ((self (comp (->drop-func-ref `(namespace ,@ns)) e))
              (args-list (cond
                          ((null? args) args)
                          ((multi-exps? (car args)) (comp (car args) e))
                          (else (list (comp (car args) e)))))
              (nargs-list (cons (if (check-lua-feature 'ISSUE-1)
                                    self ; pass in the real self
                                    (comp '(id "self") e)) ; pass the value of `self' var
                                args-list))
              ;; NOTE: tf is to get the function from self which is actually a table in Lua
              (tf `(call (toplevel cdr)
                         (call (@ (language lua table) lua-table-ref)
                               ,self
                               (const ,func)
                               ,(get-nearest-namespace self)))))
         (->call func (lambda (_) tf) nargs-list))))
    (('func-def `(id ,func) ('params p ...) body)
     (let ((new-env (new-scope e)))
       `(define
          ,(string->symbol func)
          (lambda ((name . ,(string->symbol func)))
            ,(->lambda e (,(extract-ids p)) (comp body new-env))))))
    (('func-def ('namespace ns ...) ('params p ...) body)
     (let ((self (comp (->drop-func-ref `(namespace ,@ns)) e)))
       (let-values (((cref? func) (->table-ref-func `(namespace ,@ns))))
         (let* ((sn (if cref?
                        ;; if colon-ref, self must be bound to `self' variable.
                        (let ((self-rename (newsym 'self)))
                          (lua-static-scope-set! e 'self
                                                 `((rename ,self-rename) (value ,self)))
                          self-rename)
                        ;; point-ref, in this case, the first arg must be always self
                        ;; but never bound to `self' variable inexcplictly. That means
                        ;; `self' variable would be `nil' in the most general case.
                        #f))
                (zz (append
                     (if (check-lua-feature 'ISSUE-1)
                         '()
                         '(self)) ; the original Lua should pass `self' as hidden argument
                     (extract-ids p (check-lua-feature 'ISSUE-1))))
                ;; NOTE: This new env is only for the new lambda scope, or we can't record
                ;;       `self' correctly. Don't use it in previous context/exprs, it should
                ;;       absoultely clean before pass into the lambda. Although lambda has its
                ;;       own new scope in the low-level, we still need this new env to record
                ;;       things before the Lua code compiled to tree-il.
                ;; NOTE: We can't create this new env in ->lambda, since the exprs in body should
                ;;       call `comp' with this new env. And we have no chance to do it within,
                ;;       since the macro omits all the exprs to "body ...".
                (new-env (new-scope e))
                (refexp
                 `(call (@ (language lua table) lua-table-set!)
                        ,(get-nearest-namespace self)
                        ,self
                        (const ,func)
                        (call (toplevel cons)
                              (const ,sn)
                              (lambda ()
                                ,(->lambda
                                  new-env
                                  (,(append
                                     (if (check-lua-feature 'ISSUE-1)
                                         '()
                                         ;; NOTE: If colon-ref, then no `self' as arg explicitly.
                                         ;;       Because the self MAY be applied as an arg, so we
                                         ;;       always put the first parameter as `_' to prevent
                                         ;;       the parity inconsistent.
                                         (if cref? '(self) '(_)))
                                     (extract-ids p (check-lua-feature 'ISSUE-1))))
                                  (comp body new-env)))))))
           ;; If you enabled ISSUE-1 fix:
           ;; NOTE: Yes, we changed something from the original Lua.
           ;;       No matter colon-ref or point-ref, say,
           ;;       a.b.c:func(), or a.b.c.func() will return `self' which has
           ;;       been confirmed in the definition.
           ;;       It's different from Lua5.2, which will return nil if you use point-ref for applying,
           ;;       even if you define the function with colon-ref. It's RIDICULOUS!
           ;;       No one wants to get nil from `self' in the reasonable code, unless they forget define
           ;;       the function with colon-ref.
           ;;       But it's reasonable to use point-ref in definition, since sometimes we need to refer `self'
           ;;       which isn't bind to the current table. If users use point-ref for definition, the `self' may
           ;;       be bind to the lexical variable out of the current scope. It's useful sometimes.
           ;; NOTE: We don't have to drop `self' table here if cref is #f, since `self' will be created as global,
           ;;       and it'll be referenced as global. All these codes are confirmed in compile time. So it won't
           ;;       reference `self' as lexical at all, say, the generated tree-il will be:
           ;;       (let (self) (list sn) (list self) ... (toplevel self) ...)
           ;;       Although `self' was bound in `let', but it's always referenced with `toplevel' command.
           (if (check-lua-feature 'ISSUE-1)
               `(let (self)
                  ,(list sn)
                  ,(list self)
                  ,refexp)
               ;; NOTE: The original Lua colon-ref, which means to refer `self' without binding to the nearest
               ;;       environment, but pass it as a hidden argument.
               (pk "refexp0: " refexp))))))
    (('local ('func-def `(id ,func) ('params p ...) body))
     (let ((new-env (new-scope e)))
       `(define
          (lexical ,(string->symbol func))
          (lambda ((name . ,(string->symbol func)))
            ,(->lambda new-env (,(extract-ids p)) (comp body new-env))))))
    (('anon-func-def ('params p ...) body)
     (let ((new-env (new-scope e)))
       `(lambda () ,(->lambda new-env (,(extract-ids p)) (comp body new-env)))))
    (('return vals ...)
     (->return (if (null? vals) vals (comp (car vals) e))))

    ;; TODO: finish the rest
    (else (error comp "invalid src" src))))
