;;  Copyright (C) 2013
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
  #:use-module (language tree-il)
  #:export (compile-tree-il))

(define empty-lexical-environment
  (make-hash-table))

(define-syntax-rule (@implv sym)
  (-> (@ '(language lua impl) 'sym)))

(define-syntax-rule (@impl sym arg ...)
  (-> (apply (@implv sym) arg ...)))

(define (lua-init)
  #t) ;; nothing to do yet.

(define (compile-tree-il exp env opts)
  (values
   (parse-tree-il
    (begin (lua-init)
           (comp exp empty-lexical-environment)))
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
;; (put 'pmatch/source 'scheme-indent-function 1)

(define (comp src e)
  (pmatch/source src
    (nil
     (-> (const 'nil)))
    (true
     (-> (const 'true)))
    (false
     (-> (const 'false)))
    ((number ,x)
     (-> (const x)))
    ((string ,x)
     (-> (const x)))
    ((variable ,x)
     (lookup x e))
    ((store ,x ,v)
     (store x (comp v e) e))
    ;; FIXME: we need lexical scope
    ((begin ,form)
     (comp form e))
    ((begin . ,forms)
     `(begin ,@(map (lambda (x) (comp x e)) forms)))
    ((add ,x ,y)
     (@impl lua-add (comp x e) (comp y e)))
    ((minu ,x ,y)
     (@impl lua-minus (comp x e) (comp y e)))
    ((multi ,x ,y)
     (@impl lua-multi (comp x e) (comp y e)))
    ((div ,x ,y)
     (@impl lua-div (comp x e) (comp y e)))
    ((mod ,x ,y)
     (@impl lua-mod (comp x e) (comp y e)))
    ((expt ,x ,y)
     (@impl lua-expt (comp x e) (comp y e)))
    ((func-call ,prefix-exp ,name ,args)
     (@impl (comp prefix-exp e) (comp name e) (comp args e)))
    ;; TODO: finish the rest
    ))
    
