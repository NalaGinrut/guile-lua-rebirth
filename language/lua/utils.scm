;;  Copyright (C) 2013,2014,2016,2017
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

(define-module (language lua utils)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system base lalr)
  #:use-module (system base pmatch)
  #:use-module (rnrs records syntactic)
  #:use-module (rnrs records procedural)
  #:use-module (rnrs records inspection)
  #:export (location
	    unget-char1
	    syntax-error
	    lex-error
	    *eof-object*
	    make-reader
	    port-source-location
	    return
	    ->
            ->fixop
            %>
            debug-it
	    
	    new-stack
	    new-queue
	    stack-pop!
	    stack-push!
	    stack-top
	    stack-empty?
	    
	    queue-out!
	    queue-in!
	    queue-head
	    queue-tail
	    queue-empty?
	    
	    hash-keys
	    range

	    make-compiler
	    make-file-compiler
	    make-token-checker

            ->lua
            @impl
            @impv

            ->list
            newsym
            compile-string
            fix-for-cross
            fix-if-multi
            in-repl?
            id->key
            ->tnp
            ->drop-func-ref
            get-nearest-namespace
            check-lua-feature
            enable-lua-feature
            DEBUG))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))

(define (unget-char1 c port)
  (and (char? c) (unread-char c port)))

(define* (syntax-error message #:optional token)
  (if (lexical-token? token)
      (throw 'syntax-error #f message
             (and=> (lexical-token-source token)
                    source-location->source-properties)
             (or (lexical-token-value token)
                 (lexical-token-category token))
             #f)
      (throw 'syntax-error #f message #f token #f)))

(define (lex-error what loc form . args)
  (throw 'lex-error #f what
         (and=> loc source-location->source-properties)
         form #f args))

(define *eof-object*
  (call-with-input-string "" read-char))

(define (make-reader make-parser make-tokenizer port)
  (let ((parse (make-parser)))
    (parse (make-tokenizer port) syntax-error)))

(define-syntax-rule (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

(define-syntax-rule (return port category value)
  (make-lexical-token category (port-source-location port) value))

(define-syntax-rule (-> (type arg ...))
  `(type ,arg ...))

(define-syntax-rule (->fixop l1 l2)
  `(,(car l2) ,l1 ,(cadr l2)))

;; for debug
(define (normal-compute func . args)
  (apply func args))
(define (debug-it func . args)
  `(,(procedure-name func) ,@args))
(define %%> (make-parameter normal-compute))
(define %> (%%>))

(define new-stack make-q)
(define new-queue make-q)

(define stack-pop! q-pop!)
(define stack-push! q-push!)
(define stack-top q-front)
(define stack-empty? q-empty?) 

(define queue-out! q-pop!)
(define queue-in! enq!)
(define queue-head q-front)
(define queue-tail q-rear)
(define queue-empty? q-empty?)

(define (hash-keys ht)
  (hash-map->list (lambda (k v) k) ht))

(define* (range from to #:optional (step 1))
  (iota (- to from) from step))

;; debug utils

;; e.g: (make-compiler 'ecmascript)
;; (put 'lambda* 'scheme-indent-function 1)
(define* (make-compiler lang)
  (lambda* (src #:key (to 'value))
    (let* ((language (lookup-language lang))
           (reader (language-reader language)))
      (compile (call-with-input-string src reader)
               #:from lang #:to to))))

(define (make-file-compiler lang)
  (lambda* (file #:key (to 'value))
    (let* ((language (lookup-language lang))
           (reader (language-reader language)))
      (cond
       ((file-exists? file)
        (compile (call-with-input-file file reader)
                 #:from lang #:to to))
       (else (error "no such a file" file))))))

(define* (make-token-checker tokenizer)
  (lambda* (src #:optional (mode 'slim))
    (let ((tokens (call-with-input-string src tokenizer)))
    (case mode
      ((slim) (map lexical-token-category tokens))
      ((all) tokens)
      (else (error make-token-checker "wrong mode" mode))))))

(define (->lua x)
  (string->symbol (string-append "lua-" x)))

(define-syntax-rule (@impv sym)
  (module-ref (resolve-module '(language lua impl)) sym))

(define-syntax-rule (@impl func args ...)
  ((@impv func) args ...))

(define (->list n) (and n (record->list n)))

(define* (record->list record #:optional (alist #f))
  (define (record-ref rtd k)
    ((record-accessor rtd k) record))
  (define (gen-val rtd k i)
    (let ((v (record-ref rtd i)))
      (if alist
          (cons k v)
          v)))
  (let* ((rtd (record-rtd record))
         (p (record-type-parent rtd))
         (name (record-type-name rtd))
         (pfields (if p (vector->list (record-type-field-names p)) '()))
         (plen (if p (length pfields) 0))
         (fields (vector->list (record-type-field-names rtd)))
         (len (length fields)))
    (append `(,name 
              ,@(map (lambda (k i) (gen-val p k i)) pfields (iota plen))
              ,@(map (lambda (k i) (gen-val rtd k i)) fields (iota len))))))

;; Sometimes we need to apply gensym on a symbol rather than string,
;; so this helper function could be useful.
(define (newsym sym)
  (gensym (symbol->string sym)))

(define (compile-string str . opts)
  (apply read-and-compile (open-input-string str) opts))

(define (fix-for-cross o)
  (match o
    (('const v) v)
    (else o)))

(define (fix-if-multi vs x)
  (match vs
    (('multi-exps rest ...) x)
    (else (list x))))

(define (in-repl?)
  (not (null? (fluid-ref *repl-stack*))))

(define (id->key o)
    (match o
      (`(id ,x) `(const ,(string->symbol x)))
      (else (error 'id->key "BUG: Shouldn't be here!" o))))

;; symbol -> id-list -> id-list
;; table name -> final ref -> table ref
(define (->tnp rr)
  (define (ns->lst ns)
    (match ns
      (() '())
      (`(id ,x) ns)
      (`(namespace ,rest (colon-ref ,p2))
       ;; get rid of colon-ref, since it's already handled before
       `(,@(ns->lst rest)))
      (`(namespace (id ,p1) (id ,p2))
       (list `(id ,p1) `(id ,p2)))
      (('namespace rest p1)
       `(,@(ns->lst rest) ,(ns->lst p1)))
      (else (error 'ns->lst "BUG[1]: Shouldn't be here!" ns))))
  (define (->sym o)
    (match o
      (`(id ,x) (string->symbol x))
      (else (error '->sym "BUG[2]: Shouldn't be here!" o))))
  (define r (ns->lst rr))
  (cond
   ((null? r) (error 'namespace "BUG[3]: Shouldn't be here!" r))
   ((= (length r) 1) (values (->sym r) #f #f))
   ((= (length r) 2) (values (->sym (car r)) (cadr r) #f))
   (else (values
          (->sym (car r))
          (list-head r (- (length r) 2))
          (list-tail r 1)))))

(define (->drop-func-ref ns)
  (match ns
    (('namespace p _) p)
    (else (error '->drop-func-ref "BUG: Shouldn't be here!" ns))))

(define (get-nearest-namespace self)
  (car (list-tail self (1- (length self)))))

(define *lua-features*
  '((ISSUE-1 . #f)
    (guile-lua-extension . #f)))
(define (check-lua-feature f)
  (let ((ret (assoc-ref *lua-features* f)))
    (format #t "~a is ~a~%" f ret)
    ret))
(define* (enable-lua-feature f #:optional (v #t))
  (assoc-set! *lua-features* f v))

(define (DEBUG fmt . args)
  (apply format #t fmt args))
