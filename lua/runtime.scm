;;; Guile Lua --- runtime functionality

;;; Copyright (C) 2010 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;; "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;; 2013/06/22 modified for guile-lua-rebirth

;;; Code:

(define-module (language lua runtime)
  #:use-module (language lua utils)
  #:use-module ((srfi srfi-1) #:select (filter! list-index))
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-98) #:select (get-environment-variable))
  #:use-module ((system base compile) #:select (compile read-and-compile))
  #:use-module ((system vm program) #:select (make-program))

  #:export (runtime-error

            ;; misc
            value-type->string
            assert-type
            assert-table
            assert-string
            assert-number

            ;; tables
            make-table
            table?
            table-slots
            table-metatable
            table-metatable!
            table-length

            ;; userdata
            userdata
            userdata-metatable
            register-userdata!

            ;; metatable
            might-have-metatable?
            get-metatable
            dispatch-metatable-event

            ;; table interaction
            index
            new-index!
            get-field

            ;; operators
            len unm eq lt le gt ge add sub mul div pow
            neq concat

            ;; modules
            make-module-table

            ;; global environment
            *global-env-table*
            save-fenv
            check-global-function))

;; Local Variables:
;; eval: (put 'define-global 'scheme-indent-function 1)
;; End:

(define (runtime-error string . arguments)
  "Throw an error tagged with 'lua-runtime"
  (throw 'lua-runtime
         (apply format #f (string-append "Lua: ERROR: " string "\n")
                arguments)))

(define (runtime-warning string . arguments)
  (apply format #t (string-append "Lua: RUNTIME WARNING: " string "\n")
         arguments))

;;;;; MISCELLANEOUS

(define (value-type->string x)
  (cond ((table? x) "table")
        ((string? x) "string")
        ((number? x) "number")
        ((boolean? x) "boolean")
        ((eqv? x 'nil) "nil")
        ((procedure? x) "function")
        ;; TODO: value-type->string must recognize threads
        (else "userdata")))

(define (assert-type argument caller expected value predicate)
  (if (not (predicate value))
      (runtime-error "bad argument ~a to '~a' (~a expected, got ~a)"
                     argument caller expected (value-type->string value))))

(define-syntax define-assert
  (syntax-rules ()
    ((_ name string predicate)
     (define (name argument caller value)
       (assert-type argument caller string value predicate)))))

(define-assert assert-table "table" table?)
(define-assert assert-string "string" string?)
(define-assert assert-number "number" number?)

;;;;; TABLES

(define-record-type table
  (%make-table metatable slots)
  %table?
  (metatable %table-metatable %table-metatable!)
  (slots %table-slots))

(define (make-table)
  (%make-table #f (make-hash-table)))
(define (table? t)
  (%table? t))
(define (table-metatable t)
  (%table-metatable t))
(define (table-metatable! t meta)
  (%table-metatable! t meta))
(define (table-slots t)
  (%table-slots t))

;;;;; USERDATA

;; Userdata is tracked by this property. It can be #f, indicating the
;; object is not userdata, #t, indicating the object is userdata but has
;; no metatable, or an actual table, which counts as the metatable.
(define userdata-property (make-object-property))

(define userdata? userdata-property)
(define (userdata-metatable x)
  (and (table? (userdata-property x)) (userdata-property x)))

(define* (register-userdata! x #:optional (metatable #t))
  (set! (userdata? x) metatable))

;;;;; METATABLES

(define (might-have-metatable? x)
  (or (table? x) (userdata? x)))

(define (get-metatable x)
  (cond ((table? x) (table-metatable x))
        ((userdata? x) (userdata-metatable x))
        (else #f)))

;;;;; TABLE INTERACTION

(define (dispatch-metatable-event key default x . arguments)
  (let ((metatable (get-metatable x)))
    (apply
     (if metatable
         (hash-ref (table-slots metatable) key default)
         default)
     arguments)))

;; see manual section 2.5.5
(define (table-length table)
  (let ((numeric-keys
         (sort! (filter! number? (hash-keys (table-slots table))) <)))
    (if (eq? (car numeric-keys) 1)
        (let lp ((cell (car numeric-keys))
                 (rest (cdr numeric-keys))
                 (length 0))
          ;; length does not count "holes"
          ;; so if a table has the keys 1,2,3 and 5, the length of the table is 3
          (if (or (> cell (+ length 1)) (null? rest))
              (+ length 1) ;; add one to length as though we had started from one
              (lp (car rest) (cdr rest) cell)))
        0)))

(define (index table key)
  (dispatch-metatable-event
   "__index"
   (lambda (table key) (hash-ref (table-slots table) key 'nil))
   table
   table key))

(define (new-index! table key value)
  (dispatch-metatable-event
   "__newindex"
   (lambda (table key value) (hash-set! (table-slots table) key value))
   table
   table key value))

(define* (get-field table key #:optional (default 'nil))
  (let ((result (index table key)))
    (if (eqv? result 'nil)
        default
        result)))

;;;;; OPERATORS
(define (len a)
  "A function backing the unary # (length) operator"
  (cond
   ((string? a) (string-length a))
   ((table? a) (table-length a))
   (else (runtime-error "attempt to get length of a ~A value"
                        (value-type->string a)))))

(define (unm a)
  "A function backing the unary - (negation) operator"
  (if (might-have-metatable? a)
      (dispatch-metatable-event "__unm" - a a)
      (- a)))

(define (builtin-eq a b)
  "A function backing the == operator"
  (equal? a b))

(define (builtin-concat a b)
  (when (or (table? a) (table? b))
    (runtime-error "attempt to concatenate a table value"))
  (when (or (eqv? a 'nil) (eqv? b 'nil))
    (runtime-error "attempt to concatenate a nil value"))
  (when (or (boolean? a) (boolean? b))
    (runtime-error "attempt to concatenate a boolean value"))
  (format #f "~a~a" a b))

(define (neq a b)
  "An inversion of eq"
  (not (eq a b)))

(define (ge a b)
  "A function backing the >= (greater-than-or-equal-to) operator"
  (not (lt a b)))

(define (gt a b)
  "A function backing the > (greater-than) operator"
  (not (le a b)))

;; This macro could be even cooler and generate the slot names as well as the
;; parsers name/function mappings at expand-time
(letrec-syntax
    ((define-binary-operators
      (syntax-rules ()
        ((_ (name slot-name default) ...)
         (begin
           (define-binary-operators () name slot-name default)
           ...))
        ((_ () name slot-name default)
         (begin
           (define (name a b)
             (cond ((might-have-metatable? a)
                    (dispatch-metatable-event slot-name default a a b))
                   ((might-have-metatable? b)
                    (dispatch-metatable-event slot-name default b a b))
                   (else (default a b)))))))))
  (define-binary-operators
   (add "__add" +)
   (sub "__sub" -)
   (mul "__mul" *)
   (div "__div" /)
   (expt "__expt" expt)
   (le "__le" <=)
   (lt "__lt" <)
   (eq "__eq" builtin-eq)
   (concat "__concat" builtin-concat)))

;;;;; MODULES

;; A metatable for tables backed by modules
(define module-metatable (make-table))

(hash-set!
 (table-slots module-metatable) "__index"
 (lambda (table key)
   (let ((slots (table-slots table)))
     (if (hash-ref slots key)
         (hash-ref slots key)
         (let ((key (string->symbol key))
               (module (hash-ref slots 'module)))
           (if (not (module-defined? module key))
               'nil
               (module-ref module key #f)))))))

(define (make-module-table name)
  (define table (make-table))
  (table-metatable! table module-metatable)
  (hash-set! (table-slots table) 'module (resolve-module name))
  table)

;;;;; GLOBAL ENVIRONMENT

(define *global-env-table* (make-table))

;; Saves _G and returns a function that will restore it
(define (save-fenv table)
  "Saves *global-env-table* and returns a function to restore it"
  (let* ((save *global-env-table*))
    (set! *global-env-table* table)
    (lambda ()
      (set! *global-env-table* save))))

(define (check-global-function name value)
  (when (eqv? value 'nil)
    (runtime-error "attempt to call global '~a' (a nil value)" name)))

;;;;; BUILT-INS

(define-syntax define-global
  (syntax-rules (do-not-export)
    ((_ (do-not-export name) value)
     (begin
       (define name value)
       (new-index! *global-env-table* (symbol->string 'name) name)))
    ((_ (name . rest) body ...)
     (define-global name (lambda rest body ...)))
    ((_ name value)
     (begin
       (define name value)
       (export name)
       (new-index! *global-env-table* (symbol->string 'name) name)))))

(define-global (assert v . opts)
  (if v
      (apply values v opts)
      (runtime-error "~a" (if (null? opts) "assertion failed" (car opts)))))

;; NOTE: collectgarbage cannot be fully implemented because it expects
;; an incremental garbage collector that matches lua's interface
(define-global collectgarbage
  (lambda* (opt #:optional (arg 'nil))
    (define (ignore) (runtime-warning "collectgarbage cannot respect command ~a" opt))
    (assert-type 1 "collectgarbage" "string" opt string?)
    (cond ((string=? opt "stop") (ignore))
          ((string=? opt "restart") (ignore))
          ((string=? opt "collect") (gc))
          ((string=? opt "count") (ignore))
          ((string=? opt "step") (ignore))
          ((string=? opt "setpause") (ignore))
          ((string=? opt "setstepmul") (ignore))
          (else (runtime-error "bad argument #1 to 'collectgarbage' (invalid option ~a)" opt)))))

(define run-lua (make-compiler 'lua))
(define run-lua-file (make-file-compiler 'lua))

(define-global (dofile filename)
  (assert-string 1 "dofile" filename)
  (run-lua-file filename))

(define-global (do-not-export error)
  (lambda* (message #:optional level)
    (runtime-warning "level argument to error is not respected")
    (throw 'lua-error message)))

;; global variable table
(define-global _G *global-env-table*)

(define-global (getmetatable table)
  (assert-table 1 "getmetatable" table)
  (let* ((mt (table-metatable table)))
    (if (eq? mt #f)
        'nil
        mt)))

(define-global (ipairs table)
  (assert-table 1 "ipairs" table)
  (values
    (lambda (table indice)
      (set! indice (+ indice 1))
      (let* ((value (index table indice)))
        (if (eqv? value 'nil)
            (values 'nil 'nil)
            (values indice value))))
    table
    0))

(define (load-chunkname-warning chunkname)
  (when chunkname
    (runtime-warning "load and loadstring ignore chunkname")))

(define-global load
  (lambda* (func #:optional chunkname)
    (load-chunkname-warning chunkname)
    (lambda ()
      (run-lua
       ((@ (language lua parser) read-lua)
        (open-input-string
         (let lp ((tree '())
                  (result (func)))
           (if (or (equal? func "") (eqv? func 'nil) (eq? func *unspecified*))
               (string-concatenate-reverse tree)
               (lp (cons func tree) (func))))))))))

(define-global loadfile
  (lambda* (#:optional filename)
    (lambda ()
      (run-lua
       (if filename
           (call-with-input-file filename
             (lambda (file)
               ((@ (language lua parser) read-lua) file)))
           ((@ (language lua parser) read-lua) (current-input-port)))))))

(define-global loadstring
  (lambda* (string #:optional chunkname)
    (load-chunkname-warning chunkname)
    (lambda ()
      (run-lua (call-with-input-string
                string
                (@ (language lua parser) read-lua))))))

;; TODO: module

(define-global next
  (lambda* (table #:optional (index 'nil))
    (assert-table 1 "next" table)
    (let* ((keys (hash-keys (table-slots table))))
      ;; empty table = nil
      (if (null? keys)
          'nil
          (begin
            (if (eqv? index 'nil)
                (let ((next-index (list-ref keys 0)))
                  (values next-index (rawget table next-index)))
                (let ((key-ref
                       (1+ (list-index (lambda (x) (equal? x index)) keys))))
                  (if (>= key-ref (length keys))
                      (values 'nil 'nil)
                      (let* ((next-index (list-ref keys key-ref)))
                        (values next-index (rawget table next-index)))))))))))

(define-global pairs
  (lambda* (table)
    (values next table 'nil)))

(define-global (pcall function . arguments)
  (catch #t
    (lambda () (apply function arguments))
    (lambda args (apply values (cons #f args)))))

(define-global (print . arguments)
  (for-each
   (lambda (x)
     (display (tostring x))
     (write-char #\tab))
   arguments)
  (newline)
  'nil)

(define-global (rawequal v1 v2)
  (equal? v1 v2))

(define-global (rawget table key)
  (assert-table 1 "rawget" table)
  (hash-ref (table-slots table) key))

(define-global (rawset table key value)
  (assert-table 1 "rawset" table)
  (hasht-set! (table-slots table) key value))

(define-global (select index . rest)
  (define rest-length (length rest))
  (cond
   ((number? index)
    (let lp ((vals '())
             (i index))
      (if (> i rest-length)
          (apply values (reverse! vals))
          (lp (cons (list-ref rest (- i 1)) vals) (+ i 1)))))
   (else rest-length)))

(define-global (setmetatable table metatable)
  (assert-table 1 "setmetatable" table)
  (assert-type 2 "setmetatable" "nil or table" metatable
               (lambda (x) (or (table? x) (eqv? x 'nil))))
  (table-metatable! table (if (eqv? metatable 'nil) #f metatable))
  table)

;; NOTE: built-in 'tonumber' is implemented on string->number and may
;; not have the same semantics as lua's tonumber; it should be based on the lexer
(define-global tonumber
  (lambda* (e #:optional (base 10))
    (cond
     ((number? e) e)
     ((string? e)
      (unless (memv base '(2 8 10 16))
        (runtime-warning
         "tonumber cannot respect bases other than 2, 8, 10, and 16"))
      (string->number e base))
     (else 'nil))))

(define-global (tostring e)
  (cond
   ((string? e) e)
   ((eqv? e #t) "true")
   ((eqv? e #f) "false")
   ((eqv? e 'nil) "nil")
   ((number? e) (number->string e))
   ((might-have-metatable? e)
    (dispatch-metatable-event
     "__tostring"
     (lambda (table) (format #f "~A" e))
     e
     e))
   (else (runtime-error "tostring cannot convert value ~A" e))))

(define-global (type v)
  (value-type->string v))

(define-global unpack
  (lambda* (array #:optional (i 1) j)
    (assert-table 1 "unpack" array)
    (unless j (set! j (table-length array)))
    (apply values (reverse!
     (let lp ((ls '())
             (i i))
       (if (> i j)
           ls
           (if (eqv? 'nil (index array i))
               ls
               (lp (cons (index array i) ls) (+ i 1)))))))))

;; _VERSION
;; contains a string describing the lua version
(define-global _VERSION "Guile/Lua 5.1")

(define-global (xpcall f err)
  (catch #t
    (lambda () (values #t (f)))
    (lambda args (values #f (err args)))))

;;; MODULE SYSTEM

;; package
(define-global package (make-table))

;; FIXME: this has no meaning if Guile does not support the Lua C
;; API. Also, we would need some other way to specify the path.
;;
;; package.cpath
#;
(new-index! package "cpath"
            (or (get-environment-variable "LUA_CPATH")
                "./?.so;/usr/lib/lua/5.1/?.so;/usr/lib/lua/5.1/loadall.so"))

;; package.loaded
(define loaded (make-table))
(new-index! package "loaded" loaded)

;; package.loaders
(define loaders (make-table))
(new-index! package "loaders" loaders)

;; package.loadlib
(new-index! package "loadlib"
            (lambda (lib func . _) (runtime-error "loadlib not implemented")))

;; FIXME: Like cpath, this has no meaning if Guile does not support the
;; Lua C API, and we would need some other way to specify the path.
;;
;; package.path
#;
(new-index! package "path"
            (or (get-environment-variable "LUA_PATH")
                "./?.lua;/usr/share/lua/5.1/?.lua;/usr/share/lua/5.1/?/init.lua;/usr/lib/lua/5.1/?.lua;/usr/lib/lua/5.1/?/init.lua"))

;; package.preload
(define preload (make-table))
(new-index! package "preload" preload)

;; package.seeall
(new-index! package "seeall"
            (lambda (module . _) (runtime-error "seeall unimplemented")))

;; arg
;; command line argument table
(define arg (make-table))
(let lp ((rest (command-line))
         (i 0))
  (new-index! arg i (car rest))
  (if (not (null? (cdr rest)))
      (lp (cdr rest) (1+ i))))

;; require
(define (register-loaded-module name table)
  (rawset *global-env-table* name table)
  (rawset loaded name table))

(define (module-exists? name)
  (if (module-public-interface (resolve-module name))
      #t
      #f))

(define-global (require module-name . _)
  (assert-type 1 "require" "string" module-name string?)
  ;; try to load module, if it's not already loaded
  (if (not (hash-ref (table-slots loaded) module-name))
      (let* ((std-module-name
              `(language lua standard ,(string->symbol module-name))))
        (if (module-exists? std-module-name)
            (register-loaded-module
             module-name
             (make-module-table std-module-name)))))

  (if (not (hash-ref (table-slots loaded) module-name))
      (runtime-error "require failed"))
  (rawget loaded module-name))
