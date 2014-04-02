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

(define-module (language lua parser)
  #:use-module (language lua utils)
  #:use-module (language lua lexer)
  #:use-module (system base lalr)
  #:export (make-parser read-lua))

(define (read-lua port)
  (make-reader make-parser make-lua-tokenizer port))

;; Lua 5.1 BNF in Lemon by RiciLake

;; chunk      ::= block .

;; semi       ::= ';' .
;; semi       ::= .

;; block      ::= scope statlist .
;; block      ::= scope statlist laststat semi .
;; ublock     ::= block 'until' exp .

;; scope      ::= .
;; scope      ::= scope statlist binding semi.
           
;; statlist   ::= .
;; statlist   ::= statlist stat semi .

;; stat       ::= 'do' block 'end' .
;; stat       ::= 'while' exp 'do' block 'end' .
;; stat       ::= repetition 'do' block 'end' .
;; stat       ::= 'repeat' ublock .
;; stat       ::= 'if' conds 'end' .
;; stat       ::= 'function' funcname funcbody .
;; stat       ::= setlist '=' explist1 .
;; stat       ::= functioncall .

;; repetition ::= 'for' NAME '=' explist23 .
;; repetition ::= 'for' namelist 'in' explist1 .
           
;; conds      ::= condlist .
;; conds      ::= condlist 'else' block .
;; condlist   ::= cond .
;; condlist   ::= condlist 'elseif' cond .
;; cond       ::= exp 'then' block .
           
;; laststat   ::= 'break' .
;; laststat   ::= 'return' .
;; laststat   ::= 'return' explist1 .

;; binding    ::= 'local' namelist .
;; binding    ::= 'local' namelist '=' explist1 .
;; binding    ::= 'local' 'function' NAME funcbody .

;; funcname   ::= dottedname .
;; funcname   ::= dottedname ':' NAME .

;; dottedname ::= NAME .
;; dottedname ::= dottedname '.' NAME .

;; namelist   ::= NAME .
;; namelist   ::= namelist ',' NAME .

;; explist1   ::= exp .
;; explist1   ::= explist1 ',' exp .
;; explist23  ::= exp ',' exp .
;; explist23  ::= exp ',' exp ',' exp .

;; %left      'or' .
;; %left      'and' .
;; %left      '<' '<=' '>' '>=' '==' '~=' .
;; %right     '..' .
;; %left      '+' '-' .
;; %left      '*' '/' '%' .
;; %right     'not' '#' .
;; %right     '^' .

;; exp        ::= 'nil'|'true'|'false'|NUMBER|STRING|'...' .
;; exp        ::= function .
;; exp        ::= prefixexp .
;; exp        ::= tableconstructor .
;; exp        ::= 'not'|'#'|'-' exp .         ['not']
;; exp        ::= exp 'or' exp .
;; exp        ::= exp 'and' exp .
;; exp        ::= exp '<'|'<='|'>'|'>='|'=='|'~=' exp .
;; exp        ::= exp '..' exp .
;; exp        ::= exp '+'|'-' exp .
;; exp        ::= exp '*'|'/'|'%' exp .
;; exp        ::= exp '^' exp .
           
;; setlist    ::= var .
;; setlist    ::= setlist ',' var .

;; var        ::= NAME .
;; var        ::= prefixexp '[' exp ']' .
;; var        ::= prefixexp '.' NAME .

;; prefixexp  ::= var .
;; prefixexp  ::= functioncall .
;; prefixexp  ::= OPEN exp ')' .

;; functioncall ::= prefixexp args .
;; functioncall ::= prefixexp ':' NAME args .

;; args        ::= '(' ')' .
;; args        ::= '(' explist1 ')' .
;; args        ::= tableconstructor .
;; args        ::= STRING .

;; function    ::= 'function' funcbody .

;; funcbody    ::= params block 'end' .

;; params      ::= '(' parlist ')' .

;; parlist     ::= .
;; parlist     ::= namelist .
;; parlist     ::= '...' .
;; parlist     ::= namelist ',' '...' .

;; tableconstructor ::= '{' '}' .
;; tableconstructor ::= '{' fieldlist '}' .
;; tableconstructor ::= '{' fieldlist ','|';' '}' .

;; fieldlist   ::= field .
;; fieldlist   ::= fieldlist ','|';' field .
            
;; field       ::= exp .
;; field       ::= NAME '=' exp .
;; field       ::= '[' exp ']' '=' exp .

(define (make-parser)
  (lalr-parser
   (;; punctuations
    semi-colon comma dot lbrace rbrace lparen rparen lbracket rbracket
    colon
    
    ;; reserved word
    return function end if then elseif else true false nil
    do while repeat until local for break in
    ;; or and not hash

    ;; type
    string number

    ;; misc
    id sp-id tri-dots

    ;; according to operations precedence
    (left: or) 
    (left: and) 
    (left: lt gt leq geq neq eq)
    (right: concat)
    (left: add minus)
    (left: multi div mod)
    (right: not hash)
    (right: expt)
    (right: assign))
   
   (program: (chunk) : $1
             (*eoi*) : *eof-object*)
   
   (chunk (block) : $1)
   
   (terminator (semi-colon) : $1)

   (block () : '(begin) 
          (stmt-list) : $1)

   (stmt-list (stmt) : $1
              (stmt stmt-list) : `(begin ,$1 ,$2)
              (stmt-list last-stmt terminator) : `(begin ,$1 ,$2))

   (last-stmt (break) : '(break)
              (return) : '(returned) ; is it proper to return *unspecified* 
              (return exp-list1) : `(returned ,$1))

   (stmt (do block end) : `(do ,$2)
         (while exp do block end) : `(while ,$2 do ,$4)
         (repetition) : $1
         (repeat block until exp) : `(repeat ,$2 until ,$3)
         (if-stmt) : $1
         (function-stmt) : $1
         (set-list assign exp-list1) : `(global-assign ,$1 ,$3)
         (local-binding) : $1)

   (repetition (for name assign exp-list23 do block end) 
               : `(for ,$2 ,$4 do ,$6)
               (for name-list in exp-list1 do block end) 
               : `(for ,$2 in ,$3 do ,$6))

   (if-stmt (if exp then block) : `(if ,$2 then ,$3)
            (if exp else block) : `(if ,$2 else ,$3)
            (if exp then block else block) : `(if ,$2 then ,$3 else ,$4)
            (if exp then block elseif-stmts) : `(if ,$2 then ,$3 ,$4))
   
   (elseif-stmts (elseif-stmt) : $1
                 (elseif-stmt elseif-stmts) : `(begin ,$1 ,$2))

   (elseif-stmt (elseif exp then block) : `(if ,$2 then ,$3)
                (elseif exp then block else block) : `(if ,$2 then ,$3 else ,$4))

   (local-binding (local name-list) : '(begin) ; nothing to do 
                  (local name-list assign exp-list1) : `(local-assign ,$3 ,$4)
                  (local function name funcbody) : `(local function ,$3 ,$4))

   (func-name (dotted-name) : $1
              (dotted-name colon name) : `(func-self-pass-in ,$1 ,$3))

   (dotted-name (name) : $1
                (dotted-name dot name) : `(func-dot-name ,$1 ,$3))

   (name-list (name) : $1
              (name-list dot name) : `(dot-name ,$1 ,$2))

   (name (id) : $1
         (sp-id) : $1)

   (exp-list1 (exp) : $1
              (exp-list1 comma exp) : `(begin ,$1 ,$3))

   (exp-list23 (exp comma exp) : `(begin ,$1 ,$3)
               (exp comma exp comma exp) : `(begin ,$1 ,$3 ,$5))

   (exp (nil) : 'nil
        (true) : 'true
        (false) : 'false ; only nil and false are FALSE
        (numbers) : $1
        (string) : `(string ,$1)
        (tri-dots) : $1
        ;;(function) : $1
        (prefix-exp) : $1
        (table-constructor) : $1
        (not exp) : `(not ,$2)
        (hash prefix-exp) : `(hash ,$2)
        (exp or exp) : `(or ,$1 ,$3)
        (exp and exp) : `(and ,$1 ,$3)
        (exp arith-compare exp) : `(,$2 ,$1 ,$3)
        (exp concat exp) : `(concat ,$1 ,$3)
        (exp arith-op exp) : `(,$2 ,$1 ,$3))

   (numbers (number) : `(number ,$1)
            (minus number) : `(- ,$1)
            (add number) : `(+ ,$1))
   
   (arith-compare (lt) : $1
                  (leq) : $1
                  (gt) : $1
                  (geq) : $1
                  (eq) : $1
                  (neq) : $1)

   (arith-op (add) : $1
             (minus) : $1
             (multi) : $1
             (div) : $1
             (mod) : $1
             (expt) : $1)

   (set-list (var) : $1
             (set-list comma var) : `(,@$1 ,$3))

   (var (name) : `(variable ,$1)
        (prefix-exp lbracket exp rbracket) : `(table-ref ,$1 ,$2)
        (prefix-exp dot name) : `(dot-name ,$1 ,$3))

   (prefix-exp (var) : $1
               (function-call) : $1
               (lparen exp rparen) : $2)

   (function-call (prefix-exp args) : `(func-call ,$1 ,$2)
                  (prefix-exp colon name args) : `(func-call ,$1 ,$3 ,$4))

   (args (lparen rparen) : '(void-args)
         (lparen exp-list1 rparen) : $2
         (table-constructor) : `(args-with-table ,$1) ; just pass the table as arg
         (string) : `(string ,$1))

   (function-stmt (function funcname funcbody) : `(function ,$2 ,$3)
                  (function funcbody) : `(function ,$2))

   (funcname (dottedname) : $1
             (dottedname colon name) : $1)

   (dottedname (name) : $1
               (dottedname dot name) : `(ref-from ,$1 ,$3))

   (funcbody (params block end) : `(funcbody ,$1 ,$2))
   
   (params (lparen par-list rparen) : $2)

   (par-list () : '(void-par-list)
             (name-list) : $1
             (tri-dots) : '(void-varlist) ; WTF?!
             (name-list comma tri-dots) : `(varlist ,$1))

   (table-constructor (lbrace rbrace) : '(table)
                      (lbrace field-list rbrace) : `(table ,$2)
                      (lbrace field-list comma rbrace) : `(table ,$2)
                      (lbrace field-list semi-colon rbrace) : `(table ,$2))

   (field-list (field) : $1
               (field-list comma field) : `(field-list ,@$1 ,$3)
               (field-list semi-colon field) : `(field-list ,@$1 ,$3))

   ;; NOTE: is dot-name the same with table-bracket-ref
   (field (exp) : $1
          (name assign exp) : `(field-assign ,$1 ,$3)
          (lbracket exp rbracket assign exp) : `(field-bracket-assign ,$2 ,$5))))
