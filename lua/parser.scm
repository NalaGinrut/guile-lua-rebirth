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
  #:use-module (language lua runtime)
  #:export (make-parser read-lua))

(define (read-lua port)
  (make-reader make-parser make-lua-tokenizer port))

(define (make-parser)
  (lalr-parser
   ;; according to operations precedence
   or 
   and 
   less-than larger-than less-eq larger-eq neq eq
   concat
   add minus
   multi div mod
   not hash
   expt

   ;; punctuations
   semi-colon comma dot lbrace rbrace lparen rparen lbracket rbracket

   ;; reserved word
   return function end if then elseif else true false nil or and
   do while repeat until local for break in not

   )

;; Lua BNF  
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

              
)
