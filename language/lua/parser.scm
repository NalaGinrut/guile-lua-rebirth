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

(define-module (language lua parser)
  #:use-module (language lua utils)
  #:use-module (language lua lexer)
  #:use-module (system base lalr)
  #:export (make-parser read-lua))

(define (read-lua port)
  (make-reader make-parser make-lua-tokenizer port))

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

   ;; The *unit-of-compilation* of Lua is called a chunk.
   ;; Syntactically, a chunk is simply a block:   
   (chunk (block) : $1
          (*eoi*) : *eof-object*)
   
   (terminator () : '()
               (semi-colon) : '()) ; FIXME: accept semi-colon

   ;; A block is a list of statements, which are executed sequentially
   (block (scope stat-list) : `(scope ,@$1 ,$2)
          (scope stat-list last-stat terminator) : `(scope ,@$1 ,@$2 ,$3))

   (scope () : '()
          (scope stat-list binding terminator) : `(scope ,$1 ,@$2))
          
   (stat-list (stat) : $1 ; FIXME: is this correct grammar?
              (stat-list stat terminator) : `(begin ,$1 ,$2))

   (stat ;; A block can be explicitly delimited to produce a single statement:
         (do block end) : $2
         (while exp do block end) : `(while ,$1 do ,$4)
         (repeatition do block end) : `(rep ,$1 ,$3)
         (if conds end) : `(if ,@$2)
         (var-list assign exp-list) : `(assign ,$1 ,$3)
         ;; NOTE: Lua grammar doesn't accept exp directly,
         ;;       you have to use assign or function on exp,
         ;;       say, a=1+2 or print(1+2), 
         ;;       1+2 will cause a syntax error.
         ;; FIXME:
         ;;       However, we need to test the parsing of exp easier,
         ;;       so we add this syntax. It's susppended whether
         ;;       we should keep it when GLR is mature.         
         (exp) : $1)

   (repeatition (for name assign range) : `(assign ,$2 ,$4)
                (for name-list in exp-list) : `(assign ,$2 ,$4))

   (conds (cond-list) : $1
          (cond-list else block) : `(,@$1 else ,@$3))
   
   (cond-list (cond) : $1
              (cond-list elseif cond) : `(,@$1 elseif ,@$3))
   
   (cond (exp then block) : `(,$1 then ,$3))
             
   (var-list (var) : $1
             ;; Multi values binding
             ;; NOTE:
             ;; (FIXME: this note should be moved to compile-tree-il.scm)
             ;; ** Before the assignment, the list of values is adjusted
             ;;    to the length of the list of variables.
             ;; There're three cases for multi values:
             ;; 1. more values than needed, excess values are thrown away.
             ;; 2. fewer, the list is extended with as many nil's as needed.
             ;; 3. If the list of expressions ends with a function call,
             ;;    then all values returned by that call enter the list
             ;;    of values, before the adjustment
             ;;    (except when the call is enclosed in parentheses).
             (var-list comma var) : `(mul-vals ,$1 ,$3))
   
   (exp-list (exp) : $1
             ;; Multi values returning
             (exp-list comma exp) : `(multi-exps ,$1 ,$3))

   (range (exp comma exp) : `(range ,$1 ,$3)
          (exp comma exp comma exp) : `(range ,$1 ,$3 ,$5))

   (last-stat () : '())
   (binding () : '())

   (name-list (name) : $1
              ;; FIXME: Shouldn't it be 'multi-vals ?
              (dotted-name comma name) : `(multi-names ,$1 ,$3))

   (dotted-name (name) : $1
                (dotted-name dot name) : '(not-yet))

   ;; Variables are places that store values. 
   ;; There are three kinds of variables in Lua:
   ;; global variables, local variables, and table fields.
   (var (number) : `(number ,$1)
        (string) : `(string ,$1)
        (boolean) : $1
        (nil) : '(marker nil)
        (name) : $1)

   (name (id) : `(id ,$1))

   (boolean (true) : '(boolean true)
            (false) : '(boolean false))

   (exp (misc-exp) : $1
        (arith-exp) : $1
        (string-exp) : $1
        (logic-exp) : $1
        (var) : $1)
   
   ;; Lua Precedence(from higher to lower):
   ;;              ^
   ;;          not  - (unary)
   ;;          *   /
   ;;          +   -p
   ;;          ..
   ;;          <   >   <=  >=  ~=  ==
   ;;          and
   ;;          or

   ;; for logic and comparison
   (logic-exp (logic-exp or logic-exp) : `(or ,$1 ,$3)
              (logic-exp and logic-exp) : `(and ,$1 ,$3)
              (logic-term) : $1)
   (logic-term (logic-term gt logic-term) : `(gt ,$1 ,$3)
               (logic-term lt logic-term) : `(lt ,$1 ,$3)
               (logic-term leq logic-term) : `(leq ,$1 ,$3)
               (logic-term geq logic-term) : `(geq ,$1 ,$3)
               (logic-term neq logic-term) : `(neq ,$1 ,$3)
               (logic-term eq logic-term) : `(eq ,$1 ,$3)
               (logic-val) : $1)
   (logic-val (lparen logic-exp rparen) : $2
              (string-exp) : $1 
              (arith-exp) : $1
              (misc-exp) : $1
              (var) : $1)

   ;; for string
   (string-exp (string concat string) : `(concat ,$1 ,$3)
               (arith-exp) : $1
               (misc-exp) : $1
               (var) : $1)   ;; for misc and unary operations

   ;; for arithmatic
   (arith-exp  (arith-exp add arith-exp) : `(add ,$1 ,$3)
               (arith-exp minus arith-exp) : `(minus ,$1 ,$3)
               (arith-term) : $1)
   (arith-term (arith-term multi arith-term) : `(multi ,$1 ,$3)
               (arith-term div arith-term) : `(div ,$1 ,$3)
               (arith-factor) : $1)
   (arith-factor (lparen arith-exp rparen) : $2
                 (misc-exp) : $1
                 (var) : $1)

   (misc-exp (not misc-exp) : `(not ,$2)
             (hash misc-exp) : `(hash ,$2)
             ;; NOTE: There's no '+number' notation in Lua! 
             ;;       But '-number' is fine.
             (minus misc-exp) : `(minus ,$2)
             (misc-exp expt misc-exp) : `(expt ,$1 ,$3)
             (misc-val) : $1)
   (misc-val (lparen misc-exp rparen) : $2
             (var) : $1)
   ))
