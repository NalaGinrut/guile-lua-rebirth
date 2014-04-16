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
   
   (program (chunk) : $1
            (*eoi*) : *eof-object*)
   
   (chunk (block) : $1)

   (terminator () : '()
               (semi-colon) : '()) ; FIXME: accept semi-colon

   ;;(stmt (chunk terminator) : `(begin $1))

   (block (scope stat-list) : `(scope ,@$1 ,$2)
          (scope stat-list last-stat terminator) : `(scope ,@$1 ,$2 ,$3))

   (scope () : '()
          (scope stat-list binding terminator) : `(scope ,$1 ,$2))
          
   (stat-list (stat) : $1 ; FIXME: is this correct grammar?
              (stat-list stat terminator) : `(begin ,$1 ,$2))

   (stat (set-list assign exp-list1) : `(assign ,$1 ,$3)
         ;; NOTE: Lua grammar doesn't accept exp directly,
         ;;       you have to use assign or function on exp,
         ;;       say, a=1+2 or print(1+2), 
         ;;       1+2 will cause a syntax error.
         ;; FIXME:
         ;;       However, we need to test the parsing of exp easier,
         ;;       so we add this syntax. It's susppended whether
         ;;       we should keep it when GLR is mature.         
         (exp) : $1)

   (set-list (var) : $1
             ;; Multi values
             (set-list comma var) : `(values ,@$1 ,$3))

   (exp-list1 (exp) : $1
              (exp-list1 comma exp) : `(begin ,$1 ,$3))

   (last-stat () : '(begin))
   (binding () : '(begin))

   (name (id) : `(id ,$1))

   (boolean (true) : '(boolean true)
            (false) : '(boolean false))

   (var (number) : `(number ,$1)
        (string) : `(string ,$1)
        (boolean) : $1
        (nil) : '(marker nil)
        (name) : $1)

   (exp (exp comma exp) : `(begin ,$1 ,$3)
        (misc-exp) : $1
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
