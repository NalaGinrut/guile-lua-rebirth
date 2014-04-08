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
   
   (program: (chunk) : $1
             (*eoi*) : *eof-object*)
   
   (chunk (block) : $1)

   (terminator (semi-colon) : 'semi-colon) ; FIXME: accept semi-colon

   (stmt (chunk terminator) : `(begin $1))

   (block (exp) : $1)

   (name (id) : `(id ,$1))

   (boolean (nil) : '(boolean nil)
            (true) : '(boolean true)
            (false) : '(boolean false))

   (var (number) : `(number ,$1)
        (string) : `(string ,$1)
        (boolean) : $1
        (name) : $1)

   (exp (exps) : $1
        (var) : $1)

   (exps (logic-exps) : $1
         (string-exps) : $1
         (arith-exps) : $1
         (misc-exps) : $1)

   ;; Lua Precedence:
   ;;     or
   ;;     and
   ;;     <     >     <=    >=    ~=    ==
   ;;     ..
   ;;     +     -
   ;;     *     /     %
   ;;     not   #     - (unary)
   ;;     ^

   ;; for logic and comparison
   (logic-exps (logic-exps logic-exp) : `(begin ,$1 ,$2)
               (logic-exp) : $1)
   (logic-exp (logic-exp or logic-term) : `(or ,$1 ,$3)
              (logic-exp and logic-term) : `(and ,$1 ,$3)
              (logic-term) : $1)
   (logic-term (logic-term gt logic-val) : `(gt ,$1 ,$3)
               (logic-term lt logic-val) : `(lt ,$1 ,$3)
               (logic-term leq logic-val) : `(leq ,$1 ,$3)
               (logic-term geq logic-val) : `(geq ,$1 ,$3)
               (logic-term neq logic-val) : `(neq ,$1 ,$3)
               (logic-term eq logic-val) : `(eq ,$1 ,$3)
               (logic-val) : $1
               (arith-exp) : $1)   
   (logic-val (lparen logic-exp rparen) : $2
              (var) : $1)

   ;; for string
   (string-exps (string concat string) : `(concat ,$1 ,$3)
                (string) : `(string ,$1)
                (exp) : $1)
            
   ;; for arithmatic
   (arith-exps (arith-exps arith-exp) : `(begin ,$1 ,$2)
               (arith-exp) : $1)
   (arith-exp  (arith-exp add arith-term) : `(add ,$1 ,$3)
               (arith-exp minus arith-term) : `(minus ,$1 ,$3)
               (arith-term) : $1
               (logic-exp) : $1)
   (arith-term (arith-term multi arith-factor) : `(multi ,$1 ,$3)
               (arith-term div arith-factor) : `(div ,$1 ,$3)
               (arith-factor) : $1)
   (arith-factor (lparen arith-exp rparen) : $2
                 (var) : $1)

   ;; for misc and unary operations
   (misc-exps (misc-exps misc-exp) : `(begin ,$1 ,$2)
              (misc-exp) : $1)
   (misc-exp (not misc-exp) : `(not ,$2)
             (hash misc-exp) : `(hash ,$2)
             ;; NOTE: There's no '+number' notation in Lua! 
             ;;       But '-number' is fine.
             (minus misc-exp) : `(minus ,$2)
             (misc-exp expt misc-exp) : `(expt ,$1 ,$3)
             (exp) : $1)
   ))
