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
   (left: or) 
   (left: and) 
   (left: less-than larger-than less-eq larger-eq neq eq)
   (right: concat)
   (left: add minus)
   (left: multi div mod)
   (right: not hash)
   (right: expt)

   ;; punctuations
   semi-colon comma dot lbrace rbrace lparen rparen lbracket rbracket
   colon

   ;; reserved word
   return function end if then elseif else true false nil or and
   do while repeat until local for break in not

   ;; misc
   id sp-id)

  (program: (block) : $1
            (*eoi*) : *eof-object*)
  
  (terminator (semi-colon) : $1
              () : '(begin))

  (block (scope stmt-list) : $x
         (scope stmt-list last-stmt terminator) : $x)

  (ublock (block until exp) : $x)

  (scope () : '(begin)
         (scope stmt-list binding terminator) : $x)

  (stmt-list () : '(begin)
             (scope stmt-list stmt terminator) :  $x)
    
  (stmt (do block end) : $x
        (while exp do block end) : $x
        (repetition do block end) : $x
        (repeat ublock) : $x
        (if conds end) : $x
        (function funcname funcbody) : $x
        (set-list eq exp-list1) : $x)

  (repetition (for name eq exp-list23) : $x
              (for name-list in exp-list) : $x)

  (conds (cond-list) : $x
         (cond-list else block) : $x)
  
  (cond-list (cond) : $x
             (cond-list elseif cond) : $x)

  (cond (exp then block) : $x)

  (last-stmt (break) : $x
             (return) : $x
             (return exp-list1) : $x)

  (binding (local name-list) : $x
           (local name-list eq exp-list1) : $x
           (local function name funcbody) : $x)

  (func-name (dotted-name) : $1
             (dotted-name colon name) : $x)

  (dotted-name (name) : $1
               (dotted-name dot name) : $x)

  (name-list (name) : $1
             (name-list dot name) : $x)
  
  (exp-list1 (exp) : $1
             (exp-list1 comma exp) : $x)

  (exp-list23 (exp comma exp) : $x
              (exp comma exp comma exp) : $x)

  (exp (nil) : $1
       (true) : $1
       (false) : $1
       (number) : $1
       (string) : $1
       (tri-dots) : $1 ; how to deal with var-list?
       (func) : $1
       (prefix-exp) : $1
       (table-constructor) : $1
       (not) : $1
       (hash) : $1
       (minus) : $1
       (exp or exp) : $x
       (exp and exp) : $x
       (exp arith-compare exp) : $x
       (exp concat exp) : $x
       (exp arith-op exp) : $x)

  (arith-compare (less-than) : $1
                 (less-eq) : $1
                 (larger-than) : $1
                 (larger-eq) : $1
                 (eq) : $1
                 (neq) : $1)

  (arith-op (add) : $1
            (minus) : $1
            (multi) : $1
            (div) : $1
            (mod) : $1
            (expt) : $1)

  (tri-dots (dot dot dot) : args)

  (set-list (var) : $1
            (set-list comma var) : $x)

  (var (name) : $1
       (prefix-exp lbracket exp rbracket) : $x
       (prefix-exp dot name) : $x)

  (function-call (prefix-exp args) : $x
                 (prefix-exp colon name args) : $x)

  (args (lparen rparen) : $x
        (lparen exp-list1 rparen) : $x
        (table-constructor) : $1
        (string) : $1)

  (func (function funcbody) : $x)

  (funcbody (params block end) : $x)
  
  (params (lparen par-list rparen) : $x)

  (par-list () : $x
            (name-list) : $x
            (tri-dots) : $x
            (name-list comma tri-dots) : $x)

  (table-constructor (lbrace rbrace) : $x
                     (lbrace field-list rbrace) : $x
                     (lbrace field-list comma rbrace) : $x
                     (lbrace field-list semi-colon rbrace) : $x)

  (field-list (field) : $1
              (field-list comma field) : $x
              (field-list semi-colon field) : $x)

  (field (exp) : $x
         (name eq exp) : $x
         (lbracket exp rbracket eq exp) : $x)) 
