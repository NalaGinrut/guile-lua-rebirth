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

(define-module (language lua lexer)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (system base lalr)
  #:use-module (language lua utils)
  #:export (make-lua-tokenizer
            debug-lua-tokenizer))

;; Character predicates

;; Lua only accepts ASCII characters as of 5.2, so we define our own
;; charsets here
(define (char-predicate string)
  (let ((char-set (string->char-set string)))
    (lambda (c)
      (and (not (eof-object? c)) (char-set-contains? char-set c)))))

(define is-digit? (char-predicate "0123456789"))
(define is-id-head? (char-predicate "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"))
(define (valid-id? c) (or (is-id-head? c) (is-digit? c)))
(define (is-newline? c) (and (char? c) (or (char=? c #\newline) (char=? c #\cr))))
(define *delimiters* " \t\n()[]{};+-/*%^~=<>\".,")
(define *operation-sign* "+-*/%^=~<>randot#")

(define *arith-op*
  '(("+" . add)
    ("-" . minus)
    ("*" . multi)
    ("/" . div)
    ("%" . mod)
    ("^" . expt)))
    
(define *relational-op*
  '(("==" . eq)
    ("~=" . neq)
    ("<"  . lt)
    (">"  . gt)
    ("<=" . leq)
    (">=" . geq)))

(define *logical-op*
  '(("or"  . or)
    ("and" . and)
    ("not" . not)))

(define *misc-op*
  '(#;(".." . concat) ; don't need it here
    ("#" . hash)
    ("=" . assign)))

;; NOTE: dots will have speical process
;;       so it doesn't appear in both punctuations and operations
(define *punctuations*
  '((";" . semi-colon)
    ("," . comma)
    (":" . colon)
    ;;("." . dot) ; don't need it here
    ("{" . lbrace)
    ("}" . rbrace)
    ("(" . lparen)
    (")" . rparen)
    ("[" . lbracket)
    ("]" . rbracket)))

(define-syntax-rule (punc->symbol c)
  (assoc-ref *punctuations* (string c)))

(define-syntax-rule (is-puctuation? c)
  (punc->symbol c))

(define (is-delimiter? c)
  (or (eof-object? c)
      (group-checker *delimiters* c)))

(define is-whitespace?
  (lambda (c)
    (and (char? c) (char-set-contains? char-set:whitespace c))))

(define read-word
  (lambda (port)
    (read-delimited *delimiters* port 'peek)))

(define *reserved-words*
  '(return function end if then elseif else true false nil or and
    do while repeat until local for break in not))

(define (is-reserved-word? str)
  (and=> (memq (string->symbol str) *reserved-words*) car))

(define *all-op* (append *arith-op* *relational-op* *logical-op* *misc-op*))

(define (get-op-token lst)
  (and (not (null? lst))  
       (assoc-ref *all-op* (list->string (reverse lst)))))

(define (is-op? c port)
  (define (check port lst)
    (let lp((l lst) (r '()))
      (cond
       ((and (null? l) ; hit all chars 
	     (is-delimiter? (peek-char port))) ; next char is delimiter
	;; hit an op, return as symbol
	(string->symbol (list->string lst)))
       ((and (not (null? l)) (char? (peek-char port)) (char=? (peek-char port) (car l)))
        (lp (cdr l) (cons (read-char port) r)))
       (else
	;; missed op, give all the chars back 
        (for-each (lambda (x) (unget-char1 x port)) (reverse r))
        #f))))
  (cond
   ((group-checker *operation-sign* c)
    (case c
      ((#\o) (check port '(#\o #\r)))
      ((#\a) (check port '(#\a #\n #\d)))
      ((#\n) (check port '(#\n #\o #\t)))
      (else  ; now it must be an op anyway, maybe invalid, but impossible not
       (get-op port))))
   (else #f)))

(define (get-op port)
  (let lp((c (read-char port)) (op '()))
    (cond
     ((not (group-checker *operation-sign* (peek-char port))) ; if op is end
      (cond
       ((get-op-token (cons c op)) ; and if it's an valid op
        => identity)
       (else
        ;; missed op, give back all the chars
        (unget-char1 c port)
        (for-each (lambda (x) (unget-char1 x port)) (reverse op))
        #f)))
     (else 
      (if (group-checker *operation-sign* (peek-char port)) ; next maybe part of op
	  (lp (read-char port) (cons c op)) ; read next
	  (lex-error "invalid op" (port-source-location port)
		     (list->string (reverse (cons c op)))))))))

(define (get-main-number port)
  (let ((num (read-word port)))
    (cond
     ((eqv? #\. (peek-char port)) ; not an integer
      (read-char port)
      (string->number (string-append num "." (read-delimited "e " port 'peek))))
     (else (string->number num))))) ; return integer

(define (get-exponent-number port sign)
  (let ((e (string->number (read-word port))))
    (if (integer? e)
        (case sign
          ((#\-) (- e))
          ((#\+) e)
          (else (error "wrong sign" sign)))
        (lex-error "Invalid exponent" (port-source-location port) e))))

;; Lua only has float number, so we convert it to inexact
(define (compose-number main exponent)
  (exact->inexact (* main (expt 10 exponent))))

(define (read-lua-number port)
  (let* ((main (get-main-number port))
         (exponent (cond
                    ((or (member (peek-char port) '(#\e #\E)))
                     (read-char port)
                     (cond
                      ((member (peek-char port) '(#\- #\+)) ; expt have sign
                       (get-exponent-number port (read-char port))) 
                      ((is-digit? (peek-char port))
                       (get-exponent-number port #\+)) ; default is positive
                      ((is-delimiter? (peek-char port))
                       0) ; no exponent
                      (else (lex-error "Invalid exponent number!" 
                                       (port-source-location port) #f))))
                    (else #f)))) ; don't have exponent
    (if exponent
        (compose-number main exponent)
        main)))

(define (read-lua-string port)
  (let* ((c (read-char port)) ; first #\" or #\'
         (str (read-delimited (string c) port 'peek)))
    (if (eof-object? (read-char port))
        (lex-error "String must be ended with \" or '" 
                   (port-source-location port) #f)
        str))) ; return the string

(define (skip-block-comment port)
  (let ((c0 (read-char port)) ; first #\[
        (c1 (read-char port))) ; second #\[
    (cond
     ((eof-object? c1)
      (lex-error "Wrong comment format encountered EOF!" 
                 (port-source-location port) #f))
     ((char=? c1 #\[)
      (read-delimited "]" port) ; read till first #\]
      (let ((c (read-char port)))
        (cond
         ((eof-object? c)
          (lex-error "Wrong comment format encountered EOF before second ']'" 
                     (port-source-location port) #f))
         ((char=? c #\]) ; second #\]
          #t) ; end block comment normally
         (else ; there's ']' in the comment but not the comment-end-sign ']]'
          (let lp((non (read-delimited "]" port)))
            (let ((e (read-char port)))
              (cond
               ((eof-object? e)
                (lex-error "Wrong comment encountered EOF before second ']'" 
                           (port-source-location port) #f))
               ((char=? e #\]) ; second #\]
                #t) ; end block comment normally
               (else (lp (read-delimited "]" port))))))))))
     (else ;; seems can't occur since it'd be a line-comment 
      (lex-error "No! This can't happen!" 
                 (port-source-location port) #f)))))
   
(define already-in-the-comment #f)

(define (skip-lua-comment port)
  (let ((c (peek-char port)))
    (cond 
     ((eof-object? c)
      (lex-error "Invalid comment header, encountered EOF"
                 (port-source-location port) #f))
     ((char=? c #\[) ; block comment
      (skip-block-comment port))
     ((char=? c #\-) ; code-within comment
      (read-line port) ; skip the first line
      (set! already-in-the-comment #t))
     ((and already-in-the-comment (char=? c #\@))
      (read-line port)) ; comment in comment
     (else (read-line port))))) ; skip line comment

;; As Lua specification, underscore follows an UPPERCASE char is a special-id
(define (is-special-id? id)
  (and (> (string-length id) 1)
       (char=? (string-ref id 0) #\_)
       (char-upper-case? (string-ref id 1))))

(define (is-valid-id? id)
  (not (string-any (lambda (c) (and (not (valid-id? c)) c)) id)))

(define (read-lua-identifier port)
  (let ((id (read-word port)))
    (cond
     ((is-reserved-word? id)
      => (lambda (res)
           (values res #f)))
     ((is-special-id? id) ; special id
      (if (is-valid-id? id)
          (values 'sp-id id)
          (lex-error "Invalid special id:" (port-source-location port) id))) 
     (else
      (if (is-valid-id? id) ; normal id
          (values 'id id)
          (lex-error "Invalid id:" (port-source-location port) id))))))

(define (next-token port)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) '*eoi*)
     ((is-whitespace? c)
      (read-char port)
      (next-token port))
     ((is-puctuation? c)
      => (lambda (punc)
           (read-char port)
           (return port punc #f)))
     ((is-digit? c) 
      (let ((num (read-lua-number port)))
        (return port 'number num))) ; it's number
     ((member c '(#\" #\'))
      (let ((str (read-lua-string port)))
        (return port 'string str))) ; it's string
     ((eqv? c #\.) ; check if . or .. or ...
      (read-char port)
      (let ((d (peek-char port)))
        (cond
         ((eqv? d #\.)
          (read-char port)
          (cond
           ((eqv? (peek-char port) #\.)
            (read-char port)
            (return port 'tri-dots #f)) ; tri-dot ...
           ;; FIXME: should we consider to avoid four dots here?
           (else (return port 'concat #f)))) ; concat ..
         (else (return port 'dot #f))))) ; dot .
     ((is-op? c port)
      => (lambda (op)
	   (return port op #f)))
     ((char=? c #\-)
      (read-char port) ; skip #\-
      (cond
       ((char=? (peek-char port) #\-)
        (read-char port) ; skip the second #\-
        (skip-lua-comment port)
        (next-token port)) ; -- is comment
       ((is-digit? (peek-char port))
        (- (read-lua-number port))) ; it's negtive number
       (lex-error "unexpected symbol near '-'" (port-source-location port) #f)))
     ((member c '(#\] #\@)) ; maybe comment
      (read-char port) ; skip #\] or #\@
      (cond
       (already-in-the-comment ; code-within comment or comment-in-comment
        (cond
         ((eqv? (read-char port) #\])
          (set! already-in-the-comment #f) ; end comment
          (next-token port))
         ;; FIXME: "@" or "@ ", which is the line-comment?
         (else (read-line port)))) ; '@' within comment is line-comment
       (else
        (unread-char c port) ; return back the char
        ;;(unread-char #\] port) ; don't need it
        (return port (punc->symbol c) #f)))) ; return the punc as token
    (else
     (cond
      ((eof-object? c) '*eoi*)
      ((is-id-head? c)
       (receive (cat val)
           (read-lua-identifier port)
         (return port cat val)))
      (else (lex-error "Invalid token!" (port-source-location port) c)))))))

(define lua-tokenizer
  (lambda (port)
    (let lp ((out '()))
      (let ((tok (next-token port)))
        (if (eq? tok '*eoi*)
            (reverse! out)
            (lp (cons tok out)))))))

(define (make-lua-tokenizer port)
 (let ((eoi? #f)
       (stack (new-stack)))
   (lambda ()
     (if eoi?
         '*eoi*
         (let ((tok (next-token port)))
           (case (if (lexical-token? tok) (lexical-token-category tok) tok)
             ((lparen)
              (stack-push! stack tok)) ; ready to check parens
             ((rparen) ; rparen fit
              (if (and (not (stack-empty? stack))
                       (eq? (lexical-token-category (stack-top stack)) 'lparen))
                  (stack-pop! stack)
                  (lex-error "unexpected right parenthesis"
                                (lexical-token-source tok)
                                #f)))
             ((lbracket)
              (stack-push! stack tok)) ; ready to check brackets
             ((rbracket) ; rbracket fit
              (if (and (pair? stack)
                       (eq? (lexical-token-category (car stack)) 'lbracket))
                  (stack-pop! stack)
                  (lex-error "unexpected right bracket"
                                (lexical-token-source tok)
                                #f)))
             ((lbrace)
              (stack-push! stack tok)) ; ready to check braces
             ((rbrace) ; rbrace fit
              (if (and (not (stack-empty? stack))
                       (eq? (lexical-token-category (stack-top stack)) 'lbrace))
                  (set! stack (cdr stack))
                  (lex-error "unexpected right brace"
                                (lexical-token-source tok)
                                #f)))
             ;; NOTE: this checker promised the last semi-colon before eof will 
             ;;       return '*eoi* directly, or we have to press EOF (C-d) to 
             ;;       end the input.
             ;;       BUT I WONDER IF THERE'S A BETTER WAY FOR THIS!
             ((semi-colon)
              ;; FIXME:
              ;; Lua doesn't need semi-colon as statment-ending
              (set! eoi? (stack-empty? stack))))
           tok)))))

(define (debug-lua-tokenizer src)
  ((make-token-checker lua-tokenizer) src))
