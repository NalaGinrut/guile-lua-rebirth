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

;; Lua only accepts ASCII characters in 5.2
;; Define charsets for faster searching
(define (char-predicate string)
  (let ((cs (string->char-set string)))
    (lambda (c)
      (and (not (eof-object? c)) (char-set-contains? cs c)))))

(define is-digit? (char-predicate "0123456789"))
(define is-id-head? (char-predicate "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"))
(define (valid-id? c) (or (is-id-head? c) (is-digit? c)))
(define (is-newline? c) (and (char? c) (or (char=? c #\newline) (char=? c #\cr))))
(define *delimiters* " \t\n()[]{};+-/*%^~=<>\".,:")
(define is-op-sign0? (char-predicate "+-*/%^=~<>#"))
(define is-op-sign1? (char-predicate "randot"))
(define check-delimiter (char-predicate *delimiters*))

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

(define-syntax-rule (maybe-op-sign? c)
  (or (is-op-sign1? c) (is-op-sign0? c)))

;; BUG
;; FIXME: Should pass "1+ -1" and "1+-1"
(define-syntax-rule (maybe-op-stop? c last)
  (cond
   ;;((is-delimiter? c) #t)
   ((is-op-sign0? last)
    (not (is-op-sign0? c)))
   ((is-op-sign1? last)
    (not (is-op-sign1? c)))
   (else #t)))
  
(define-syntax-rule (punc->symbol c)
  (assoc-ref *punctuations* (string c)))

(define-syntax-rule (is-puctuation? c)
  (punc->symbol c))

(define (is-delimiter? c)
  (or (eof-object? c)
      (check-delimiter c)))

(define (is-whitespace? c)
  (and (char? c) (char-set-contains? char-set:whitespace c)))

(define (read-word port)
  (read-delimited *delimiters* port 'peek))

(define *reserved-words*
  '(return function end if then elseif else true false nil or and
    do while repeat until local for break in not))

(define (is-reserved-word? str)
  (and=> (memq (string->symbol str) *reserved-words*) car))

(define *all-op* (append *arith-op* *relational-op* *logical-op* *misc-op*))

(define (get-op-token lst)
  (and (not (null? lst))  
       (assoc-ref *all-op* (list->string (reverse lst)))))

(define last-token 'none)

(define (is-uminus? c port)
  (and (char=? c #\-)
       ;;(display last-token)(newline)
       (memq last-token '(punc op none))))

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
   ((maybe-op-sign? c)
    (cond
     ((char=? c #\o) (check port '(#\o #\r)))
     ((char=? c #\a) (check port '(#\a #\n #\d)))
     ((char=? c #\n) (check port '(#\n #\o #\t)))
     ((is-comment? c port)
      (read-char port) ; skip #\-
      (skip-lua-comment port)
      'comment) ; -- is start of comment
     ((is-uminus? c port)
      (read-char port) ; skip #\-
      'uminus)
     (else  ; now it must be an op anyway, maybe invalid, but impossible not
      (get-op port))))
   (else #f)))

(define (get-op port)
  (let lp((c (read-char port)) (op '()))
    (cond
     ((maybe-op-stop? (peek-char port) c) ; if op is end
      (cond
       ((get-op-token (cons c op)) ; and if it's an valid op
        => identity)
       (else
        ;; missed op, give back all the chars
        (unget-char1 c port)
        (for-each (lambda (x) (unget-char1 x port)) (reverse op))
        #f)))
     (else 
      (if (maybe-op-sign? (peek-char port)) ; next maybe part of op
	  (lp (read-char port) (cons c op)) ; read next
	  (lex-error "invalid op" (port-source-location port)
		     (list->string (reverse (cons c op)))))))))

(define (get-main-number port)
  (define (read-num port)
    (let lp((c (peek-char port)) (ret '()))
      (cond
       ((is-delimiter? c) ; normal number
	(list->string (reverse ret)))
       ((memv c '(#\e #\E)) ; exponent main number
	(list->string (reverse ret)))
       (else
	(read-char port)
	(lp (peek-char port) (cons c ret))))))
  (let ((num (read-num port)))
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

;; Lua only has float number, so we convert it to inexact number
(define (compose-number main exponent)
  (exact->inexact (* main (expt 10 exponent))))

(define (read-lua-number port)
  (let* ((main (get-main-number port))
         (exponent (cond
                    ((memv (peek-char port) '(#\e #\E))
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

(define (is-comment? c port)
  (cond
   ((char=? c #\-)
    (read-char port) ; skip #\-
    (or (char=? (peek-char port) #\-)
        (not (unread-char c port))))
   (else #f)))

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

;; As Lua specification, underscore follows one or more UPPERCASE is a special-id
;; Specifically, single underscore "_" is reserved as dummy variable
(define (is-special-id? id)
  (and (char=? (string-ref id 0) #\_)
       (or (= (string-length id) 1) ; single underscore
	   ;; or is it _X...
	   (char-upper-case? (string-ref id 1)))))	   

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
     ((eof-object? c)
      (set! last-token 'none)
      '*eoi*)
     ((is-whitespace? c)
      (read-char port)
      ;; NOTE: don't memorize whitespace to last-token!!!
      (next-token port))
     ((is-puctuation? c)
      => (lambda (punc)
           (read-char port)
           (set! last-token 'punc)
           (return port punc #f)))
     ((is-digit? c)
      (let ((num (read-lua-number port)))
        (set! last-token 'number)
        (return port 'number num))) ; it's number
     ((member c '(#\" #\'))
      (let ((str (read-lua-string port)))
        (set! last-token 'string)
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
            (set! last-token 'tri-dots)
            (return port 'tri-dots #f)) ; tri-dot ...
           ;; FIXME: should we consider to avoid four dots here?
           (else
            (set! last-token 'concat)
            (return port 'concat #f)))) ; concat ..
         (else (return port 'dot #f))))) ; dot .
     ((is-op? c port)
      => (lambda (op)
           (cond
            ((eq? op 'comment)
             (next-token port)) ; comment
            (else
             (set! last-token 'op)
             (return port op #f)))))
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
      ((is-id-head? c)
       (receive (cat val)
           (read-lua-identifier port)
         (set! last-token 'id)
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
              (if (and (not (stack-empty? stack))
                       (eq? (lexical-token-category (stack-top stack)) 'lbracket))
                  (stack-pop! stack)
                  (lex-error "unexpected right bracket"
			     (lexical-token-source tok)
			     #f)))
             ((lbrace)
              (stack-push! stack tok)) ; ready to check braces
             ((rbrace) ; rbrace fit
              (if (and (not (stack-empty? stack))
                       (eq? (lexical-token-category (stack-top stack)) 'lbrace))
                  (stack-pop! stack)
                  (lex-error "unexpected right brace"
			     (lexical-token-source tok)
			     #f)))
             ;; NOTE: this checker promised the last semi-colon before eof will 
             ;;       return '*eoi* directly, or we have to press EOF (C-d) to 
             ;;       end the input.
             ;;       BUT I WONDER IF THERE'S A BETTER WAY FOR THIS!
             ((semi-colon *eoi*)
              ;; FIXME:
              ;; Lua doesn't need semi-colon as statment-ending
              (set! eoi? (stack-empty? stack))))
           tok)))))

(define (debug-lua-tokenizer src)
  ((make-token-checker lua-tokenizer) src))
