;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Ragnarok is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Ragnarok is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (language lua utils)
  #:use-module (ice-9 q)
  #:use-module (system base language)
  #:use-module (system base lalr)
  #:use-module (system base pmatch))

(module-export-all! (current-module))

(define-syntax-rule (group-checker what c)
  (string-contains what (string c)))

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

(define-syntax-rule (pmatch/source x clause ...)
  (let ((x x))
    (let ((res (pmatch x
                 clause ...)))
      (let ((loc (location x)))
        (if loc
            (set-source-properties! res (location x))))
      res)))

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
