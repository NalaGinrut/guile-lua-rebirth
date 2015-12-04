;;  Copyright (C) 2015
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

(define-module (language lua type-annotation)
  #:use-module (ice-9 rdelim)
  #:use-module (language lua utils)
  #:use-module (language lua irregex)
  #:export (push-function-name
            pop-function-name
            gen-current-func-anno

            push-nearest-block-header
            pop-nearest-block-header
            top-nearest-block-header

            get-func-anno
            print-all-type-annos))

(define (peek-funcname port)
  (when (is-whitespace? (peek-char port))
        (read-char port)
        (peek-funcname port))
  (let ((fname (read-delimited port *delimiters*)))
    (unread-string fname port)
    fname))

(define *function-anno-stack* (new-stack))

(define (push-function-name port)
  (stack-push! *function-anno-stack* (peek-funcname port)))

(define (pop-function-name port)
  (stack-pop! *function-anno-stack*))

(define (gen-current-func-anno)
  (string-join (cdr *function-anno-stack*) "@"))

(define *nearest-block-header* (new-stack))

(define (push-nearest-block-header token)
  (stack-push! *nearest-block-header* token))

(define (pop-nearest-block-header token)
  (stack-pop! *nearest-block-header*))

(define (top-nearest-block-header)
  (stack-top *nearest-block-header*))

(define *func-anno-db* (make-hash-table))

(define (get-func-anno func) (hash-ref *func-anno-db*))

(define *fun-ta-re* (string->sre "@anno:(.*)"))
(define (try-to-detect-type-annotation line)
  (define (store-type-anno pat)
    (hash-set! *func-anno-db* (gen-current-func-anno) pat))
  (define (parse anno)
    (map (lambda (s)
           (string->symbol (string-trim-both s)))
         (string-split anno #\sp)))
  (cond
   ((irregex-match *fun-ta-re* line)
    => (lambda (m)
         (let ((anno (irregex-match-substring m 1)))
           (store-type-anno (parse anno)))))
   (else #f)))

(define (print-all-type-annos)
  (hash-for-each
   (lambda (k v)
     (format #t "~2t~a: ~a~%" k v))
   *func-anno-db*))
