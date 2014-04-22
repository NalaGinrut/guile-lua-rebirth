;;  Copyright (C) 2014
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

;; -----------------------------------------------------------------
;; This module is the implementation of Lua 5.0 table.
;; Ref: <<The implementation of Lua 5.0>> by Roberto etc.

(define-module (language lua table)
  #:use-module (language lua utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-69)
  #:use-module ((rnrs) #:select (div))
  #:export ())

(define-record-type <lua-table>
  (%make-lua-table hash-part array-part array-max)
  is-lua-table?
  (hash-part lua-table-hash-part lua-table-hash-part!)
  (array-part lua-table-array-part lua-table-array-part!)
  (array-max lua-table-array-max lua-table-array-max!))

(define (make-lua-table)
  (%make-lua-table (make-hash-table) #f 0))

(define (create-array-part size)
  (if (zero? size)
      #f
      (make-array #f size)))

(define (resize-array-part! t k v)
  (let ((new-ap (create-array-part (lua-table-array-max t)))
        (new-hp (make-hash-table)))
    (hash-for-each
     (lambda (k v)
       (if (integer? k)
           (array-set! new-ap v k)
           (hash-set! new-hp k v))))
    (lua-table-array-max! t 0)
    (lua-table-array-part! t new-ap)
    (lua-table-hash-part! t new-hp)))     

(define (try-to-set! t k v)
  (let ((ap (lua-table-array-part t)))
    (hash-set! ap k v)
    (lua-table-array-max! t (max k (lua-table-array-max t)))))

(define (array-should-be-resized? t k)
  (let ((hp (lua-table-hash-part t))
        (n (lua-table-array-max t)))
    ;; all the numbers of 1 ~ (n/2) are in the slots
    (and (every (lambda (i) (hash-ref hp i)) (iota (div n 2)))
         ;; any number of (n/2)+1 ~ n is in the slots
         (any (lambda (i) (hash-ref hp i)) (range (1+ (div n 2)) n)))))

(define (lua-table-set! t k v)
  (cond
   ((integer? k)
    (cond
     ((lua-table-array-part t) (try-to-set! t k v))
     ((array-should-be-resized? t) (resize-array-part! t k v))
     (else (hash-table-set! (lua-table-hash-part t) k v))))
  (else (hash-table-set! (lua-table-hash-part t) k v))))
