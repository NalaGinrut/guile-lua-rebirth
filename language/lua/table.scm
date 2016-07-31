;;  Copyright (C) 2014,2015,2016
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
  #:use-module (language lua types)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-69)
  #:use-module ((rnrs) #:select (div))
  #:export (new-lua-table
            try-lua-array-set!
            lua-array-ref
            try-lua-table-set!
            lua-table-ref))

(define-record-type <lua-table>
  (%make-lua-table hash-part array-part array-max)
  is-lua-table?
  (hash-part lua-table-hash-part lua-table-hash-part!)
  (array-part lua-table-array-part lua-table-array-part!)
  (array-max lua-table-array-max lua-table-array-max!))

(define (make-lua-table)
  (%make-lua-table #f #f 0))

(define (new-lua-table)
  `(call (@@ (language lua table) make-lua-table)))

(define (create-array-part size)
  (make-array #f (if (zero? size) 1 size)))

(define (resize-array-part! t i)
  (let ((new-ap (create-array-part i))
        (ap (lua-table-array-part t)))
    (array-copy! ap new-ap)
    (lua-table-array-max! t i)
    (lua-table-array-part! t new-ap)))

(define (try-lua-array-set! t i v)
  (let ((ap (lua-table-array-part t)))
    (array-set! ap v i)
    (lua-table-array-max! t (max i (lua-table-array-max t)))))

(define (lua-array-ref t i)
  (let ((ap (lua-table-array-part t)))
    (array-ref ap i)))

(define (array-should-be-resized? t)
  (let* ((ap (lua-table-hash-part t))
         (size (lua-table-array-max t))
         (half (div size 2)))
    ;; all the numbers of 1 ~ (n/2) are in the slots
    ;; any number of (n/2)+1 ~ n is in the slots
    (let lp((i 0) (overhalf? #f))
      (cond
       ((> i size) #t) ; slots are full
       ((array-ref ap i) ; number is in the slots
        (if overhalf? ; now it's in the upper of slots
            #t ; at least one number of (n/2)+1 ~ n in the slots
            (lp (1+ i) (> (1+ i) half)))) ; try next number
       (else
        ;; number is NOT in the slots
        (if overhalf? ; now it's in the upper of slots
            #f ; not all numbers of 1 ~ (n/2) are in the slots
            (lp (1+ i) (> (1+ i) half)))))))) ; try next number

(define (try-lua-table-set! t k v)
  (let ((tp (lua-table-hash-part t)))
    (when (not tp) (lua-table-hash-part! t (make-hash-table)))
    (hash-table-set! (lua-table-hash-part t) k v)))

(define (lua-table-set! t k v)
  (cond
   ((integer? k)
    (when (and (>= k (lua-table-array-max t)) (array-should-be-resized? t))
          ;; k = array-size is necessary, since we need to allocate array
          ;; when k=0 and array is null.
          ;; NOTE: we're little different from Lua-5.0, the first index of
          ;;       array in guile-lua-rebirth is 0, not 1.
          (resize-array-part! t k))
    (cond
     ;; integer k < array-size will store in the array
     ((lua-table-array-part t) (try-lua-array-set! t k v))
     ;; integer k >= array-size will store in the hashtable
     (else (try-lua-table-set! t k v))))
   ;; non-integer k will store in the hashtable
   (else (try-lua-table-set! t k v))))

;; if no value in table, then return nil
(define (lua-table-ref t k)
  (or
   (cond
    ((integer? k)
     (or (array-ref (lua-table-array-part t) k)
         (hash-table-ref (lua-table-hash-part t) k)))
    (else (hash-table-ref (lua-table-hash-part t) k)))
   (gen-nil)))
