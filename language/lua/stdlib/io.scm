;;  Copyright (C) 2016
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

(define-module (language lua stdlib io)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (primitive:lua-print))

;; NOTE: `print' returns nothing (unspecified in Guile). A variable assigned to
;;       `print' application result should get `nil' as its value.
(define (primitive:lua-print thunk)
  (define (fix x)
    (match x
      ((? boolean?) (if x 'true 'false))
      ('(marker nil) 'nil)
      ((? unspecified?) "")
      (else x)))
  (call-with-values
      (lambda () (thunk))
    (lambda args
      (for-each (lambda (xx) (format #t "~a~/" (fix xx))) args)
      (newline))))
