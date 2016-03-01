;;  Copyright (C) 2014,2016
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

(define-module (language lua optimize)
  #:use-module (language lua types)
  #:use-module (language lua peval)
  #:use-module (language lua lir)
  #:use-module (ice-9 match)
  #:export (lua-optimize
            try-to-optimize-op))

;; This is a Lua specific optimizer.
;; For now, there's only peval(include constant-fold/dead-code-elimination...etc)
;; New optimizing pass could be added, but maybe unecessary, since Guile will do
;; all the rest full-stack optimizing, in principle. But some Lua specific optimizing
;; maybe needed though.
;; NOTE: peval accept lir only, so we have to convert ast to lir before pass it in.
;; NOTE: tree-il compiler accept ast only, so we need to convert lir to ast again.
(define* (lua-optimize x env #:key (peval? #f))
  (if peval?
      (lir->ast (peval (ast->lir x) env))
      x))

(define-syntax-rule (try-to-reduce-func-call op x y env)
  #t)
