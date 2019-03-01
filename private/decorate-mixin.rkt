#lang racket/base

;; decorate-mixin.rkt -- decorate values in text-field% objects
;;
;; This file is part of gui-widget-mixins -- Mixins to enhance Racket GUI Widgets
;; Copyright (c) 2019 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/gui/base
         racket/class
         racket/contract)

(provide/contract
 (decorate-mixin (-> (-> string? string?) (subclass?/c text-field%)
                     (subclass?/c text-field%))))

(define (decorate-mixin decorate base-class)
  (class base-class
    (init [init-value ""])
    (inherit has-focus?)

    ;; Holds the un-decorated value, what the client will expect when calling
    ;; `get-value`
    (define original init-value)

    ;; When #t, the field contains the decorated value
    (define decorated? #t)

    (define/override (set-value v)
      (set! original v)
      (set! decorated? (not (has-focus?)))
      (super set-value (if decorated? (decorate v) v)))

    (define/override (get-value)
      (if decorated? original (super get-value)))

    (define/override (on-focus on?)
      (if on?
          (begin
            ;; The widget now has focus, put the original value in so it can
            ;; be edited.
            (super set-value original)
            (set! decorated? #f))
          (begin
            ;; The widget has lost focus, display the decorated value.
            (set! original (super get-value))
            (set! decorated? #t)
            (super set-value (decorate original)))))

    (super-new [init-value (decorate init-value)])))
