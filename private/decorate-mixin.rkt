#lang racket/base
;; SPDX-License-Identifier: MIT or Apache-2.0
;; decorate-mixin.rkt -- decorate values in text-field% objects
;;
;; This file is part of gui-widget-mixins -- Mixins to enhance Racket GUI Widgets
;; Copyright (c) 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>

(require racket/gui/base
         racket/class
         racket/contract)

(provide/contract
 (decorate-mixin (-> (-> string? string?) (subclass?/c text-field%)
                     (subclass?/c text-field%)))
 (decorate-with (->* (string?) (#:validate (or/c #f (-> string? any/c)))
                     (-> string? string?))))

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

(define (decorate-with label #:validate [validate-fn #f])
  (lambda (s)
    (if (and (> (string-length label) 0)
             (or (not validate-fn) (validate-fn s)))
        (string-append s " " label)
        s)))
