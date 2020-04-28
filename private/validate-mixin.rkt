#lang racket/base

;; validate-mixin.rkt -- add validation to text-field% objects
;;
;; This file is part of gui-widget-mixins -- Mixins to enhance Racket GUI Widgets
;; Copyright (c) 2019, 2020 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/string
         racket/contract)

(provide/contract
 (validate-mixin (->* ((-> string? any/c) (-> any/c string?) (subclass?/c text-field%))
                      (#:allow-empty? boolean?)
                      (subclass?/c text-field%))))


(define (validate-mixin string->data data->string base-class
                        #:allow-empty? (allow-empty #f))
  (class base-class
    (init-field [callback-delay #f]
                [allow-empty? allow-empty]
                [valid-callback #f])
    (init [callback #f])
    (inherit get-value get-field-background set-field-background)

    (define mixin-callback callback)

    ;; Saved values for CONTROL and EVENT callback parameters, used when the
    ;; callback is delayed (see CALLBACK-DELAY)
    (define callback-control #f)
    (define callback-event #f)

    ;; The previous value for which the user callback has been invoked, used
    ;; to avoid repeatedly invoking the callback for the same value.
    (define previous-value #f)

    (define (delayed-callback)
      (when (and mixin-callback callback-control callback-event)
        (mixin-callback callback-control callback-event)
        (set! callback-control #f)
        (set! callback-event #f)))

    (define timer (new timer% [notify-callback delayed-callback]))

    ;; This callback is passed to the super class and ensures that the our
    ;; client only receives callback invocations on valid values and after the
    ;; appropriate delay.
    (define (on-callback control event)
      ;; Only pass-on the callback if the value is actually valid
      (when (and mixin-callback (valid-value? (get-value)))
        (define value (get-value/validated))
        (unless (equal? previous-value value)
          ;; avoid delivering the callback for the same value
          (set! previous-value value)
          (if callback-delay
              (begin
                (set! callback-control control)
                (set! callback-event event)
                (send timer start callback-delay #t))
              (mixin-callback control event)))))

    ;; Return #t if DATA is valid (taking the ALLOW-EMPTY? value in
    ;; consideration).
    (define (valid-value? data)
      (let ([t (string-trim data)])
        (or (and allow-empty? (string=? t "")) (string->data t))))

    ;; Original background color for the field, can only be retrieved after
    ;; the parent class has been initialized.
    (define good-bg #f)
    ;; Background color to use when value is invalid.
    (define bad-bg (send the-color-database find-color "Tomato"))

    ;; Trigger a validation of the contents, will update the background
    ;; according to whether the value is valid or not and will invoke
    ;; VALID-CALLBACK with the current validity status.
    (define/public (validate)
      (let* ([valid? (valid-value? (get-value))]
             [bg-color (if valid? good-bg bad-bg)])
        (when bg-color       ; might be #f early in the initialization process
          (set-field-background bg-color))
        (when valid-callback
          (valid-callback this valid?))))

    ;; Intercept keyboard inputs and validate them
    (define/override (on-subwindow-char receiver event)
      (begin0 (super on-subwindow-char receiver event)
        (validate)))

    ;; Update the value in the control either from a string or from a value
    ;; type we manage.
    (define/override (set-value v)
      (super set-value (if (string? v) v (data->string v)))
      (validate))

    ;; Return a value converted using DATA->STRING, returns #f if the contents
    ;; is invalid and 'empty if the contents are empty and ALLOW-EMPTY? is #t.
    (define/public (get-value/validated)
      (let ([v (string-trim (get-value))])
        (if (valid-value? v)
            (if (= (string-length v) 0)
                'empty
                (string->data v))
            #f)))

    ;; Initialize the parent class and pass in our own callback.
    (super-new [callback on-callback])
    (set! good-bg (get-field-background)) ; can now retrieve bg color
    (validate)                            ; must validate initial-value
    ))
