#lang racket/base
;; SPDX-License-Identifier: MIT or Apache-2.0
;; cue-mixin.rkt -- add a cue text to text-field% objects
;;
;; This file is part of gui-widget-mixins -- Mixins to enhance Racket GUI Widgets
;; Copyright (c) 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>


(require racket/gui/base
         racket/class
         racket/string
         racket/contract)

(provide/contract
 (cue-mixin (-> string? (subclass?/c text-field%) (subclass?/c text-field%))))

(define cue-text-style
  (let ((grey-text (new style-delta%)))
    (send grey-text set-delta-foreground "gray")
    grey-text))

(define normal-text-style
  (let ((black-text (new style-delta%)))
    (send black-text set-delta-foreground "black")
    black-text))

(define (text-empty? a-text)
  (define snip (send a-text find-first-snip))
  (or (not snip) (= 0 (send snip get-count))))

(define (cue-mixin default-cue base-class)
  (class base-class
    (init-field [cue default-cue])
    (init [callback #f])
    (inherit get-editor)

    (define mixin-callback callback)
    (define showing-cue? #f)

    (define/private (clear-cue)
      (define editor (get-editor))
      (when (and editor showing-cue?)
        (send* editor
          (erase)
          (change-style normal-text-style 'start 'end #f))
        (set! showing-cue? #f)))

    (define/private (maybe-insert-cue)
      (define editor (get-editor))
      (unless (or (not editor) showing-cue? (not (text-empty? editor)))
        (send* editor
          ;; NOTE; change-style will change *selected* text between start and
          ;; end, and make it sticky, so text inserted after 'end will also
          ;; have the same style.  It is simpler to start with an empty text,
          ;; apply the style and than insert the cue, otherwise we would have
          ;; to select the cue, apply the style and un-select it.
          (change-style cue-text-style 'start 'end #f)
          (insert cue)
          (move-position 'home))
        (set! showing-cue? #t)))

    (define/override (on-subwindow-char receiver event)
      (clear-cue)
      (begin0 (super on-subwindow-char receiver event)
        (queue-callback (lambda () (maybe-insert-cue)))))

    (define/private (on-callback control event)
      (when (and mixin-callback (not showing-cue?))
        (mixin-callback control event)))

    (define/override (set-value v)
      (clear-cue)
      (super set-value v)
      (maybe-insert-cue))

    (define/override (get-value)
      (if showing-cue? "" (super get-value)))

    (super-new [callback (lambda (c e) (on-callback c e))])
    (maybe-insert-cue)))
