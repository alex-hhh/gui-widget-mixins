#lang racket/base
;; SPDX-License-Identifier: MIT or Apache-2.0
;; tooltip-mixin.rkt -- add tooltips to GUI widgets
;;
;; This file is part of gui-widget-mixins -- Mixins to enhance Racket GUI Widgets
;; Copyright (c) 2019, 2020, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>

(require racket/gui/base
         racket/class
         racket/string
         racket/math
         racket/contract)

(provide/contract
 (tooltip-mixin (-> (implementation?/c window<%>) (implementation?/c window<%>))))

;; TODO: we could easily update this to handle an arbitrary pict as a tooltip,
;; or multi-line tool-tips
(define tooltip-canvas%
  (class canvas%
    (init-field label)
    (inherit min-width min-height get-client-size get-dc)

    (define border 5)
    (define color (send the-color-database find-color "LightYellow"))

    (define/override (on-paint)
      (let ([dc (get-dc)])
        (send dc set-pen color 1 'transparent)
        (send dc set-brush color 'solid)
        (let-values ([(cw ch) (get-client-size)])
          (send dc draw-rectangle 0 0 cw ch)
          (send dc set-font small-control-font)
          (let-values ([(lw lh _1 _2) (send dc get-text-extent label)])
            (send dc draw-text label
                  (- (/ cw 2) (/ lw 2))
                  (- (/ ch 2) (/ lh 2)))))))

    (super-new)
    (let ((dc (get-dc)))
      (let-values ([(text-width text-height _1 _2)
                    (send dc get-text-extent label small-control-font)])
        (min-width (exact-ceiling (+ text-width border)))
        (min-height (exact-ceiling (+ text-height border)))))))

(define (make-tooltip-frame tooltip)
  (define frame (new frame%
                     [label ""]
                     [style '(no-caption no-resize-border float)]
                     [stretchable-width #f]
                     [stretchable-height #f]))
  (new tooltip-canvas% [parent frame] [label tooltip])
  (send frame reflow-container)
  frame)

(define (tooltip-mixin base-class)
  (class base-class
    (init-field [tooltip #f]
                [tooltip-delay 500])
    (inherit client->screen get-client-size get-width is-shown?)

    (define tooltip-window #f)
    (define mouse-inside-widget? #f)

    (define popup-x 0)
    (define popup-y 0)
    (define delta-move 10)
    (define offset 2)

    (define timer (new timer%
                       [notify-callback
                        (lambda ()
                          (when (and mouse-inside-widget? (is-shown?))
                            (send tooltip-window show #t)))]))

    ;; `get-display-left-top-inset` is not guaranteed to return 0,0 for
    ;; monitor 0.  See #3 for a counter example.  Since the offset is only
    ;; needed on MacOS to account for the menu bar at the top of the screen,
    ;; we only use this function on that platform (where it seems to work
    ;; correctly)
    (define get-offset-fn
      (if (equal? (system-type 'os) 'macosx)
          get-display-left-top-inset
          (lambda () (values 0 0))))

    (define/private (position-tooltip receiver event)
      (when tooltip
        (unless tooltip-window
          (set! tooltip-window (make-tooltip-frame tooltip)))
        (define ex (send event get-x))
        (define ey (send event get-y))
        (define twh (send tooltip-window get-height))
        (let-values ([(x y) (send receiver client->screen (+ ex offset) (- ey offset))]
                     [(dx dy) (get-offset-fn)])
          (send tooltip-window move (- x dx) (- y dy twh)))))

    (define/private (show-tooltip receiver event show?)
      (when tooltip
        (if show?
            (begin
              (when (and receiver event)
                (set! popup-x (send event get-x))
                (set! popup-y (send event get-y))
                (position-tooltip receiver event))
              (send timer start tooltip-delay #f))
            (when (and tooltip-window (send tooltip-window is-shown?))
              (send timer stop)
              (send tooltip-window show #f)))))

    (define/override (on-subwindow-char receiver event)
      (when tooltip-window
        (show-tooltip receiver event #f))
      (super on-subwindow-char receiver event))

    (define/override (on-superwindow-show shown?)
      (unless shown?
        (show-tooltip #f #f #f)))

    (define/override (on-subwindow-event receiver event)
      (define event-type (send event get-event-type))
      (case event-type
        ((leave)
         (set! mouse-inside-widget? #f)
         (show-tooltip receiver event #f))
        ((enter motion)
         (define mouse-x (send event get-x))
         (define mouse-y (send event get-y))
         (define-values (cw ch) (get-client-size))
         ;; NOTE: motion events don't seem to be sent by GTK (Gnome?) (see #1)
         ;; while mouse leave events are sometimes missed on Windows (see #2),
         ;; so we consider that the mouse is inside the widget if either this
         ;; is an enter event or if the mouse event coordinates are over the
         ;; widget.
         (set! mouse-inside-widget?
               (or (equal? 'enter event-type)
                   (and (>= mouse-x 0) (< mouse-x cw) (>= mouse-y 0) (< mouse-y ch))))
         (when mouse-inside-widget?
           (let ((dx (abs (- mouse-x popup-x)))
                 (dy (abs (- mouse-y popup-y))))
             (if (or (>= dx delta-move) (>= dy delta-move))
                 (show-tooltip receiver event
                               (not (and tooltip-window
                                         (send tooltip-window is-shown?))))
                 (when tooltip-window
                   (position-tooltip receiver event))))))
        (else
         (show-tooltip receiver event #f)))
      ;; Let our superclass handle the event
      (super on-subwindow-event receiver event))

    (super-new)))
