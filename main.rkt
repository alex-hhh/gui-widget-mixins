#lang racket/base
;; SPDX-License-Identifier: MIT or Apache-2.0
;; main.rkt -- export all defined mixins
;;
;; This file is part of gui-widget-mixins -- Mixins to enhance Racket GUI Widgets
;; Copyright (c) 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>

(require "private/decorate-mixin.rkt"
         "private/validate-mixin.rkt"
         "private/cue-mixin.rkt"
         "private/tooltip-mixin.rkt")

(provide
 decorate-mixin
 decorate-with
 validate-mixin
 cue-mixin
 tooltip-mixin)

;; raco setup --check-pkg-deps --pkgs gui-widget-mixins
;; raco test --no-run-if-absent --package gui-widget-mixins

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
