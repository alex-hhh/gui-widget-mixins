#lang racket/base

;; main.rkt -- export all defined mixins
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

(require "private/decorate-mixin.rkt"
         "private/validate-mixin.rkt"
         "private/cue-mixin.rkt"
         "private/tooltip-mixin.rkt")

(provide
 decorate-mixin
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
