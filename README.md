gui-widget-mixins
=================

A small collection of mixin classes to improve the GUI widgets provided by
Racket.  These mixins be used individually or combined together.  This [blog
post][bp] describes how these mixins work and this library is an improved
version of what is presented there. The following mixins are defined:

* `cue-mixin` -- a mixin which will add a cue text to a `text-field%`,
  whenever the field is empty, this can be used to provide a hint for what the
  contents of the field are expected to be

* `decorate-mixin` -- a mixin to decorate the contents of `text-field%`
  widgets.

* `tooltip-mixin` -- a mixin which adds a tooltip to any GUI control, when the
  user hovers the mouse over the control

* `validate-mixin` -- a mixin to provide data validation for `text-field%`
  widgets, for example to allow the user to only enter numbers, or valid
  dates

You can install this package using

```
raco pkg install gui-widget-mixins
```

[bp]: https://alex-hhh.github.io/2018/11/an-enhanced-text-field-gui-control-for-racket.html
