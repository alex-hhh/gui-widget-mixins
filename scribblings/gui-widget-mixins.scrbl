#lang scribble/manual
@require[@for-label[gui-widget-mixins
                    racket/gui/base
                    racket/contract
                    racket/class
                    racket/base]]

@title{gui-widget-mixins} @author{Alex Harsanyi}

@defmodule[gui-widget-mixins]

A small collection of @racket[mixin] classes to improve the GUI widgets
provided by Racket.  These mixins be used individually or combined together.
This
@hyperlink["https://alex-hhh.github.io/2018/11/an-enhanced-text-field-gui-control-for-racket.html"]{blog
post} describes how these mixins work and this library is an improved version
of what is presented there. The following mixins are defined:

@itemlist[

@item{@racket[cue-mixin] -- a mixin which will add a cue text to a
@racket[text-field%], whenever the field is empty, this can be used to provide
a hint for what the contents of the field are expected to be}

@item{@racket[decorate-mixin] -- a mixin to decorate the contents of
@racket[text-field%] widgets}

@item{@racket[tooltip-mixin] -- a mixin which adds a tooltip to any GUI
control, when the user hovers the mouse over the control}

@item{@racket[validate-mixin] -- a mixin to provide data validation for
@racket[text-field%] widgets, for example to allow the user to only enter
numbers, or valid dates}

]

@section{Examples}

To run the examples shown in this section, you will need a simple GUI
application with a @racket[frame%] instance, so the the following "prelude"
code is used in all examples, this should be added before the examples shown
below, in order to obtain runnable programs.

@codeblock{
#lang racket/gui
(require gui-widget-mixins)
(define toplevel (new frame% [label "Example"] [border 15]))
(send toplevel show #t)
}

The @racket[cue-mixin] will add a cue text to a @racket[text-field], whenever
the field is empty, this can be used to provide a hint for what the contents
of the field are expected to be. In the example below, the input
@racket[text-field%] will display the cue "meters/second" when it is empty.
This will show up in gray color, to make it distinct from normal input and the
client code will get an empty string back when it retrieves the value of the
field.

@codeblock{
(define speed-input
  (new (cue-mixin "meters/second" text-field%)
       [parent toplevel] [label "Speed "]))
}

@centered{
@image[#:suffixes @list[".png"]]{scribblings/image/cue-example}
}

The @racket[decorate-mixin] can be used to decorate a value when the text
input does not have focus, and remove the decoration when the user edits the
value.  To continue our previous example, the decorate mixin which adds "m/s"
to the value entered in the field can be defined like this:

@codeblock{
(define speed-input
  (new (decorate-mixin (decorate-with "m/s") text-field%)
       [parent toplevel] [label "Speed "] [init-value "3.5"]))
}

@centered{
@image[#:suffixes @list[".png"]]{scribblings/image/decorate-example-1}
@image[#:suffixes @list[".png"]]{scribblings/image/decorate-example-2}
}

Unlike the other mixins in this package, the @racket[tooltip-mixin] can be
used to add tooltips to any gui control, in the example below, it is added to
the @racket[button%] class.  The class returned by @racket[tooltip-mixin] will
accept an extra init parameter to specify the string to use:

@codeblock{
(define button
  (new (tooltip-mixin button%)
       [parent toplevel] [label "Push Me"]
       [tooltip "Tooltip for the Push Me button"]))
}

@centered{
@image[#:suffixes @list[".png"]]{scribblings/image/tooltip-example}
}

The @racket[validate-mixin] can be used to restrict the contents of a
@racket[text-field%] to specific values.  It will not actually prevent the
user from typing invalid values in the field, but it will highlight the field
in red if the value is invalid and the @racket[text-field%] callback will only
be invoked for valid values.  There are additional methods and functionality
available, see the reference for @racket[validate-mixin].  As a simple
example, the code below creates a text field which only accepts numeric
values:

@codeblock{
(define speed-input
  (new (validate-mixin string->number number->string text-field%)
       [parent toplevel] [label "Speed "] [init-value "3.5"]))
}

@centered{
@image[#:suffixes @list[".png"]]{scribblings/image/validate-example-1}
@image[#:suffixes @list[".png"]]{scribblings/image/validate-example-2}
}

These mixins can be combined together, for example the code below will create
a class which will have tooltip support, a cue text, and the contents will be
decorated and validated.  Since most of this functionality is visible when the
user interacts with the field, there is no screen-shot provided.

@codeblock{
(define speed-input%
  (tooltip-mixin
   (cue-mixin
    "meters/second"
    (decorate-mixin
     (decorate-with "m/s" #:validate string->number)
     (validate-mixin
      string->number number->string
      text-field%)))))

(define speed-input
  (new speed-input% [parent toplevel] [label "Speed "]
       [tooltip "Speed in meters/second"]))
}

@section{Reference}

@defproc[(cue-mixin (default-cue string?) (base-class (subclass?/c text-field%)))
         (subclass?/c text-field%)]{

Extends a @racket[base-class] to display a cue text when the field is empty.
The cue text is displayed in a different color (gray) to make it visually
distinct from normal field contents.

@racket[default-cue] is default cue string to be displayed in all instances of
this class.  This can also be specified for each instance of the resulting
class via the CUE init field.

@racket[base-class] is the class to extend with the cue functionality.  It
must be a @racket[text-field%], or an class derived from @racket[text-field%]

}

@defproc[(decorate-mixin (decorate (-> string? string?))
                         (base-class (subclass?/c text-field%)))
         (subclass?/c text-field%)]{

Extends @racket[base-class] to display a decorated value when the control DOES
NOT have keyboard focus.  This allows, for example, a @racket[text-field%] to
display "5 weeks" when it does not have focus, and to display "5" when the
widget has focus and let the user edit the value.

@racket[decorate] is a function which adds a decoration to its value.  It
receives a string representing the original contents of the
@racket[text-field%] must return a string representing the decorated value.

@racket[base-class] is the class to extend with the decorate functionality.
It must be a @racket[text-field%], or an class derived from
@racket[text-field%]

}

@defproc[(decorate-with (label string?)
                        (#:validate validate-fn (or/c #f (-> string? any/c)) #f))
                        (-> string? string?)]{

Return a helper function that can be used as the decorator for a
@racket[decorate-mixin].  The returned decorator function will append
@racket[label] to its input string, but only when the input string is not
empty and passes an optional @racket[validate-fn] function.

}

@defmixin[tooltip-mixin (window<%>) (window<%>)]{

Extends @racket[base-class] to display a tooltip when the mouse hovers over
the control.  The resulting class will have two additional init fields, which
specify the tooltip and the delay before it shows up.

@defconstructor/auto-super[([tooltip string?]
                            [tooltip-delay (and/c integer? positive?)])]{

@racket[tooltip] is a string representing the tooltip to display when the
mouse is on the widget.

@racket[tooltip-delay] is the amount of time, in milliseconds, that the mouse
needs to hover over the control before the tooltip is shown

}}

@defproc[(validate-mixin (string->data (-> string? any/c))
                         (data->string (-> any/c string>))
                         (base-class (subclass?/c text-field%))
                         (#:allow-empty? allow-empty? boolean?))
         (subclass?/c text-field%)]{

Extends @racket[base-class] to validate its contents according to user
supplied functions.

@racket[string->data] is a function converting a string to the data type that
holds it.  It must return @racket[#f] if the string is not a valid
representation for the input data.

@racket[data->string] is a function which converts a data type into a string
to be displayed in the control.

@racket[base-class] is the class to extend with the validation functionality.
It must be a @racket[text-field%], or an class derived from
@racket[text-field%]

@racket[allow-empty?] indicates whether empty fields are considered valid or
not.  If empty fields are valid, the GET-VALUE/VALIDATED method will return
@racket['empty] for those, so @racket[string->data] can still return
@racket[#f] for empty strings, and they could still be considered valid by the
control.

The resulting mixin class has the following updated behavior, in addition to
the @racket[text-field%] class:

@itemlist[

@item{The control will have a red background when the value is invalid and a
normal background if it is valid.}

@item{The @bold{callback} of the @racket[text-field%] only invoked for valid
values, not every time the control changes.  Also, the callback is only
invoked once per value.}

@item{@bold{callback-delay} is a new @bold{init field} representing a delay in
milliseconds for invoking the callback, this prevents invoking the callback
with lots of intermediate values (e.g. if the user types in 1234 it avoids
invoking the callback for "1", "12", "123" and "1234").  If this field is #f,
there is no delay and the callback is invoked immediately.}

@item{@bold{allow-empty?} is a new @bold{init field} which determines if a
@racket[text-field%] with empty contents is valid or not.  Its default value
is taken from the ALLOW-EMPTY? parameter to the mixin function, but can be
changed for each individual instance of the class.}

@item{@bold{valid-callback} is a new @bold{init field} representing a callback
which is called when the value in the control becomes valid or invalid.  It is
called with the control as the first argument and a flag indicating if the
value is valid or not.  (-> control flag any)}

@item{@bold{validate} is a new @bold{method} which will trigger field
validation immediately}

@item{The @bold{set-value} @bold{method} is updated to allow setting either
string value or values that are converted via DATA->STRING.}

@item{@bold{get-value/validated} is a new @bold{method} to retrieve a value as
converted by @racket[string->data].  It returns @racket[#f] if the value is
invalid and @racket['empty] if the string is empty.}

]

}
