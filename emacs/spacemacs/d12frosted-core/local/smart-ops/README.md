# smart-ops

**Disclaimer** I am not author of this package. I am regular user that just
wanted to tweak it. Original package can be found
[here](https://github.com/chrisbarrett/spacemacs-layers/tree/master/cb-core/local/smart-ops).

## Summary

Provides a DSL for declaring how to format whitespace padding around operators.
When `smart-ops-mode` is activated, these rules will automatically be applied
after inserting matching characters.

This package also provides a smart backspace command that will intelligently
delete backwards through any padding surrounding the configured operators.

## Installation

Put `smart-ops.el` in your load path or install it using `M-x package-install-file`,
then load the package and activate `smart-ops-mode`.

```elisp
(require 'smart-ops)
(smart-ops-global-mode)
```

## Configuration

The examples below define a smart operators for C++.

Start by defining a basic smart operator:

```elisp
(define-smart-ops-for-mode 'c++-mode
  (smart-ops "+"))
```

Now, when you type `+` in a C++ buffer, you will get the following behaviour
(`|` represents the insertion point):

```c++
1|
1 + |
1 ++ |
```

That `++` operator doesn't look right. It shouldn't be padded. Let's fix that.

```elisp
(define-smart-ops-for-mode 'c++-mode
  (smart-ops "+")
  (smart-ops "++" :pad-before nil :pad-after nil))
```

Now you will get the following behaviour:

```c++
1|
1 + |
1++|
1 +++ |
```

In C++, you don't want to apply any padding if you're defining an operator for a
class:

```c++
class Foo {
  Foo operator+(Foo&);  // We don't want padding around this '+'.
}
```

Let's adapt that definition of `+` to ensure we don't use padding for operator
overloads.

(NB: The enclosing `define-smart-ops-for-mode` form stays the same, and will be
omitted from now on.)

```elisp
(smart-ops "+"
  :pad-unless
  (lambda (pos)
    (goto-char pos)
    (thing-at-point-looking-at (rx bow "operator" eow (* space)))))
```

This package provides some helper functions to make expressing such common cases
a bit simpler.

```elisp
(smart-ops "+"
  :pad-unless
  (smart-ops-after-match? (rx bow "operator" eow (* space))))
```

Finally, it would be a good idea to re-use this behaviour for other C++
operators that can be overridden in this way. The `smart-ops` function takes
1-or-more strings to define as operators, and will apply the same rules to all
of them.

```elisp
(smart-ops "++" "--" :pad-before nil :pad-after nil)

(smart-ops
  "+" "-" "/" "%" "^" "|" "~" "!" "=" "<<" ">>" "==" "!=" "&&" "||"
  "+=" "-=" "/=" "%=" "^=" "&" "|=" "*=" "<<=" ">>="
  :pad-unless
  (smart-ops-after-match? (rx bow "operator" eow (* space))))
```

You can use the `:action` keyword to perform some extra work after an operator
has been formatted. The example below will place point between angle brackets
when typed consecutively.

```elisp
(smart-ops "<>"
           :pad-before nil :pad-after nil
           :action (lambda (&rest _) (search-backward ">")))
```

More options are available; check the docstring for the `smart-ops` function for
the full details.

Finally, to configure a mode with some basic defaults, use the following
pattern:

```elisp
(define-smart-ops-for-mode 'csharp-mode
  (smart-ops "," ";" :pad-before nil)
  (smart-ops-default-ops))
```
