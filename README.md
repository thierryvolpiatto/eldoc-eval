Eldoc-eval
==========


## Description
Enable eldoc support when minibuffer is in use.

# Commentary:

This package enables eldoc support when minibuffer is in use.

Eldoc info is shown by default in mode-line,
but you can have eldoc info somewhere else by setting
`eldoc-in-minibuffer-show-fn` to another function (e.g `tooltip-show`).

By default with this package `M-:` will use `pp-eval-expression`
instead of `eval-expression`; you can change that by setting
`eval-preferred-function` to something else.

It also provides a convenient macro to enable eldoc support
in your own functions using minibuffer or in your defadvices,
that is `with-eldoc-in-minibuffer`, e.g:


    defadvice edebug-eval-expression (around with-eldoc activate)
     "This advice enable eldoc support."
     (interactive (list (with-eldoc-in-minibuffer
                          (read-from-minibuffer
                           "Eval: " nil read-expression-map t
                           'read-expression-history))))
     ad-do-it)


Users of own minibuffer frame will have to set
`eldoc-in-minibuffer-own-frame-p` to non-nil.

You can turn On/Off eldoc support in minibuffer at any time
with `eldoc-in-minibuffer-mode`.

# Install:

Add to your init file:

If installed from git, add eldoc-eval.el to `load-path` and

    (autoload 'eldoc-in-minibuffer-mode "eldoc-eval")
    (eldoc-in-minibuffer-mode 1)

Otherwise only `(eldoc-in-minibuffer-mode 1)` is needed.
