auto-dictionary-mode
====================

An automatic dictionary switcher for Emacs spell checking

[![Build Status](https://travis-ci.org/nschum/auto-dictionary-mode.png?branch=master)](https://travis-ci.org/nschum/auto-dictionary-mode)

Add the following to your .emacs file:

    (require 'auto-dictionary)
    (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

Then just type.  When you stop for a few moments `auto-dictionary-mode' will
start evaluating the content.

You can also force a check using `adict-guess-dictionary`, whether or not
`auto-dictionary-mode` is enabled.

If you're unhappy with the results, call `adict-change-dictionary` to
change it and stop automatic checks.

You can use `adict-change-dictionary-hook` to hook into any of these changes,
or `adict-conditional-insert` to insert text (like signatures) that will
automatically conform to the language.

To limit the candidate languages when guessing which dictionary to use, add
something like the following to your Emacs config:
```elisp
(eval-after-load "auto-dictionary"
  '(setq adict-dictionary-list
        (mapcar 'adict--guess-dictionary-cons
                '(("en" . "en")
                  ("fr" . "fr")))))
```
Get the full list from `adict-dictionary-list` in auto-dictionary.el, or
alternatively use the Customize interface to do this.
