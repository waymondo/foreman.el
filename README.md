# foreman.el

a super simple lisp for running [foreman](https://github.com/ddollar/foreman) in Emacs

`M-x foreman-start` will find the Procfile in your current project and boot it up in a dedicated shell buffer.

## Installation

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/waymondo/foreman.el.git

In your Emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/foreman.el")
    (require 'foreman)

## License

Copyright 2012 (c) Justin Talbott <justin@waymondo.com>

Released under the same license as GNU Emacs.
