Apache Mode for Emacs
=====================

This is a major mode intended for editing Apache HTTPD configuration
files.  It provides syntax colouring to identify sections and
keywords, to catch misspellings, and to look nice.  It works either
with GNU Emacs or with XEmacs/SXEmacs.

If you are reading this file offline then the master source code
repository can be found at: https://github.com/martenjj/apachemode


Installation
------------

If the mode is not included as standard with your Linux distro or Emacs
installation, then you can download it from here and install it
yourself.  Clone the Git tree or download a tarball, go into the
'apachemode' directory and simply do 'make' to generate the Lisp files.

Emacs only needs the two generated .el files (apache-mode-el and
apache-mode-autoload.el); look near the end of apache-mode.el or see
the docstring of apache-set-file-patterns within Emacs to see how to
get them loaded.


History
-------

This mode was originally written in 1999, long before the advent of
online repositories such as GitHub.  Since then a number of people and
distribution channels have taken it and improved and extended it in
various ways - see the Git history for more information and credits.
This is completely within the spirit of open source software and
there's nothing nothing wrong with that at all, but it means that
there are a number of different versions being distributed and
available for download but all with slightly different features and
implementations.

This repository on GitHub is an attempt to bring all of those
different versions together again.  I have tried to combine the best
features of each available version and to ensure that it is
up-to-date.  Further development is encouraged, so if you have any
suggestions or improvements then please report issues or make a pull
request here.
