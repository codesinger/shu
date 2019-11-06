![Shu](https://raw.github.com/codesinger/shu/master/doc/shu-2011-l.png)
*Shu Swamp, Charles T. Church Nature Preserve, Mill Neck, NY*
# Shu emacs lisp package #
My personal Emacs configuration and useful lisp functions.

This repository is an ongoing project.  A colleague once asked me if I could let him
copy some of my Emacs lisp functions.  I discovered that all of my Emacs lisp code
was highly modular in the absolutely worst sense: Every file depended heavily on every
other file.  If you wanted to take some of the Emacs lisp code, you had to take all
of it and live with the fact that it completely changed the behavior of your own
personal Emacs.  That did not seem acceptable.  Hence this project.

The goal of this project is to split all of the Emacs lisp into stand alone, useful
things that can be copied piecemeal.  If you like one thing, you can take it and use
it without having to drag along everything else.

This is an incomplete work in progress.


**Table Of Contents**

* [Overview](#overview)
* [Directories](#directories)
* [Detailed description](#detaileddescription)
* [References](#references)


## Overview ##

Most of the code in this repository helps me to read and write C and C++ code
because that is what my day job, and occasionally my hobby, requires.

There is also a set of functions that allow one to use emacs as a key ring or
password manager.  Information about user IDs, passwords, URLs, and other items of
interest is kept in an ASCII text file, which may be encrypted.  emacs commands are
used to find entries and to place their values into the kill ring (clip board), from
which they can be pasted into the appropriate place.

## Directories ##

This repository contains six subdirectories.

 1. **lisp** contains the source code of the package

 2. **doc** contains the detailed documentation for all of the functions, constants,
    variables, etc.

 3. **test** contains a set of unit tests for some, but not all, of the code in the
   `lisp` directory.

 4. **personal** contains personal files that I use with emacs.  These are not part
    of the `shu` package.

 5. **usr** contains files that are normally modified or replaced by users of this
   package.

 6. **exp** contains experimental code that may or may not be useful in the future.

## External names ##

All of the externally visible names in these packages (functions macros, constants,
etc.) begin with the prefix `shu-`.  This keeps them from interfering with names
that are part of emacs or part of other packages.  But it also makes them a bit
cumbersome to type.  Each lisp file contains a function that will create a short
alias for each function name.  This is usually the original function name with the
`shu-` prefix removed.


## Detailed description <a name=detaileddescription></a>


See the files `shu-manual.md` `and shu-manual.pdf` in the `doc` directory.


## References ##


### BDE coding standard ###


<https://github.com/bloomberg/bde/wiki/Introduction-to-BDE-Coding-Standards>

<!--
LocalWords:  Shu NY detaileddescription IDs doc shu usr exp md pdf BDE
-->
