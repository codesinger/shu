![Shu](https://raw.github.com/codesinger/shu/master/doc/shu-2011-l.png)
*Shu Swamp, Charles T. Church Nature Preserve, Mill Neck, NY*
# Shu #
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
  * [shu-project](#shu-project)
  * [Generate skeleton class and component files](#generatedcode)
  * [shu-keyring](#shu-keyring)
* [References](#references)


## Overview ##

Most of the code in this repository helps me to read and write C and C++ code
because that is what my day job, and occasionally my hobby, reauires.

There is also a set of functions that allow one to use emacs as a key ring or
password manager.  Information about user IDs, passwords, URLs, and other things is
kept in an ASCII text file, which may be encrypted.  emacs commands are used to find
entries and to place their values into the kill ring, from which they can be pasted
into the appropriate place.

## Directories ##

THis repository contains three subdirectories.

 1. **lisp** contains the source code of the package

 2. **test** contains a set of unit tests for some, but not all, of the code in the
   `lisp` directory.

 3. **usr** conatains files that are normally modified or replaced by users of this
   package.



## Detailed description <a name=detaileddescription></a>


### shu-project ###



### Generate skeleton class and component files <a name=generatecode></a>



### shu-keyring ###



## References ##


### BDE coding standard ###


<https://github.com/bloomberg/bde/wiki/Introduction-to-BDE-Coding-Standards>
