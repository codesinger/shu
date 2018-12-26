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
because that is what my day job, and occasionally my hobby, requires.

There is also a set of functions that allow one to use emacs as a key ring or
password manager.  Information about user IDs, passwords, URLs, and other items of
interest is kept in an ASCII text file, which may be encrypted.  emacs commands are
used to find entries and to place their values into the kill ring (clip board), from
which they can be pasted into the appropriate place.

## Directories ##

This repository contains four subdirectories.

 1. **lisp** contains the source code of the package

 2. **test** contains a set of unit tests for some, but not all, of the code in the
   `lisp` directory.

 3. **personal** contains personal files that I use with emacs.  These are not part
    of the `shu` package.

 4. **usr** conatains files that are normally modified or replaced by users of this
   package.

## External names ##

All of the externally visible names in these packages (functions macros, constants,
etc.) begin with the prefix `shu-`.  This keeps them from interfering with names
that are part of emacs or part of other packages.  But it also makes them a bit
cumbersome to type.  Each lisp file contains a function that will create a short
alias for each function name.  THis is usually the original function name with the
`shu-` prefix removed.


## Detailed description <a name=detaileddescription></a>


### shu-project ###



### Generate skeleton class and component files <a name=generatecode></a>



### shu-keyring ###

`shu-keyring.el` contains a set of functions for maintaining and querying a keyring of
names, URLs, users IDs, passwords, and related information that are maintained in an
external keyring file.

Functions allow you to find a keyring entry by name and to put one piece of its
information, such as user ID or password, in the clip board, from which it may be
pasted into a browser or other application.

The keyring file may be encrypted with GPG.  As of emacs 23, the EasyPG package is
included with the emacs distribution.  When you tell emacs to open a file that has
been encrypted with GPG, you are prompted for the passphrase and the file is
decrypted into memory.

The file `keyring.txt` in the **usr** directory is am example of a small keyring
file that has not been encruypted.  Each entry in the file consists of a set of name
value pairs.  Each value may be enclosed in quotes if it contains embedded blanks.

A single set of name value pairs starts with an opening `<` and is terminated by a
closing `\>`.

Here is an example of a set of name value pairs taken from that file:

```
< name="Fred email" url=mail.google.com  id=freddy@gmail.com  pw=secret />
```

The names may be arbitrary but there are six names that are recognized by the
functons in `keyring.el`.  They are:

 1. **acct** represents an account number

 2. **id** represents a user ID

 3. **name** represents the name of the entry.  This is the key that is used to find
    the entry.  If mo name is given, then the name of the URL is used.  If the URL
    sarts with `www.`, the `www.` is removed to form the name.  An entry that has no
    name and a URL of `www.facebook.com` would have an auto generated name of `facebook`.

 4. **pin** represents a pin number

 5. **pw** represents a password

 6. **url** represents a URL

To use a keying file, place the following lines in your `.emacs` file:

```
(load-file "~/.emacs.d/shu-base.elc")
(load-file "~/.emacs.d/shu-nvplist.elc")
(load-file "~/.emacs.d/shu-keyring.elc")
(shu-keyring-set-alias)
(setq shu-keyring-file "~/shu/usr/keyring.txt")
```

replacing `~/shu/usr/keyring.txt` with the path to your keyring file.

All of the `shu` functions require `shu-base`.

If using the sample keyrig file,
Fred can now use this to log onto his gmail account as follows.

Type <kbd>M-x krurl</kbd>.  This prompts for the name of the desired key.  Type
`Fred em` and hit <kbd>TAB</kbd> to complete.  This fills out the name as 'Fred
email` and puts the URL `mail.google.com` into the clip board.  Open a browser and
paste the URL into it to go to gmail.  At gmail, select login.  In emacs type <kbd>M-x
krid</kbd>.  When prompted for the key, use the up arrow to retrieve tha last key
used, which will be `Fred email`.  This puts `freddy@gmail.com` into the clip board
for conveniently pasting into the gmail widow.  To obtain the password, type <kbd>M-x
krpw</kbd>.  This puts the password into the clip board from which it may be paste
into the gmail widow.


### List of short keyring commands ###

**kracct** Put the account number in the clip board.

**krfn** Display the name of the keyring file, if any.

**krid** Put the user ID in the clip board.

**krpin** Put the pin number in the clip board.

**krpw** Put the password in the clip board.

**krurl** Put the URK in the clip board.

**krvf** Validate the keyring file by parsing it and displaying the results in a buffer.



## References ##


### BDE coding standard ###


<https://github.com/bloomberg/bde/wiki/Introduction-to-BDE-Coding-Standards>
