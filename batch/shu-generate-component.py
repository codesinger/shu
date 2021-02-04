#!/usr/bin/python3

import argparse
import subprocess

#
# Copyright (C) 2020 Stewart L. Palmer
#
# Package: shu-cpp-misc
# Author: Stewart L. Palmer <stewart@stewartpalmer.com>
#
# This file is NOT part of GNU Emacs.
#
# This is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# There is a copy of the Gnu General Public license in the file
# LICENSE in this repository.  You should also have received a copy
# of the GNU General Public License along with GNU Emacs.  If not,
# see <http://www.gnu.org/licenses/>.


def main():
    """Script to generate the three files of a standard C++ BDE style component
    """
    parser = argparse.ArgumentParser(description="Generate skeleton component files")
    parser.add_argument("--shufiles", required=True,
                        help="Path to Shu byte compiled lisp")
    parser.add_argument("--globalname", default="BloombergLP",
                        help="Company global namespace")
    parser.add_argument("--namespace", required=True,
                        help="Namespace of new component")
    parser.add_argument("--author", required=True,
                        help="Author of new component")
    parser.add_argument("--classname", required=True,
                        help="Name of the class")
    parser.add_argument("--modern", action='store_true',
                        help="If specified, use 'delete' for deleted constructors")
    args = parser.parse_args()

    cmd =(
        f'emacs -batch -l {args.shufiles}/shu-batch-mode.elc '
        f'-f shu-generate-component '
        f'{args.globalname} {args.namespace} {args.classname} "{args.author}" {args.modern}'
        )
    print(cmd)
    subprocess.run(cmd, shell=True)




if __name__ == '__main__':
    main()
