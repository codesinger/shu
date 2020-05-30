#!/usr/bin/python3

import argparse
import subprocess

#
# Copyright (C) 2020 Stewart L. Palmer
#
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
    """Script to generate the the code for a comdb2 row class
    """
    parser = argparse.ArgumentParser(description="Generate skeleton component files")
    parser.add_argument("--shufiles", required=True,
                        help="Path to Shu byte compiled lisp")
    parser.add_argument("--input", required=True,
                        help="Name of the input file")
    parser.add_argument("--output", required=True,
                        help="Name of the output file")
    args = parser.parse_args()

    cmd =(
        f'emacs -batch -l {args.shufiles}/shu-batch-mode.elc '
        f'-f shu-generate-comdb2-code '
        f'{args.input} {args.output}'
        )
    print(cmd)
    subprocess.run(cmd, shell=True)




if __name__ == '__main__':
    main()
