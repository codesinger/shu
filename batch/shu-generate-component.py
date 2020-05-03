#!/usr/bin/python3

import argparse
import subprocess

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
    args = parser.parse_args()

    cmd =(
        f'emacs -batch -l {args.shufiles}/shu-batch-mode.elc '
        f'-f shu-generate-component '
        f'{args.globalname} {args.namespace} {args.classname} "{args.author}"'
        )
    print(cmd)
    subprocess.run(cmd, shell=True)




if __name__ == '__main__':
    main()
