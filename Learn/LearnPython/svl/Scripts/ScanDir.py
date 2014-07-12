# !/usr/bin/python3

import os
import argparse
import re

parser = argparse.ArgumentParser()
parser.add_argument('-p', '--pattern', required=True, help='Pattern to find number. Example:' + "'.*?(\d\d?)\..*'")
parser.add_argument('-n', '--name', required=True,
                    help='New filename template. It should contain place for file number. Example:' + "'Oj Mamochki {0:03d}.avi'")
parser.add_argument('-r', '--run', action='store_true', help='Actual run, otherwise it shows files to be renamed')
parser.add_argument('dir', default='.', help='Directory to scan')

args = parser.parse_args()

pathArg = args.dir
patternArg = args.pattern.lstrip('\'')
nameArg = args.name
runArg = args.run


def scan_dir(path, pattern, name, do_rename):
    files = os.listdir(path)

    for file in files:
        filename = os.path.join(path, file)
        if os.path.isfile(filename):
            match = re.match(pattern, file)
            if match:
                groups = [int(group) for group in match.groups()]
                new_filename = os.path.join(path, name.format(*groups))

                print(new_filename, ' --> ', filename)
                if do_rename:
                    os.rename(filename, new_filename)
            else:
                print('*' * 10, ' ', filename, ' ', '*' * 10)


scan_dir(pathArg, patternArg, nameArg, runArg)

if not runArg:
    answers = {
        'y': True,
        'yes': True,
    }
    print('Would you like to rename files? y/N: ', end='')
    answer = input('Would you like to rename files? y/N: ')
    run = answers.get(answer.lower(), False)

    if run:
        scan_dir(pathArg, patternArg, nameArg, run)
