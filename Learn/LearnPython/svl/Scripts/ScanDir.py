#!/usr/bin/python3

import os
import argparse
import re

parser = argparse.ArgumentParser()
parser.add_argument('-p', '--pattern', required = True, help = 'Pattern to find number. Example:' + "'.*?(\d\d?)\..*'")
parser.add_argument('-n', '--name', required = True, help = 'New filename template. It should contain place for file number. Example:' + "'Oj Mamochki {0:03d}.avi'")
parser.add_argument('-r', '--run', action = 'store_true', help = 'Actual run, otherwise it shows files to be renamed')
parser.add_argument('dir', default= '.', help = 'Directory to scan')

args = parser.parse_args()

path = args.dir
pattern = args.pattern.lstrip('\'')
name = args.name
run = args.run

print(pattern)
print(path)

#path = '/home/victor'

files = os.listdir(path)

for file in files:
    filename=os.path.join(path, file)
    if os.path.isfile(filename):
        match = re.match(pattern, file)
        if match:
            newFilename = os.path.join(path, name.format(int(match.group(1))))
            print(newFilename, ' --> ', filename)
            if run:
                os.rename(filename, newFilename)
        else:
            print('*' * 10, ' ', filename, ' ', '*' * 10)
