import xml.etree.ElementTree as Et
import os
import argparse


def extract_filename(e):
    filename = e.find('file').text
    modular_index = filename.find('modular')
    if modular_index > -1:
        filename = filename[modular_index:]
    return filename


def process_entry(e, result):
    filename = extract_filename(e)
    if filename in result:
        result[filename] += 1
    else:
        result[filename] = 1


def process_entries(items, result):
    for e in items:
        process_entry(e, result)


def process_file(file, errors):
    tree = Et.parse(file)
    items = tree.findall('.//problem')
    process_entries(items, errors)


def process_files(path):
    errors = dict()
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith('.xml'):
                process_file(os.path.join(root, file), errors)
    return errors


def dump_result(errors):
    sorted_files = sorted(errors.keys())
    for file in sorted_files:
        print(file, errors[file])


parser = argparse.ArgumentParser()
parser.add_argument('dir', default='.', help='Directory to scan')
args = parser.parse_args()

files_with_errors = process_files(args.dir)
dump_result(files_with_errors)

print("Files count: ", len(files_with_errors), "  Errors count: ", sum(files_with_errors.values()))
