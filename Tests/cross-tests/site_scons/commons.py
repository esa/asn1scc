import os
import re
import subprocess
import itertools

from collections import namedtuple

import SCons

Result = namedtuple('Result', ['test_case', 'encoding', 'decoder', 'encoder', 'decoded', 'encoded'])

class ChildProcessError(BaseException):
    pass

class TooManyFilesError(BaseException):
    pass

def find_files_recursive(search_path, file_regex):
    regex = re.compile(file_regex)
    return [os.path.join(root, filename)
            for root, dirnames, filenames in os.walk(search_path)
            for filename in filenames
            if regex.match(filename)]

def glob_files_recursive(search_path, file_regex):
    out = []
    regex = re.compile(file_regex)
    for node in SCons.Script.Glob(os.path.join(search_path, "*")):
        _handle_node(node, regex, out)
    return out

def find_unique_file(search_path, file_regex):
    return _expect_one(find_files_recursive(search_path, file_regex))

def glob_unique_file(search_path, file_regex):
    return _expect_one(glob_files_recursive(search_path, file_regex))

def _expect_one(files):
    if len(files) != 1:
        raise TooManyFilesError("Found {} files: {}".format(len(files), files))
    return files[0]

def _handle_node(node, regex, out):
    if type(node) is SCons.Node.FS.Dir:
        out += glob_files_recursive(str(node), regex)
    elif regex.match(str(node)):
        out.append(str(node))

def list_files(directory):
    return [os.path.join(directory, file_) for file_ in os.listdir(directory)]

def get_files_with_suffix(sources, suffix):
    regex = re.compile(".*({})$".format(suffix))
    return [source for source in sources if regex.match(source)]

def call_bin(command):
    process = subprocess.Popen(command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if stderr or process.returncode != 0:
        raise ChildProcessError("Command failed: '" + str(stderr.strip()) + "'")
    return stdout

def grouper(n, iterable, fillvalue=None):
    "grouper(3, 'ABCDEFG', 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return itertools.izip_longest(fillvalue=fillvalue, *args)

def without_top_directory(nodes, n=1):
    return [_cut_top_dirs_from(str(node), n) for node in nodes]

def _cut_top_dirs_from(node, n=1):
    return '/'.join(node.split('/')[n:])

def to_strings(nodes):
    return [str(node) for node in nodes]
