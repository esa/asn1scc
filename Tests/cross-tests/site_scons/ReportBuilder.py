from SCons.Script import *

import glob
import itertools
import os
import json

from commons import *

def do_pair_test(decoder, encoder, test_case, encoding, report_file):
    encoded = int(call_bin([encoder, 'encode']))
    decoded = int(call_bin([decoder, 'decode']))
    result = Result(test_case,
                    encoding,
                    os.path.basename(decoder),
                    os.path.basename(encoder),
                    decoded,
                    encoded)
    report_file.write(json.dumps(result._asdict()) + '\n')

def do_test_case(target, source, env):
    output = str(target[0])
    c_bin = str(source[0])
    ada_bin = str(source[1])
    test_case, encoding = _test_case_info(os.path.dirname(output))
    print("Running case {} with encoding {}...".format(test_case, encoding))
    with open(output, 'w') as report:
        for pair in itertools.permutations([c_bin, ada_bin]):
            do_pair_test(*pair, test_case=test_case, encoding=encoding, report_file=report)

def _test_case_info(path):
    temp = os.path.dirname(path)
    return os.path.basename(temp), os.path.basename(path)

def builder():
    return Builder(action=do_test_case)
