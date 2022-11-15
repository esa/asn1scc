from SCons.Script import *

import json

from commons import Result

def do_summary(target, source, env):
    passed_test_cases = sum(_handle_report(report_file) for report_file in source)
    _print_summary(passed_test_cases, len(source) * 2)

def _handle_report(report_file):
    with open(str(report_file), 'r') as report:
        return sum(_handle_pair(line) for line in report.readlines())

def _handle_pair(line):
    result = Result(**json.loads(line))
    print(_format_output(result))
    return result.encoded == result.decoded

def _format_output(result):
    return 'Test case: {} using encoding {}: {} with decoder {} = [{}], encoder {} = [{}]'.format(
        result.test_case,
        result.encoding,
        'passed' if result.decoded == result.encoded else 'failed',
        result.decoder,
        result.decoded,
        result.encoder,
        result.encoded)

def _print_summary(passed, overall):
    print('Ran {} test cases, {} passed, {} failed'.format(
        overall, passed, overall-passed))
    if overall == passed:
        print('Tests passed')
    else:
        print('Some of the tests failed')

def builder():
    return Builder(action=do_summary)
