"""Check image routing, isolation arguments and failure-artifact preservation."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

RUNNER = Path(__file__).with_name('runCoverage.sh')
FAKE_DOCKER = '''#!/usr/bin/env python3
import json, os, sys
from pathlib import Path
args = sys.argv[1:]
with open(os.environ['DOCKER_CALLS'], 'a') as stream:
    stream.write(json.dumps(args) + '\\n')
if args[0] == 'create':
    print('test-container-id')
elif args[0] == 'inspect':
    code = int(os.environ.get('CONTAINER_EXIT', '0'))
    if 'json' in args[2]:
        print(json.dumps({'image':'sha256:test', 'exit_code':code}))
    else:
        print('exited', code)
elif args[0] == 'cp':
    target = Path(args[-1])
    target.mkdir(parents=True)
    (target / 'evidence.json').write_text('{}')
'''


class DockerRunnerTests(unittest.TestCase):
    def invoke(self, root, options, exit_code=0):
        binary = root / 'docker'
        binary.write_text(FAKE_DOCKER)
        binary.chmod(0o755)
        log = root / 'calls.jsonl'
        env = {**os.environ, 'PATH': str(root) + os.pathsep + os.environ['PATH'],
               'DOCKER_CALLS': str(log), 'CONTAINER_EXIT': str(exit_code)}
        result = subprocess.run(['bash', str(RUNNER), '--outdir', str(root / 'output'), *options],
                                env=env, text=True, capture_output=True, timeout=30)
        calls = [json.loads(x) for x in log.read_text().splitlines()] if log.exists() else []
        return result, calls

    def test_routes_language_and_metric_to_isolated_images(self):
        for metric, language, expected in [('gcov', 'c', 'local'), ('gcov', 'Ada', 'local'),
                                           ('stmt', 'c', 'statement-c'), ('stmt', 'Ada', 'statement-ada')]:
            with self.subTest(metric=metric, language=language), tempfile.TemporaryDirectory() as temp:
                root = Path(temp)
                result, calls = self.invoke(root, ['--metric', metric, '--language', language,
                                                   '--', '--filter', 'case with spaces'])
                self.assertEqual(result.returncode, 0, result.stderr)
                create = calls[0]
                self.assertIn('asn1scc-coverage:' + expected, create)
                self.assertIn('--pull=never', create)
                self.assertEqual(create[create.index('--network') + 1], 'none')
                self.assertEqual(create[create.index('--user') + 1], '10001:10001')
                self.assertNotIn('--volume', create)
                self.assertNotIn('--mount', create)
                self.assertEqual(create[-1], 'case with spaces')
                self.assertTrue((root / 'output/results/evidence.json').exists())

    def test_container_failure_is_authoritative_and_artifacts_survive(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            result, calls = self.invoke(root, ['--metric', 'stmt'], exit_code=7)
            self.assertEqual(result.returncode, 7)
            self.assertTrue((root / 'output/results/evidence.json').exists())
            self.assertFalse(any(c[0] == 'rm' for c in calls))

    def test_paired_pilot_routes_to_its_own_entrypoint(self):
        for metric, script in [('gcov', 'encodePilot.py'), ('stmt', 'statementPilot.py')]:
            with self.subTest(metric=metric), tempfile.TemporaryDirectory() as temp:
                result, calls = self.invoke(Path(temp), ['--metric', metric, '--encode-pilot'])
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn('/opt/coverage/' + script, calls[0])

    def test_invalid_or_conflicting_scope_is_rejected_before_docker(self):
        for options in [['--metric', 'unknown'], ['--language', 'Ada', '--encode-pilot'],
                        ['--language', 'Ada', '--decode-pilot'], ['--encode-pilot', '--decode-pilot'],
                        ['--language', 'Ada', '--invalid-value-pilot'],
                        ['--encode-pilot', '--invalid-value-pilot'], ['--decode-pilot', '--invalid-value-pilot'],
                        ['--', '--language', 'Ada'], ['--', '--outdir=/tmp/elsewhere']]:
            with self.subTest(options=options), tempfile.TemporaryDirectory() as temp:
                result, calls = self.invoke(Path(temp), options)
                self.assertEqual(result.returncode, 2)
                self.assertEqual(calls, [])

    def test_decode_pilot_routes_both_metrics_and_explicit_image(self):
        for metric in ('gcov', 'stmt'):
            with self.subTest(metric=metric), tempfile.TemporaryDirectory() as temp:
                result, calls = self.invoke(Path(temp), ['--metric', metric, '--decode-pilot',
                                                         '--image', 'decode-test-image'])
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn('/opt/coverage/decodePilot.py', calls[0])
                self.assertIn('decode-test-image', calls[0])
                self.assertEqual(calls[0][-2:], ['--metric', metric])

    def test_invalid_value_pilot_routes_both_metrics(self):
        for metric in ('gcov', 'stmt'):
            with self.subTest(metric=metric), tempfile.TemporaryDirectory() as temp:
                result, calls = self.invoke(Path(temp), ['--metric', metric, '--invalid-value-pilot'])
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn('/opt/coverage/invalidValuePilot.py', calls[0])
                self.assertEqual(calls[0][-2:], ['--metric', metric])


if __name__ == '__main__':
    unittest.main(verbosity=2)
