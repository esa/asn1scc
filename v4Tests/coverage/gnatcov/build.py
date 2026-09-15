"""Build C-enabled GNATcoverage using the release-matched upstream recipes."""
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tarfile
import urllib.request

ROOT = Path('/build')
LOCK = json.loads(Path(__file__).with_name('sources.json').read_text())
FSF = ROOT / 'fsf'


def run(command, cwd=ROOT, **kwargs):
    print('RUN', command, flush=True)
    subprocess.run(command, cwd=cwd, check=True, **kwargs)


def fetch(url, target, expected):
    target.parent.mkdir(parents=True, exist_ok=True)
    with urllib.request.urlopen(url, timeout=120) as response:
        data = response.read()
    if hashlib.sha256(data).hexdigest() != expected:
        raise ValueError('Source checksum mismatch: ' + url)
    target.write_bytes(data)


def replace_once(text, old, new):
    if text.count(old) != 1:
        raise ValueError('Unexpected upstream recipe: ' + old)
    return text.replace(old, new)


def main():
    run(['git', 'init', str(FSF)])
    run(['git', 'remote', 'add', 'origin', 'https://github.com/alire-project/GNAT-FSF-builds.git'], FSF)
    run(['git', 'fetch', '--depth', '1', 'origin', LOCK['fsf_recipes_commit']], FSF)
    run(['git', 'checkout', '--detach', 'FETCH_HEAD'], FSF)
    archive = ROOT / 'bindings.tar.gz'
    fetch('https://api.github.com/repos/AdaCore/llvm-bindings/tarball/' + LOCK['bindings_commit'],
          archive, LOCK['bindings_archive_sha256'])
    bindings = ROOT / 'llvm-bindings-16'
    with tarfile.open(archive) as stream:
        for member in stream:
            if not member.isfile():
                continue
            path = Path(*Path(member.name).parts[1:])
            destination = (bindings / path).resolve()
            if not destination.is_relative_to(bindings.resolve()):
                raise ValueError('Invalid archive member')
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(stream.extractfile(member).read())
    source = ROOT / 'clang16/libclang'
    for name, digest in LOCK['clang_api_files'].items():
        fetch('https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-' + LOCK['llvm_version']
              + '/clang/tools/libclang/' + name, source / name, digest)

    # The upstream downloader writes builder.name but checks builder.filename.
    # GitHub archives from different projects can also have the same basename.
    driver = FSF / 'lib/anod/build.py'
    driver.write_text(driver.read_text().replace('os.path.join(cache_dir, builder.filename',
                                                'os.path.join(cache_dir, builder.name'))
    recipe = FSF / 'specs/gnatcov.anod'
    text = replace_once(recipe.read_text(), 'm.set_var("C_SUPPORT", "False")',
        'm.set_var("C_SUPPORT", "True")\n'
        '            m.set_var("CXXFLAGS", "-I/usr/lib/llvm-16/include -include cstdint")\n'
        '            m.set_var("CLANG_LIBS", "/build/clang16/libclang.a " + '
        '" ".join(__import__("glob").glob("/usr/lib/llvm-16/lib/libclang*.a")))')
    text = replace_once(text,
        '"https://github.com/AdaCore/gnatcoverage/archive/refs/heads/%s.zip"\n                % self.version',
        '"https://github.com/AdaCore/gnatcoverage/archive/' + LOCK['gnatcoverage_commit'] + '.zip"')
    recipe.write_text(text)
    stable = FSF / 'specs/stable_sloc.anod'
    text = stable.read_text()
    if text.count('-XC_SUPPORT=False') != 2:
        raise ValueError('Unexpected stable_sloc recipe')
    stable.write_text(text.replace('-XC_SUPPORT=False', '-XC_SUPPORT=True')
                      .replace('"gpr_stubs"', '"/build/clang-bindings-install/share/gpr"'))
    os.environ['PATH'] = '/usr/lib/llvm-16/bin:' + os.environ['PATH']
    os.environ['CPLUS_INCLUDE_PATH'] = str(ROOT / 'clang16')
    run(['./anod', 'build', 'gprbuild', '--sandbox-dir=/build/sandbox', '--nocolor'], FSF)
    env = os.environ.copy()
    env['PATH'] = ('/build/sandbox/x86_64-linux/base_gcc/install/bin:'
                   '/build/sandbox/x86_64-linux/gprbuild/install/bin:' + env['PATH'])
    project = str(bindings / 'clang-bindings/libclang.gpr')
    run(['gprbuild', '-p', '-P' + project, '-j8', '-XLIBRARY_TYPE=static'], env=env)
    run(['gprinstall', '-p', '-f', '-P' + project, '-XLIBRARY_TYPE=static',
         '--prefix=/build/clang-bindings-install'], env=env)
    objects = ROOT / 'clang16-obj'
    objects.mkdir()

    def compile_api(path):
        target = objects / (path.stem + '.o')
        with (objects / (path.stem + '.log')).open('w') as log:
            run(['g++-13', '-std=c++17', '-O2', '-fPIC', '-DNDEBUG', '-DCINDEX_NO_EXPORTS',
                 '-I/usr/lib/llvm-16/include', '-I/build/clang16', '-include', 'cstdint',
                 '-c', str(path), '-o', str(target)], stdout=log, stderr=subprocess.STDOUT)
        return str(target)

    with ThreadPoolExecutor(max_workers=4) as pool:
        compiled = list(pool.map(compile_api, sorted(source.glob('*.cpp'))))
    run(['ar', 'rcs', '/build/clang16/libclang.a', *compiled])
    os.environ['GPR_PROJECT_PATH'] = '/build/clang-bindings-install/share/gpr'
    run(['./anod', 'build', 'gnatcov', '--sandbox-dir=/build/sandbox', '--nocolor'], FSF)
    installed = ROOT / 'sandbox/x86_64-linux/gnatcov/install'
    run([str(installed / 'bin/gnatcov'), '--version'])
    manifest = {'sources': LOCK, 'C_SUPPORT': True,
                'packages': subprocess.check_output(['dpkg-query', '-W'], text=True),
                'archives_sha256': {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                    for p in (ROOT / 'sandbox/tmp/cache').iterdir() if p.is_file() and p.suffix != '.sha1'}}
    (installed / 'toolchain-build.json').write_text(json.dumps(manifest, indent=2) + '\n')


if __name__ == '__main__':
    main()
