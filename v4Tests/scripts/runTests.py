#!/usr/bin/env python3

import os
import sys
import shutil
import getopt
import subprocess
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import List, Optional

os.environ["DOTNET_ROOT"] = os.environ.get("DOTNET_ROOT", "/home/maxime/.dotnet")

# Thread-safe printing
_print_lock = threading.Lock()

def safe_print(*args, **kwargs):
    with _print_lock:
        print(*args, **kwargs)


@dataclass
class TestConfig:
    """Encapsulates all configuration that was previously global mutable state."""
    language: str
    rootDir: str
    slim: str = ""
    acnV2: str = ""
    icdPdus: str = ""
    xerMode: bool = False
    path_to_asn1scc: str = ""
    jobs: int = 1
    workDir: Optional[str] = None  # user-specified override for base working dir


@dataclass
class WorkItem:
    """A single test case to be executed by a worker thread."""
    asn1_label: str       # e.g. "01-INTEGER/009.asn1"
    acn_label: str        # the ACN test name (inline content or filename)
    behavior: int         # 0=must pass, 1=must fail at compile, 2=must fail at runtime
    expErrMsg: str
    targetDir: str        # per-file working directory
    asn1file_path: str    # absolute path to the original .asn1 file
    acn_content: str      # ACN content to write to sample1.acn (empty if acn_file is set)
    acn_file: str = ""    # absolute path to an external .acn file to copy (alternative to acn_content)


class TestRunResults:
    """Thread-safe accumulator for test results."""
    def __init__(self):
        self._lock = threading.Lock()
        self.errors: List[str] = []
        self.nTests = 0

    def add_error(self, msg: str):
        with self._lock:
            self.errors.append(msg)

    def add_pass(self):
        with self._lock:
            self.nTests += 1

    def get_errors(self) -> List[str]:
        with self._lock:
            return list(self.errors)

    def get_nTests(self) -> int:
        with self._lock:
            return self.nTests


# Global results — only written through thread-safe methods
results = TestRunResults()


def resolvedir(path):
    if sys.platform == 'cygwin':
        return "c:\\" + "\\".join(path.split("/")[3:])
    else:
        return path

def resolvesep():
    if sys.platform == 'cygwin':
        return "\\"
    else:
        return os.sep

def PrintFailed(mssg):
    safe_print("\033[31m%-65s\033[0m" % (mssg))

def PrintSucceededAsExpected(mssg):
    safe_print("\033[32m%-65s\033[0m" % (mssg))

def PrintWarning(mssg):
    safe_print("\033[93m%-65s\033[0m" % (mssg))


def mysystem(cfg: TestConfig, targetDir: str, cmd: str, bCanFail: bool, asn1_label: str = "", acn_label: str = ""):
    """Execute a shell command, logging to <targetDir>/log.txt.

    Thread-safe: each call writes to the per-test-case directory, not a shared log file.
    Does NOT use os.chdir — pass cwd=targetDir to subprocess.call instead.
    """
    log_path = os.path.join(targetDir, "log.txt")
    with open(log_path, 'a') as f:
        f.write(cmd + "\n")
    ret = subprocess.call(cmd, shell=True, cwd=targetDir)
    if ret != 0 and not bCanFail:
        PrintFailed(cmd)
        results.add_error(f'Failed {asn1_label} {acn_label} in {cfg.language}')
        raise Exception('TestFailed')
    return ret


def CreateACNFile(targetDir: str, content: str):
    str_start = "TEST-CASE DEFINITIONS ::= BEGIN\n"
    str_end = "END\n"
    with open(os.path.join(targetDir, "sample1.acn"), 'w') as f:
        f.write("-- Auto generated file\n\n")
        f.write(str_start)
        f.write("\t" + content + "\n")
        f.write(str_end)


# behavior 0 :test case must pass
# behavior 1 :test case must fail in the asn1f.exe, with specific error message
# behavior 2 :test case must fail during execution of the generated executable
def RunTestCase(cfg: TestConfig, item: WorkItem):
    """Execute a single test case. Thread-safe: all state comes from cfg and item."""
    asn1 = item.asn1_label
    acn = item.acn_label
    behavior = item.behavior
    expErrMsg = item.expErrMsg
    targetDir = item.targetDir
    language = cfg.language

    safe_print(asn1, acn)

    asn1File = os.path.join(targetDir, "sample1.asn1")
    bRunCodeCoverage = "NOCOVERAGE" not in open(resolvedir(asn1File)).readline()
    acnFile = os.path.join(targetDir, "sample1.acn")
    astXml  = os.path.join(targetDir, "ast.xml")

    path_to_asn1scc = cfg.path_to_asn1scc
    if cfg.xerMode:
        encodingFlags = " -XER "
        inputFiles = "'" + resolvedir(asn1File) + "'"
    else:
        encodingFlags = " -uPER -ACN "
        inputFiles = "'" + resolvedir(asn1File) + "' '" + resolvedir(acnFile) + "'"

    tmp_err_file = os.path.join(targetDir, "tmp.err")

    res = mysystem(
        cfg, targetDir,
        path_to_asn1scc +
        " -" + language + " -x ast.xml" + encodingFlags + "-ig -typePrefix ASN1SCC_ " + cfg.acnV2 + cfg.slim + cfg.icdPdus +
        "-renamePolicy 3 -fp AUTO " + "-equal -atc -o '" + resolvedir(targetDir) +
        "' " + inputFiles +
        " 2>'" + tmp_err_file + "'", True, asn1, acn)

    with open(tmp_err_file, 'r') as ferr:
        err_msg = ferr.read()

    if behavior == 0 or behavior == 2:
        if res != 0 or err_msg != "":
            PrintFailed("Asn.1 compiler failed")
            safe_print("Asn.1 compiler error is: " + err_msg)
            results.add_error(f'Failed {asn1} {acn} in {language}')
            raise Exception('TestFailed')
    else:
        err_msg = err_msg.replace("\r\n", "").replace("\n", "").replace(resolvedir(targetDir) + resolvesep(), "")
        if res == 0 or err_msg != expErrMsg:
            PrintFailed(
                "Asn.1 compiler didn't fail or failed with "
                "different error message")
            safe_print("Expected/current messages: ")
            safe_print("'" + expErrMsg + "'")
            safe_print("'" + err_msg + "'")
            results.add_error(f'Failed {asn1} {acn} in {language}')
            raise Exception('TestFailed')
        else:
            results.add_pass()
            return

    # For Ada: add 'pragma Style_Checks (Off);' to generated test_case_*.ads files
    # to suppress -gnaty style errors on auto-generated spec files.
    # The gprbuild coverage target uses -gnaty (style checks) which flags
    # indentation and spacing issues in generated test case specs.
    if language == 'Ada':
        import glob as _glob
        for ads in _glob.glob(os.path.join(targetDir, "test_case_*.ads")):
            with open(ads, 'r') as f:
                content = f.read()
            if 'pragma Style_Checks (Off)' not in content:
                with open(ads, 'w') as f:
                    f.write('pragma Style_Checks (Off);\n' + content)

    no_automatic_test_cases = "NO_AUTOMATIC_TEST_CASES" in open(asn1File, 'r').readlines()[0]
    if no_automatic_test_cases:
        if language == "c":
            res = mysystem(cfg, targetDir, "CC=gcc make", False, asn1, acn)
            return
        elif language == 'Ada':
            res = mysystem(cfg, targetDir, "CC=gcc make", False, asn1, acn)
            return
        elif language == 'Rust':
            res = mysystem(cfg, targetDir, "cargo build", False, asn1, acn)
            return
        elif language == 'python':
            return  # No compilation step for Python; asn1scc success is sufficient
        else:
            # Scala
            res = mysystem(cfg, targetDir, "sbt compile", False, asn1, acn)
            return

    if language == "c":
        try:
            res = mysystem(cfg, targetDir, "CC=gcc make coverage", False, asn1, acn)
            with open(os.path.join(targetDir, "sample1.c.gcov"), 'r') as f:
                lines = f.readlines()
            lines = filter(lambda x : "####" in x, lines)
            lines = filter(lambda x : "COVERAGE_IGNORE" not in x, lines)
            lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != '}', lines)
            lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "default:", lines)
            lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "break;", lines)
            lines = list(lines)
            if bRunCodeCoverage and len(lines) > 0:
                PrintWarning("coverage failed. (less than 100%)")
                results.add_error(f'Failed {asn1} {acn} in {language}')
                raise Exception('TestFailed')
        except FileNotFoundError as err:
            pass
    elif language == 'Rust':
        # Copy the Rust runtime (Cargo.toml + src/*.rs) into the test directory
        rustRuntimeSrc = os.path.join(cfg.rootDir, "..", "asn1rust")
        rustRuntimeDst = os.path.join(targetDir, "asn1rust")
        shutil.rmtree(rustRuntimeDst, ignore_errors=True)
        shutil.copytree(rustRuntimeSrc, rustRuntimeDst,
                        ignore=shutil.ignore_patterns('target', 'Cargo.lock'))
        # Create a Cargo.toml for the generated code that depends on the runtime
        cargoTomlContent = (
            "[package]\n"
            "name = \"asn1scc_test\"\n"
            "version = \"0.1.0\"\n"
            "edition = \"2021\"\n"
            "\n"
            "[dependencies]\n"
            "asn1rust = { path = \"asn1rust\" }\n"
            "\n"
            "[[bin]]\n"
            "name = \"mainprogram\"\n"
            "path = \"mainprogram.rs\"\n"
        )
        with open(os.path.join(targetDir, "Cargo.toml"), 'w') as f:
            f.write(cargoTomlContent)
        # Build the project
        res = mysystem(cfg, targetDir, "cargo build >covlog.txt 2>&1", True, asn1, acn)
        if res != 0 and behavior != 2:
            PrintFailed("compilation failure")
            PrintFailed("covlog.txt is ...")
            mysystem(cfg, targetDir, "cat covlog.txt", False, asn1, acn)
            results.add_error(f'Failed {asn1} {acn} in {language}')
            raise Exception('TestFailed')
        elif behavior == 2 and res != 0:
            PrintSucceededAsExpected(
                "Test cases failed at build-time as expected")
        else:
            # Run the generated test executable
            res = mysystem(cfg, targetDir, "cargo run >covlog.txt 2>&1", True, asn1, acn)
            if res != 0 and behavior != 2:
                PrintFailed("run time failure")
                PrintFailed("covlog.txt is ...")
                mysystem(cfg, targetDir, "cat covlog.txt", False, asn1, acn)
                results.add_error(f'Failed {asn1} {acn} in {language}')
                raise Exception('TestFailed')
            elif behavior == 2 and res != 0:
                PrintSucceededAsExpected(
                    "Test cases failed at run-time as expected")
            elif behavior == 2 and res == 0:
                PrintFailed(
                    "ERROR: Executable didn't fail as it was expected to do...")
                results.add_error(f'Failed {asn1} {acn} in {language}')
                raise Exception('TestFailed')
    elif language == 'Ada':
        makeTarget = "coverage" if bRunCodeCoverage else ""
        res = mysystem(cfg, targetDir, f"make {makeTarget} >covlog.txt 2>&1", True, asn1, acn)
        if res != 0 and behavior != 2:
            PrintFailed("run time failure")
            PrintFailed("covlog.txt is ...")
            mysystem(cfg, targetDir, "cat covlog.txt", False, asn1, acn)
            results.add_error(f'Failed {asn1} {acn} in {language}')
            raise Exception('TestFailed')
        elif behavior == 2 and res == 2:
            PrintSucceededAsExpected(
                "Test cases failed at run-time as expected")
        elif behavior == 2 and res == 0:
            PrintFailed(
                "ERROR: Executable didn't fail as it was expected to do...")
            results.add_error(f'Failed {asn1} {acn} in {language}')
            raise Exception('TestFailed')
        elif behavior == 0 and res == 0:
            # -- NOCOVERAGE
            doCoverage = "-- NOCOVERAGE" not in open(os.path.join(targetDir, "sample1.asn1"), 'r').readlines()[0]
            runSpark = "RUN_SPARK" in open(os.path.join(targetDir, "sample1.asn1"), 'r').readlines()[0]
            if doCoverage:
                gcov_path = os.path.join(targetDir, "obj_x86", "debug", "test_case.adb.gcov")
                try:
                    with open(gcov_path, 'r') as f:
                        lines = f.readlines()
                    lines = filter(lambda x : "####" in x, lines)
                    lines = filter(lambda x : "COVERAGE_IGNORE" not in x, lines)
                    lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != 'end;', lines)
                    lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != 'declare', lines)
                    lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "default:", lines)
                    lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "break;", lines)
                    lines = list(lines)
                    if bRunCodeCoverage and len(lines) > 0:
                        PrintWarning("coverage failed. (less than 100%)")
                        results.add_error(f'Failed {asn1} {acn} in {language}')
                        raise Exception('TestFailed')
                except FileNotFoundError as err:
                    safe_print("No file found at : " + gcov_path)
            if runSpark:
                res = mysystem(cfg, targetDir, "gnatprove -Pasn1_x86.gpr -j0 -u test_case.adb --level=4 >sparklog.txt 2>&1", True, asn1, acn)
                try:
                    with open(os.path.join(targetDir, "sparklog.txt"), 'r') as f:
                        lines = f.readlines()
                    lines = filter(lambda x : "might fail, cannot prove" in x, lines)
                    lines = list(lines)
                    if len(lines) > 0:
                        PrintWarning("Spark failed.")
                        mysystem(cfg, targetDir, "cat sparklog.txt", False, asn1, acn)
                        results.add_error(f'Failed {asn1} {acn} in {language}')
                        raise Exception('TestFailed')
                    else:
                        PrintSucceededAsExpected("Spark OK !!!")
                except FileNotFoundError as err:
                    pass
        else:
            safe_print(res, behavior)
            PrintWarning(
                "BUG in python script, Unexpected combination "
                "of res, behavior")
    elif language == 'python':
        pytest_bin = "uvx --python 3.11 pytest" if shutil.which("uvx") else "python3 -m pytest"
        pyCmd = pytest_bin
        ret = mysystem(cfg, targetDir, pyCmd, True, asn1, acn)
        if ret != 0 and ret != 5:  # exit code 5 = no tests collected, treat as success
            PrintFailed(pyCmd)
            results.add_error(f'Failed {asn1} {acn} in {language}')
            raise Exception('TestFailed')
    else:
        # Scala
        pass
    results.add_pass()


def collect_work_items_ACN(cfg: TestConfig, asn1file: str) -> List[WorkItem]:
    """Parse an ASN.1 file for TC* directives and produce WorkItems.

    Each WorkItem gets its own targetDir so it can run independently in parallel.
    In parallel mode (-j > 1), each ACN variant gets a unique subdirectory
    (e.g. tmp_Rust/01-INTEGER/009/tc0, tmp_Rust/01-INTEGER/009/tc1, ...).
    In sequential mode (-j 1), all variants share one directory (backward compatible).
    """
    items: List[WorkItem] = []

    fnameASN = asn1file.strip()
    if not os.path.exists(fnameASN):
        safe_print("File '" + fnameASN + "' does not exist! ")
        results.add_error(f'Failed {asn1file} in {cfg.language}')
        return items

    asn1_label = os.sep.join(asn1file.split(os.sep)[-2:])

    # Per-file base working directory: tmp_<lang>/<curDir>/<baseFileName>
    baseFileName = os.path.splitext(os.path.basename(asn1file))[0]
    curDir = os.path.basename(os.path.dirname(asn1file))
    if cfg.workDir:
        fileTargetDir = os.path.join(os.path.abspath(cfg.workDir), curDir, baseFileName)
    else:
        fileTargetDir = os.path.join(cfg.rootDir, f"tmp_{cfg.language}", curDir, baseFileName)

    with open(fnameASN, 'r') as f:
        lines = f.readlines()

    tc_index = 0
    for line in lines:
        # In parallel mode, give each ACN variant its own subdirectory
        if cfg.jobs > 1:
            itemTargetDir = os.path.join(fileTargetDir, f"tc{tc_index}")
            tc_index += 1
        else:
            itemTargetDir = fileTargetDir

        if line.find("--TCLS") == 0:
            tmp_line = line.split("--TCLS")[1].strip()
            items.append(WorkItem(
                asn1_label=asn1_label, acn_label=tmp_line, behavior=0, expErrMsg="",
                targetDir=itemTargetDir, asn1file_path=fnameASN,
                acn_content=tmp_line, acn_file=""))
        elif line.find("--TCLFC") == 0:
            tmp_line = line.split("--TCLFC")[1].strip()
            tmp_err = tmp_line.split("$$$")[1].strip()
            tmp_line = tmp_line.split("$$$")[0].strip()
            items.append(WorkItem(
                asn1_label=asn1_label, acn_label=tmp_line, behavior=1, expErrMsg=tmp_err,
                targetDir=itemTargetDir, asn1file_path=fnameASN,
                acn_content=tmp_line, acn_file=""))
        elif line.find("--TCLFE") == 0:
            tmp_line = line.split("--TCLFE")[1].strip()
            tmp_err = tmp_line.split("$$$")[1].strip()
            tmp_line = tmp_line.split("$$$")[0].strip()
            items.append(WorkItem(
                asn1_label=asn1_label, acn_label=tmp_line, behavior=2, expErrMsg=tmp_err,
                targetDir=itemTargetDir, asn1file_path=fnameASN,
                acn_content=tmp_line, acn_file=""))
        elif line.find("--TCFS") == 0:
            testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
            tmp_line = line.split("--TCFS")[1].strip()
            items.append(WorkItem(
                asn1_label=asn1_label, acn_label=tmp_line, behavior=0, expErrMsg="",
                targetDir=itemTargetDir, asn1file_path=fnameASN,
                acn_content="", acn_file=os.path.join(testCaseDir, tmp_line)))
        elif line.find("--TCFFC") == 0:
            testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
            tmp_line = line.split("--TCFFC")[1].strip()
            tmp_err = tmp_line.split("$$$")[1].strip()
            tmp_line = tmp_line.split("$$$")[0].strip()
            items.append(WorkItem(
                asn1_label=asn1_label, acn_label=tmp_line, behavior=1, expErrMsg=tmp_err,
                targetDir=itemTargetDir, asn1file_path=fnameASN,
                acn_content="", acn_file=os.path.join(testCaseDir, tmp_line)))
        elif line.find("--TCFFE") == 0:
            testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
            tmp_line = line.split("--TCFFE")[1].strip()
            tmp_err = tmp_line.split("$$$")[1].strip()
            tmp_line = tmp_line.split("$$$")[0].strip()
            items.append(WorkItem(
                asn1_label=asn1_label, acn_label=tmp_line, behavior=2, expErrMsg=tmp_err,
                targetDir=itemTargetDir, asn1file_path=fnameASN,
                acn_content="", acn_file=os.path.join(testCaseDir, tmp_line)))
        elif line.find("--TCBREAK") == 0:
            break
        else:
            continue

    return items


def collect_work_items_XER(cfg: TestConfig, asn1file: str) -> List[WorkItem]:
    """Collect a single XER work item for an ASN.1 file."""
    items: List[WorkItem] = []

    fnameASN = asn1file.strip()
    if not os.path.exists(fnameASN):
        safe_print("File '" + fnameASN + "' does not exist! ")
        results.add_error(f'Failed {asn1file} in {cfg.language}')
        return items

    asn1_label = os.sep.join(asn1file.split(os.sep)[-2:])

    baseFileName = os.path.splitext(os.path.basename(asn1file))[0]
    curDir = os.path.basename(os.path.dirname(asn1file))
    if cfg.workDir:
        fileTargetDir = os.path.join(os.path.abspath(cfg.workDir), curDir, baseFileName)
    else:
        fileTargetDir = os.path.join(cfg.rootDir, f"tmp_{cfg.language}", curDir, baseFileName)

    items.append(WorkItem(
        asn1_label=asn1_label, acn_label="(no acn)", behavior=0, expErrMsg="",
        targetDir=fileTargetDir, asn1file_path=fnameASN,
        acn_content="", acn_file=""))

    return items


def prepare_and_run_item(cfg: TestConfig, item: WorkItem):
    """Prepare the working directory for a single item, then run it.

    This function is the unit of work for the thread pool. It:
    1. Recreates the targetDir
    2. Copies sample1.asn1 into it
    3. Writes or copies the ACN file
    4. Calls RunTestCase
    """
    targetDir = item.targetDir

    # Recreate the working directory
    shutil.rmtree(targetDir, ignore_errors=True)
    os.makedirs(targetDir, exist_ok=True)
    shutil.copyfile(item.asn1file_path, os.path.join(targetDir, "sample1.asn1"))

    # Write or copy the ACN file
    if not cfg.xerMode:
        if item.acn_file:
            shutil.copyfile(item.acn_file, os.path.join(targetDir, "sample1.acn"))
        else:
            CreateACNFile(targetDir, item.acn_content)

    try:
        RunTestCase(cfg, item)
    except Exception:
        pass  # errors already recorded in results


def submain(cfg: TestConfig, testCaseSet: str, cntTest: bool):
    """Collect all work items and execute them (sequentially or in parallel)."""
    testCaseStart = testCaseSet
    if testCaseSet == "" or cntTest:
        if cfg.xerMode:
            tcSet = os.path.join(cfg.rootDir, "test-cases", "xer")
        else:
            tcSet = os.path.join(cfg.rootDir, "test-cases", "acn")
    else:
        tcSet = testCaseSet

    encoding = "XER" if cfg.xerMode else "ACN"

    all_items: List[WorkItem] = []

    if os.path.isfile(tcSet):
        if encoding == "XER":
            all_items = collect_work_items_XER(cfg, os.path.abspath(tcSet))
        else:
            all_items = collect_work_items_ACN(cfg, os.path.abspath(tcSet))
    else:
        for curDir in sorted(os.listdir(tcSet)):
            if not os.path.isdir(os.path.join(tcSet, curDir)):
                continue
            asn1files = [
                x
                for x in sorted(os.listdir(os.path.join(tcSet, curDir)))
                if x.endswith(".asn1")]
            for asn1file in asn1files:
                relAsn1Path = "test-cases" + os.sep + encoding.lower() + os.sep + curDir + os.sep + asn1file
                if cntTest and testCaseStart != "" and relAsn1Path < testCaseStart:
                    safe_print("skiping test case :" + relAsn1Path)
                else:
                    absPath = os.path.abspath(os.path.join(tcSet, curDir, asn1file))
                    if encoding == "XER":
                        all_items.extend(collect_work_items_XER(cfg, absPath))
                    else:
                        all_items.extend(collect_work_items_ACN(cfg, absPath))

    # Execute work items
    if cfg.jobs <= 1:
        # Sequential execution (backward-compatible)
        for item in all_items:
            prepare_and_run_item(cfg, item)
    else:
        # Parallel execution via thread pool
        safe_print(f"Running {len(all_items)} test cases with {cfg.jobs} threads...")
        with ThreadPoolExecutor(max_workers=cfg.jobs) as executor:
            futures = {executor.submit(prepare_and_run_item, cfg, item): item for item in all_items}
            for future in as_completed(futures):
                # Exceptions are caught inside prepare_and_run_item; this just waits
                future.result()


def usage():
    safe_print("Usage: ", sys.argv[0], " <options>")
    safe_print("where <options> are:")
    safe_print("Mandatory:")
    safe_print("     -l, --lang  <language_name>")
    safe_print("           where <language_name> is c, Ada, Scala, python, or Rust")
    safe_print("Optional:")
    safe_print("     -t, --testCaseSet  <asn1File> or <testcaseDir>")
    safe_print("     -s, --slim")
    safe_print("     --acn-v2          use ACN v2 deferred patching mode")
    safe_print("     --xer             run XER tests (uses -XER instead of -uPER -ACN)")
    safe_print("     -o, --output-dir <dir>")
    safe_print("           override the output/working directory (default: tmp_<lang>)")
    safe_print("     --icd-pdus <types>")
    safe_print("           comma-separated list of PDU type names (passed as -icdPdus to asn1scc)")
    safe_print("     -j N, --jobs N    number of parallel threads (default: 1 = sequential)")
    sys.exit(1)


def main():
    global results

    rootDir = os.path.abspath(
        os.path.dirname(os.path.abspath(sys.argv[0])) + os.sep + "..")
    results = TestRunResults()

    if len(sys.argv) == 1:
        usage()

    try:
        args = sys.argv[1:]
        optlist, args = getopt.gnu_getopt(
            args, "al:t:cso:j:", ['all', 'lang=', 'testCaseSet=', 'cntTest', 'slim',
                                   'acn-v2', 'xer', 'output-dir=', 'icd-pdus=', 'jobs='])
    except:
        usage()
    if args != []:
        safe_print("Invalid arguments: ", args)
        usage()

    lang = ""
    testCaseSet = ""
    bAll = False
    cntTest = False
    slim = ""
    acnV2 = ""
    icdPdus = ""
    xerMode = False
    workDir = None
    jobs = 1
    for opt, arg in optlist:
        if opt in ("-a", "--all"):
            bAll = True
        elif opt in ("-l", "--lang"):
            lang = arg
        elif opt in ("-t", "--testCaseSet", "--testCase"):
            testCaseSet = arg
        elif opt in ("-c", "--cntTest"):
            cntTest = True
        elif opt in ("-s", "--slim"):
            slim = " -slim "
        elif opt in ("--acn-v2",):
            acnV2 = " --acn-v2 "
        elif opt in ("--xer",):
            xerMode = True
        elif opt in ("-o", "--output-dir"):
            workDir = arg
        elif opt in ("--icd-pdus",):
            icdPdus = ' -icdPdus "' + arg + '" '
        elif opt in ("-j", "--jobs"):
            try:
                jobs = int(arg)
                if jobs < 1:
                    jobs = 1
            except ValueError:
                safe_print("Invalid -j value: " + arg)
                usage()

    path_to_asn1scc = os.path.abspath(
        rootDir + "/../asn1scc/bin/Debug/net10.0/linux-x64/publish/asn1scc")

    if bAll:
        for l in ["c", "Ada", "Scala", "Rust", "python"]:
            cfg = TestConfig(
                language=l, rootDir=rootDir, slim=slim, acnV2=acnV2,
                icdPdus=icdPdus, xerMode=xerMode,
                path_to_asn1scc=path_to_asn1scc, jobs=jobs, workDir=workDir)
            encoding = "XER" if xerMode else "ACN"
            submain(cfg, "", cntTest)
    else:
        if lang not in ["c", "Ada", 'Scala', 'python', 'Rust']:
            safe_print("Invalid language argument")
            usage()

        if testCaseSet != "" and not os.path.exists(testCaseSet):
            safe_print("File '" + testCaseSet + "' not found.")
            usage()
        if lang.lower() == "c":
            os.putenv("PATH", "/usr/bin:" + os.getenv("PATH"))
        if lang == "Rust":
            if shutil.which("cargo") is None:
                safe_print("Error: 'cargo' not found in PATH. Install Rust toolchain (rustup).")
                sys.exit(1)

        cfg = TestConfig(
            language=lang, rootDir=rootDir, slim=slim, acnV2=acnV2,
            icdPdus=icdPdus, xerMode=xerMode,
            path_to_asn1scc=path_to_asn1scc, jobs=jobs, workDir=workDir)

        submain(cfg, testCaseSet, cntTest)

    safe_print("Test run ended succesfully. Number of test cases run :", results.get_nTests())
    errs = results.get_errors()
    if errs:
        safe_print('\n'.join(errs))
        sys.exit(1)


if __name__ == "__main__":
    main()
