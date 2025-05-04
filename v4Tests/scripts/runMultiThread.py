#!/usr/bin/env python3

import os
import sys
import itertools
import shutil
import getopt
import subprocess
import distutils.spawn as spawn
import multiprocessing
from threading import Thread
from queue import Queue

q = Queue()
errors = Queue()
final_results = []

def CreateACNFile(targetDir, content):
    str_start = "TEST-CASE DEFINITIONS ::= BEGIN\n"
    str_end = "END\n"
    f = open(targetDir + os.sep + "sample1.acn", 'w')
    f.write("-- Auto generated file\n\n")
    f.write(str_start)
    f.write("\t" + content + "\n")
    f.write(str_end)
    f.close()

def resolvesep():
    if sys.platform == 'cygwin':
        return "\\"
    else:
        return os.sep

def CreateACNContent(content):
    return "TEST-CASE DEFINITIONS ::= BEGIN\n" + "-- Auto generated file\n\n"  + "\t" + content + "\n" + "END\n"

def PrintFailed(mssg):
    print("\033[31m%-65s\033[0m" % (mssg))


def PrintSucceededAsExpected(mssg):
    print("\033[32m%-65s\033[0m" % (mssg))


def PrintWarning(mssg):
    print("\033[93m%-65s\033[0m" % (mssg))

def resolvedir(path):
    if sys.platform == 'cygwin':
        return "c:\\" + "\\".join(path.split("/")[3:])
    else:
        return path

def mysystem(targetDir, tmp_err, cmd, bCanFail):
    f = open(targetDir + os.sep + "log.txt", 'w')
    f.write(cmd + "\n")
    f.close()
    ret = subprocess.call(cmd, shell=True)
    if ret != 0 and not bCanFail:
        PrintFailed(cmd)
        #mysystem("cat tmp.err"+"_"+language, True)
        sys.exit(1)
    return ret

 

# behavior 0 :test case must pass
# behavior 1 :test case must fail in the asn1f.exe, with specific error message
# behavior 2 :test case must fail during execution of the generated executable
class AcnTestCase:
    def __init__(self, acnLine, acnContent, expectedErrNessage, behavior):
        self.acnLine = acnLine
        self.expectedErrNessage = expectedErrNessage
        self.acnContent = acnContent
        self.behavior = behavior
    def writeAcnContentToFile(self, targetDir):
        f = open(targetDir + os.sep + "sample1.acn", 'w')
        f.write(self.acnContent)
        f.close()


class TestCase:
    def __init__(self, language, rootDir, asn1file):
        self.language = language
        self.rootDir = rootDir
        self.asn1file = asn1file
        baseFileName = os.path.basename(asn1file)
        self.targetDir = rootDir + os.sep + "tmp0_" + language + os.sep + asn1file.split(os.sep)[2] + os.sep + os.path.splitext(baseFileName)[0]
        self.acnTestCases = []
        fnameASN = rootDir + os.sep + asn1file.strip()
        if not os.path.exists(fnameASN):
            print("File '" + fnameASN + "' does not exist! ")
            sys.exit(1)
        shutil.rmtree(self.targetDir, ignore_errors=True)
        os.makedirs(self.targetDir)
        shutil.copyfile(fnameASN, self.targetDir + os.sep + "sample1.asn1")
        f = open(fnameASN, 'r')
        lines = f.readlines()

        for line in lines:
            if line.find("--TCLS") == 0:
                tmp_line = line.split("--TCLS")[1].strip()
                self.acnTestCases.append(AcnTestCase(tmp_line, CreateACNContent(tmp_line), None, 0))
            elif line.find("--TCLFC") == 0:
                tmp_line = line.split("--TCLFC")[1].strip()
                tmp_err = tmp_line.split("$$$")[1].strip()
                tmp_line = tmp_line.split("$$$")[0].strip()
                self.acnTestCases.append(AcnTestCase(tmp_line, CreateACNContent(tmp_line), tmp_err, 1))
            elif line.find("--TCLFE") == 0:
                tmp_line = line.split("--TCLFE")[1].strip()
                tmp_err = tmp_line.split("$$$")[1].strip()
                tmp_line = tmp_line.split("$$$")[0].strip()
                self.acnTestCases.append(AcnTestCase(tmp_line, CreateACNContent(tmp_line), tmp_err, 2))
            elif line.find("--TCFS") == 0:
                testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
                tmp_line = line.split("--TCFS")[1].strip()
                acnContent = open(testCaseDir + os.sep + tmp_line, 'r').read()
                self.acnTestCases.append(AcnTestCase(tmp_line, acnContent, None, 0))
            elif line.find("--TCFFC") == 0:
                testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
                tmp_line = line.split("--TCFFC")[1].strip()
                tmp_err = tmp_line.split("$$$")[1].strip()
                tmp_line = tmp_line.split("$$$")[0].strip()
                acnContent = open(testCaseDir + os.sep + tmp_line, 'r').read()
                self.acnTestCases.append(AcnTestCase(tmp_line, acnContent, tmp_err, 1))
            elif line.find("--TCFFE") == 0:
                testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
                tmp_line = line.split("--TCFFE")[1].strip()
                tmp_err = tmp_line.split("$$$")[1].strip()
                tmp_line = tmp_line.split("$$$")[0].strip()
                acnContent = open(testCaseDir + os.sep + tmp_line, 'r').read()
                self.acnTestCases.append(AcnTestCase(tmp_line, acnContent, tmp_err, 2))
            else:
                continue

    def executeAcnTestCase(self, acnTestCase:AcnTestCase):
        print("Executing test cases for asn1 file: "+ self.asn1file + " acn line: " + acnTestCase.acnLine)
        acnTestCase.writeAcnContentToFile(self.targetDir)

        asn1File = self.targetDir + os.sep + "sample1.asn1"
        bRunCodeCoverage = "NOCOVERAGE" not in open(resolvedir(asn1File)).readline()
        acnFile = self.targetDir + os.sep + "sample1.acn"
        astXml  = self.targetDir + os.sep + "ast.xml"
        tmp_err = self.targetDir + os.sep + "tmp.err"
        #launcher = '' if sys.platform == 'cygwin' else 'mono '
        #path_to_asn1scc = spawn.find_executable('Asn1f4.exe')
        path_to_asn1scc = "../asn1scc/bin/Debug/net9.0/asn1scc"
        res = mysystem(self.targetDir, tmp_err,
            path_to_asn1scc +
            " -" + self.language + " -x '"+resolvedir(astXml)+"' -uPER -ACN -typePrefix gmamais_ " +
            "-renamePolicy 2 -fp AUTO " + "-equal -atc -o '" + resolvedir(self.targetDir) +
            "' '" + resolvedir(asn1File) + "' '" + resolvedir(acnFile) +
            "' 2>"+tmp_err, True)
        ferr = open(tmp_err, 'r')
        err_msg = ferr.read()
        ferr.close()
        behavior = acnTestCase.behavior
        targetDir = self.targetDir
        expErrMsg = acnTestCase.expectedErrNessage
        language = self.language
        if behavior == 0 or behavior == 2:
            if res != 0 or err_msg != "":
                PrintFailed("Asn.1 compiler failed")
                print("Asn.1 compiler error is: " + err_msg)
                #sys.exit(1)
                return 1
        else:
            err_msg = err_msg.replace("\r\n", "").replace("\n", "").replace(resolvedir(targetDir) +  resolvesep(), "")
            if res == 0 or err_msg != expErrMsg:
                PrintFailed(
                    "Asn.1 compiler didn't fail or failed with "
                    "different error message")
                print("Expected/current messages: ")
                print("'" + expErrMsg + "'")
                print("'" + err_msg + "'")
                #sys.exit(1)
                return 1
            else:
                return 0

        no_automatic_test_cases = "NO_AUTOMATIC_TEST_CASES" in open(asn1File, 'r').readlines()[0]
        if no_automatic_test_cases:
            if language == "c":
                res = mysystem(self.targetDir, tmp_err, "cd " + targetDir + os.sep + "; CC=gcc make", False)
                return 0
            else:
                res = mysystem(self.targetDir, tmp_err, "cd " + targetDir + os.sep + "; CC=gcc make", False)
                return 0

        if language == "c":
            try:
                res = mysystem(
                    self.targetDir, tmp_err, "cd " + targetDir + os.sep + "; CC=gcc make coverage", False)
                f = open(targetDir + os.sep + "sample1.c.gcov", 'r')
                lines = f.readlines()
                lines = filter(lambda x : "####" in x, lines)
                lines = filter(lambda x : "COVERAGE_IGNORE" not in x, lines)
                lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != '}', lines)
                lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "default:", lines)
                lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "break;", lines)
                lines = list(lines)
                if bRunCodeCoverage and len(lines) > 0:
                    PrintFailed("coverage failed. (less than 100%)")
                    return 1
                return 0
            except FileNotFoundError as err:
                PrintFailed(err)
                return 1
        else:
            #prevDir = os.getcwd()
            #os.chdir(targetDir)
            res = mysystem(self.targetDir, tmp_err, "cd " + targetDir + os.sep + "; make coverage >covlog.txt 2>&1", False)
            if res != 0 and behavior != 2:
                PrintFailed("run time failure")
                PrintFailed("covlog.txt is ...")
                #mysystem("cat covlog.txt",False)
                #sys.exit(1)
                return 1
            elif behavior == 2 and res == 2:
                PrintSucceededAsExpected("Test cases failed at run-time as expected")
                return 0
            elif behavior == 2 and res == 0:
                PrintFailed("ERROR: Executable didn't fail as it was expected to do...")
                #sys.exit(1)
                return 1
            elif behavior == 0 and res == 0:
                # -- NOCOVERAGE
                doCoverage = "--NOCOVERAGE" not in open(asn1File, 'r').readlines()[0]
                if doCoverage:
                    try:
                        f = open(targetDir + os.sep + "obj_x86" + os.sep + "debug" + os.sep + "test_case.adb.gcov", 'r')
                        lines = f.readlines()
                        lines = filter(lambda x : "####" in x, lines)
                        lines = filter(lambda x : "COVERAGE_IGNORE" not in x, lines)
                        lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != 'end;', lines)
                        lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "default:", lines)
                        lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "break;", lines)
                        lines = list(lines)
                        if bRunCodeCoverage and len(lines) > 0:
                            PrintWarning("coverage failed. (less than 100%)")
                            #sys.exit(1)
                            return 1
                        return 0
                    except FileNotFoundError as err:
                        PrintFailed(err)
                        #sys.exit(1)
                        return 1
                return 0
            else:
                print(res, behavior)
                PrintWarning("BUG in python script, Unexpected combination of res, behavior")
                return 1
            #os.chdir(prevDir)





    
    def executeTestCases(self):
        print("Executing test cases for asn1 file: "+ self.asn1file)
        for acnTestCase in self.acnTestCases:
            if errors.empty():
                ret = self.executeAcnTestCase(acnTestCase)
                if ret != 0:
                    return ret
        return 0



    def __str__(self):
        return self.asn1file        
    def __repr__(self):
        return 'TC asn1File=' + self.asn1file + ' targetDir=' +self.targetDir        

def produceTestCases(rootDir, language, testCaseStart=""):
    testCaseSet = rootDir + os.sep + "test-cases" + os.sep + "acn"
    for curDir in sorted(os.listdir(testCaseSet)):
        asn1files = [
            x
            for x in sorted(os.listdir(testCaseSet + os.sep + curDir))
            if x.endswith(".asn1")]

        for asn1file in asn1files:
            relAsn1Path = "test-cases" + os.sep + "acn" + os.sep + curDir + os.sep + asn1file
            if testCaseStart != "" and relAsn1Path < testCaseStart:
                print ("skiping test case :" + relAsn1Path)
            else:
                tc = TestCase(language,rootDir, relAsn1Path)
                q.put(tc)
        


def consumer():
    while True:
        tc = q.get()
        if errors.empty():
            errorCode = tc.executeTestCases()
            if errorCode != 0:
                errors.put(errorCode)
        q.task_done()
    
def usage():
    print("Usage: ", sys.argv[0], " <options>")
    print("where <options> are:")
    print("Mandatory:")
    print("     -l, --lang  <language_name>")
    print("           where <language_name> is c or Ada")
    print("Optional:")
    print("     -t, --testCaseSet  <asn1File>")
    sys.exit(1)

   
def main():
    if len(sys.argv) == 1:
        usage()

    try:
        args = sys.argv[1:]
        optlist, args = getopt.gnu_getopt(
            args, "al:t:", ['all', 'lang=', 'testCaseSet='])
    except:
        usage()
    if args != []:
        print("Invalid arguments: ", args)
        usage()
    
    rootDir = os.path.abspath(os.path.dirname(os.path.abspath(sys.argv[0])) + os.sep + "..")

    print ("Cores used :" + str(multiprocessing.cpu_count()))

    for _ in range(multiprocessing.cpu_count()):
        t = Thread(target=consumer)
        t.daemon = True
        t.start()

    lang = ""
    testCaseStart = ""
    bAll = False
    for opt, arg in optlist:
        if opt in ("-a", "--all"):
            bAll = True
        elif opt in ("-l", "--lang"):
            lang = arg
        elif opt in ("-t", "--testCase"):
            testCaseStart = arg

    if bAll:
        produceTestCases(rootDir, "Ada")
        produceTestCases(rootDir, "c")
    else:
        if lang not in ["c", "Ada"]:
            print("Invalid language argument")
            usage()

        if testCaseStart != "" and not os.path.exists(testCaseStart):
            print("File '" + testCaseStart + "' not found.")
            usage()
        #if lang.lower() == "c":
        #    os.putenv("PATH", "/usr/bin:" + os.getenv("PATH"))

        produceTestCases(rootDir, lang, testCaseStart)




    q.join()

    PrintSucceededAsExpected("OK ...")

if __name__ == "__main__":
    main()
