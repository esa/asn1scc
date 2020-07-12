#!/usr/bin/env python3

import os
import sys
import itertools
import shutil
import getopt
import subprocess
import distutils.spawn as spawn

# Globals

rootDir = None
language = None
targetDir = None
nTests = None


def CreateACNFile(content):
    str_start = "TEST-CASE DEFINITIONS ::= BEGIN\n"
    str_end = "END\n"
    f = open(targetDir + os.sep + "sample1.acn", 'w')
    f.write("-- Auto generated file\n\n")
    f.write(str_start)
    f.write("\t" + content + "\n")
    f.write(str_end)
    f.close()


def mysystem(cmd, bCanFail):
    f = open(language+"_log.txt", 'a')
    f.write(cmd + "\n")
    f.close()
    ret = subprocess.call(cmd, shell=True)
    if ret != 0 and not bCanFail:
        PrintFailed(cmd)
        mysystem("cat tmp.err"+"_"+language, True)
        sys.exit(1)
    return ret


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
    print("\033[31m%-65s\033[0m" % (mssg))


def PrintSucceededAsExpected(mssg):
    print("\033[32m%-65s\033[0m" % (mssg))


def PrintWarning(mssg):
    print("\033[93m%-65s\033[0m" % (mssg))


	
# behavior 0 :test case must pass
# behavior 1 :test case must fail in the asn1f.exe, with specific error message
# behavior 2 :test case must fail during execution of the generated executable
def RunTestCase(asn1, acn, behavior, expErrMsg):
    global nTests

    print(asn1, acn)

    asn1File = targetDir + os.sep + "sample1.asn1"
    bRunCodeCoverage = "NOCOVERAGE" not in open(resolvedir(asn1File)).readline()
    acnFile = targetDir + os.sep + "sample1.acn"
    astXml  = targetDir + os.sep + "ast.xml"
    launcher = '' if sys.platform == 'cygwin' else 'mono '
    path_to_asn1scc = spawn.find_executable('Asn1f4.exe')
    res = mysystem(
        launcher + path_to_asn1scc +
        " -" + language + " -x ast.xml -uPER -XER -typePrefix gmamais_ " +
        "-renamePolicy 2 -fp AUTO " + "-equal -atc -o '" + resolvedir(targetDir) +
        "' '" + resolvedir(asn1File) +
        "' 2>tmp.err"+"_"+language, True)
    ferr = open("tmp.err"+"_"+language, 'r')
    #print("str to replace '" + resolvedir(targetDir) + resolvesep() + "'")
    err_msg = ferr.read().replace("\r\n", "").replace("\n", "").replace(resolvedir(targetDir) +  resolvesep(), "")
    ferr.close()
    if res != 0 or err_msg != "":
        PrintFailed("Asn.1 compiler failed")
        print("Asn.1 compiler error is: " + err_msg)
        sys.exit(1)

    #no_automatic_test_cases = "NO_AUTOMATIC_TEST_CASES" in open(asn1File, 'r').readlines()[0]
    no_automatic_test_cases = True
    if no_automatic_test_cases:
        res = mysystem("cd " + targetDir + os.sep + "; CC=gcc make", False)
        return

    #if language == "c":
    #    try:
    #        res = mysystem(
    #            "cd " + targetDir + os.sep + "; CC=gcc make coverage", False)
    #        f = open(targetDir + os.sep + "sample1.c.gcov", 'r')
    #        lines = f.readlines()
    #        lines = filter(lambda x : "####" in x, lines)
    #        lines = filter(lambda x : "COVERAGE_IGNORE" not in x, lines)
    #        lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != '}', lines)
    #        lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "default:", lines)
    #        lines = filter(lambda l : ":".join(l.split(":")[2:]).strip() != "break;", lines)
    #        lines = list(lines)
    #        if bRunCodeCoverage and len(lines) > 0:
    #            PrintWarning("coverage failed. (less than 100%)")
    #            #sys.exit(1)
    #    except FileNotFoundError as err:
    #        pass;
    #else:
    #    prevDir = os.getcwd()
    #    os.chdir(targetDir)
    #    res = mysystem("make coverage >covlog.txt 2>&1", True)
    #    if res != 0 and behavior != 2:
    #        PrintFailed("run time failure")
    #        PrintFailed("covlog.txt is ...")
    #        mysystem("cat covlog.txt",False)
    #        sys.exit(1)
    #    elif behavior == 2 and res == 2:
    #        PrintSucceededAsExpected(
    #            "Test cases failed at run-time as expected")
    #    elif behavior == 2 and res == 0:
    #        PrintFailed(
    #            "ERROR: Executable didn't fail as it was expected to do...")
    #        sys.exit(1)
    #    elif behavior == 0 and res == 0:
    #        # -- NOCOVERAGE
    #        doCoverage = "-- NOCOVERAGE" not in open("sample1.asn1", 'r').readlines()[0]
    #        if doCoverage:

    #            def hunt_signature(l):
    #                return "test_case.adb" not in l and "mymod.adb" not in l
    #            lines = list(
    #                itertools.dropwhile(
    #                    hunt_signature, open("covlog.txt", 'r').readlines()))
    #            lines = list(itertools.dropwhile(hunt_signature, lines[1:]))
    #            excecLines = [l for l in list(lines) if "executed" in l]
    #            #print (excecLines)
    #            if excecLines:
    #                excLine = excecLines[0]
    #                if "executed:100.00" not in excLine:
    #                    PrintWarning("coverage error (less than 100%): {}".format('\n'.join(lines)))
    #                    #sys.exit(-1)
    #                else:
    #                    PrintSucceededAsExpected(excLine)
    #            else:
    #                PrintWarning("No line executed !!!: {}".format('\n'.join(lines)))
    #    else:
    #        print(res, behavior)
    #        PrintWarning(
    #            "BUG in python script, Unexpected combination "
    #            "of res, behavior")
    #    os.chdir(prevDir)
    nTests += 1


def DoWork_XER(asn1file):
    print(language, "XER", asn1file)

    fnameASN = asn1file.strip()
    if not os.path.exists(fnameASN):
        print("File '" + fnameASN + "' does not exist! ")
        sys.exit(1)

    shutil.rmtree(targetDir, ignore_errors=True)
    os.mkdir(targetDir)
    testCaseDir = os.path.dirname(os.path.abspath(fnameASN))
    shutil.copyfile(fnameASN, targetDir + os.sep + "sample1.asn1")
    RunTestCase(os.sep.join(asn1file.split(os.sep)[-2:]), "", 0, "")



def GetBehavior(asn1File):
    if asn1File.find("FAIL") != -1:
        f = open(asn1File, 'r')
        line = f.readlines()[0]
        if line.find("$$$") != -1:
            errCode = int(line.split("$$$")[1])
            errMsg = line.split("$$$")[2].strip()
            errMsg = errMsg.replace("\r\n", "").replace("\n", "")
            return (errCode, errMsg)
    return (0, "")



def submain(lang, encoding, testCaseSet, cntTest):
    global language, targetDir

    language = lang
    tmpDir = "tmp_" + lang
    targetDir = rootDir + os.sep + tmpDir

    if os.path.exists(tmpDir):
        shutil.rmtree(tmpDir)
    os.mkdir(tmpDir)
    
    testCaseStart = testCaseSet
    if testCaseSet == "" or cntTest:
        testCaseSet = rootDir + os.sep + "test-cases" + os.sep + "acn"

    funcName = "DoWork_" + encoding
    if os.path.isfile(testCaseSet):
        globals()[funcName](os.path.abspath(testCaseSet))
    else:
        #relAsn1Path = "test-cases" + os.sep + "acn" + os.sep + asn1file
        #print ("testCaseStart is :" + testCaseStart)
        for curDir in sorted(os.listdir(testCaseSet)):
            if curDir.find('.svn') != -1:
                continue
            asn1files = [
                x
                for x in sorted(os.listdir(testCaseSet + os.sep + curDir))
                if x.endswith(".asn1")]
            for asn1file in asn1files:
                relAsn1Path = "test-cases" + os.sep + "acn" + os.sep + curDir + os.sep + asn1file
                if cntTest and testCaseStart != "" and relAsn1Path < testCaseStart:
                    print ("skiping test case :" + relAsn1Path)
                else:
                    globals()[funcName](
                        os.path.abspath(
                            testCaseSet + os.sep + curDir + os.sep + asn1file))


def usage():
    print("Usage: ", sys.argv[0], " <options>")
    print("where <options> are:")
    print("Mandatory:")
    print("     -l, --lang  <language_name>")
    print("           where <language_name> is c or Ada")
    print("Optional:")
    print("     -t, --testCaseSet  <asn1File> or <testcaseDir>")
    sys.exit(1)


def main():
    global rootDir, nTests

    rootDir = os.path.abspath(
        os.path.dirname(os.path.abspath(sys.argv[0])) + os.sep + "..")
    nTests = 0

    if len(sys.argv) == 1:
        usage()

    try:
        args = sys.argv[1:]
        optlist, args = getopt.gnu_getopt(
            args, "al:t:c", ['all', 'lang=', 'testCaseSet=','cntTest'])
    except:
        usage()
    if args != []:
        print("Invalid arguments: ", args)
        usage()

    lang = ""
    testCaseSet = ""
    bAll = False
    cntTest = False
    for opt, arg in optlist:
        if opt in ("-a", "--all"):
            bAll = True
        elif opt in ("-l", "--lang"):
            lang = arg
        elif opt in ("-t", "--testCase"):
            testCaseSet = arg
        elif opt in ("-c", "--cntTest"):
            cntTest = True
    if bAll:
        f = open(language+"_log.txt", 'a')
        f.write("==========================================\n")
        f.close()
        submain("c", "XER", "", cntTest)
        submain("Ada", "XER", "", cntTest)
    else:
        if lang not in ["c", "Ada"]:
            print("Invalid language argument")
            usage()

        if testCaseSet != "" and not os.path.exists(testCaseSet):
            print("File '" + testCaseSet + "' not found.")
            usage()
        if lang.lower() == "c":
            os.putenv("PATH", "/usr/bin:" + os.getenv("PATH"))

        #f = open(language+"_log.txt", 'a')
        #f.write("==========================================\n")
        #f.close()
        submain(lang, "XER", testCaseSet, cntTest)
    print("Test run ended succesfully. Number of test cases run :", nTests)


if __name__ == "__main__":
    main()
