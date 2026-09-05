import json
import shutil
import os
import glob
import sys
import string

LIBRARY_PATH = os.path.abspath(".")
BUILD_PATH = os.path.join(LIBRARY_PATH, ".build", "dependency_check")
ASN1SOURCES_PATH = os.path.join(LIBRARY_PATH, ".build", "dependency_check", "asn1sources")
ERRORLOG_FILE = "errorlog.txt"

METADATA_FILENAME = "meta.json"
ASN_EXTENSION = ".asn"
ASN1_EXTENSION = ".asn1"
ACN_EXTENSION = ".acn"

SUBMODULES_KEY = "submodules"
ELEMENTS_KEY = "elements"
REQUIRES_KEY = "requires"
CONFLICTS_KEY = "conflicts"
NAME_KEY = "name"
MODULE_NAME_KEY = "module_name"
ASN1FILES_KEY = "asn1Files"
FROM_KEY = "from"

DEPENDENCY_ELEMENT_KEY = "element"
DEPENDENCY_SUBMODULE_KEY = "submodule"
DEPENDENCY_MODULE_KEY = "module"

MODULE_NAME_KEY = "module_name"
METADATA_KEY = "meta_json"
METADATA_PATH_KEY = "meta_path"

ASN1SCC = "asn1.exe"
ASN1SCC_ARGS = "--c-lang --acn-enc --field-prefix AUTO --type-prefix T"
CC = "gcc"
CC_ARGS = "-c -pipe -O2 -fPIC -w"
AR = "ar"
AR_ARGS = "cqs"
AR_OUT_PATH = BUILD_PATH

INTERNAL_REQUIRES_ERROR_MESSAGE = "Could not find required element inside module"
INTERNAL_CONFLICTS_ERROR_MESSAGE = "Could not find conflicting element inside module"
EXTERNAL_REQUIRES_ERROR_MESSAGE = "Could not find required element outside module"
EXTERNAL_CONFLICTS_ERROR_MESSAGE = "Could not find conflicting element outside module"

ASN1FILES_ERROR_MESSAGE = "Could not find ASN.1 file"
IMPORTS_ERROR_MESSAGE =  "Could not find module for import"
CODE_GENERATION_ERROR_MESSAGE = "C code generation failed"
COMPILATION_ERROR_MESSAGE = "Compilation failed"
LIBRARY_CREATION_ERROR_MESSAGE = "Library creation failed"

VALIDATION_FAILED_MESSAGE = "\033[1;31;49mMetadata validation failed\033[0;37;0m"
VALIDATION_PASSED_MESSAGE = "\033[1;32;49mMetadata validation passed\033[0;37;0m"

ERROR_CODE = 1
SUCCESS_CODE = 0

def logError(path, message, details):
    #filepath = os.path.join(BUILD_PATH, ERRORLOG_FILE)
    print("\033[1;37;49m%s: \033[1;31;49m%s: \033[0;37;49m%s\033[0;37;0m" % (path, message, details))
    #with open(filepath, "a+") as f:
    #    f.write("path" + ": " + message + ": " + details)


def getRecursiveDependency(element, submodule, module, metadata):
    dependencies = []
    for element_filename in element.get(ASN1FILES_KEY, []):
        dependencies.append(module.get(METADATA_PATH_KEY) + element_filename)

        acn_filename = module.get(METADATA_PATH_KEY) + os.path.splitext(element_filename)[0] + ACN_EXTENSION
        if os.path.exists(acn_filename):
            dependencies.append(acn_filename)
    
    for required in element.get(REQUIRES_KEY, []):
        if isinstance(required, str):
            dependencies += getRecursiveDependency(getElementByName(required, submodule), submodule, module, metadata)
        
        if isinstance(required, dict):
            dependencies += getRecursiveDependency(getElementByRequiresReference(required, metadata),   \
                                                   getSubmoduleByRequiresReference(required, metadata), \
                                                   getModuleByRequiresReference(required, metadata),    \
                                                   metadata)

    return dependencies


def makeValidFilename(filename):
    valid_characters = "_-.() " + string.ascii_letters + string.digits
    return "".join(c for c in filename.replace(" ", "-") if c in valid_characters)


def checkCCodeGenerationValid(element_name, metadata_path, files, path):
    asn1scc_command = ASN1SCC + " " +  ASN1SCC_ARGS + " -o " + path + " " + (" ".join(files))
    #print(asn1scc_command)

    if os.system(asn1scc_command) != 0:
        logError(metadata_path , CODE_GENERATION_ERROR_MESSAGE, element_name)
        return False
    else:
        return True 


def compileFile(src, path):
    cc_command = CC + " " +  CC_ARGS + " -I " + path + " " + src + " -o " + os.path.join(path, os.path.splitext((src))[0]) +  ".o"
    #print(cc_command)
    return os.system(cc_command)


def checkCompilationValid(element_name, metadata_path, path):
    return not [logError(metadata_path + ":" + element_name, COMPILATION_ERROR_MESSAGE, src) \
                for src in glob.glob(os.path.join(path, "*.c")) if compileFile(src, path) != 0]


def checkLibraryCreationValid(element_name, metadata_path, path):
    lib_filepath = os.path.join(path, "lib.a")
    ar_command = AR + " " + AR_ARGS + " " + lib_filepath  +  " ".join(glob.glob(os.path.join(path + "*.o")))
    #print(ar_command)

    if os.system(ar_command) != 0:
        logError(metadata_path + ":" + element_name, CODE_GENERATION_ERROR_MESSAGE, lib_filepath)
        return False
    else:
        return True


def checkElementCompilationValid(element, submodule, module, metadata, build_path):
    dependencies = set(getRecursiveDependency(element, submodule, module, metadata))
    path = os.path.join(build_path,                                  \
                        makeValidFilename(module.get(NAME_KEY)),      \
                        makeValidFilename(submodule.get(NAME_KEY)),  \
                        makeValidFilename(element.get(NAME_KEY)))

    print (path)
    os.makedirs(path)

    if not dependencies: 
        return True
    
    if checkCCodeGenerationValid(element, module.get(METADATA_PATH_KEY, ''), dependencies, path) and \
       checkCompilationValid(element, module.get(METADATA_PATH_KEY, ''),path)                    and \
       checkLibraryCreationValid(element, module.get(METADATA_PATH_KEY, ''), path): 
        return True
    else: 
        return False
        

def checkCodeValid(metadata, build_path):
    return all(checkElementCompilationValid(element, submodule, module, metadata, build_path) \
               for key, module in metadata.items()                                            \
               for submodule in module.get(SUBMODULES_KEY, [])                                \
               for element in submodule.get(ELEMENTS_KEY, []))                        


def loadLibrary(path):
    metadata = {}
    files = glob.glob(os.path.join(path, "*/" + METADATA_FILENAME))
    for filepath in files:
        with open(filepath) as f:
            module = json.load(f)
            module[METADATA_PATH_KEY] = filepath.replace(METADATA_FILENAME, "")
            metadata[module[NAME_KEY]] = module
    
    return metadata

def metadataPath(module):
    return os.path.join(module[METADATA_PATH_KEY], METADATA_FILENAME)

def getElementByName(element_name, submodule):
    for element in submodule.get(ELEMENTS_KEY, []):
        if element.get(NAME_KEY, []) == element_name:
            return element
    return {}

def getSubmoduleByRequiresReference(reference, metadata):
    for submodule in getModuleByRequiresReference(reference, metadata).get(SUBMODULES_KEY, []):
        if submodule.get(NAME_KEY, []) == reference.get(DEPENDENCY_SUBMODULE_KEY, []):
            return submodule
    return {}

def getModuleByRequiresReference(reference, metadata):
    module_name = reference.get(DEPENDENCY_MODULE_KEY, [])
    return metadata.get(module_name, {})

def getElementByRequiresReference(reference, metadata):
    element_name = reference.get(DEPENDENCY_ELEMENT_KEY, [])
    return getElementByName(element_name, getSubmoduleByRequiresReference(reference, metadata))

def checkInternalRequiresValid(metadata):
    return not [logError(metadataPath(module) + ":" + element.get(NAME_KEY), INTERNAL_REQUIRES_ERROR_MESSAGE, required)                                                          \
                for key, module in metadata.items()                                         \
                for submodule in module.get(SUBMODULES_KEY, [])                             \
                for element in submodule.get(ELEMENTS_KEY, [])                              \
                for required in element.get(REQUIRES_KEY, [])                               \
                if isinstance(required, str) and not getElementByName(required, submodule)]


def checkInternalConflictsValid(metadata):
    return not [logError(metadataPath(module) + ":" + element.get(NAME_KEY), INTERNAL_CONFLICTS_ERROR_MESSAGE, conflicting)  \
                for key, module in metadata.items()                                            \
                for submodule in module.get(SUBMODULES_KEY, [])                                \
                for element in submodule.get(ELEMENTS_KEY, [])                                 \
                for conflicting in element.get(CONFLICTS_KEY, [])                              \
                if isinstance(conflicting, str) and not getElementByName(conflicting, submodule)]
  


def checkExternalRequiresValid(metadata):
    return not [logError(metadataPath(module) + ":" + element.get(NAME_KEY), EXTERNAL_REQUIRES_ERROR_MESSAGE, required) \
                for key, module in metadata.items()                                                               \
                for submodule in module.get(SUBMODULES_KEY, [])                                                   \
                for element in submodule.get(ELEMENTS_KEY, [])                                                    \
                for required in element.get(REQUIRES_KEY, [])                                                     \
                if isinstance(required, dict) and not getElementByRequiresReference(required, metadata)]

def checkExternalConflictsValid(metadata):
    return not [logError(metadataPath(module) + ":" + element.get(NAME_KEY), EXTERNAL_CONFLICTS_ERROR_MESSAGE, conflicting) \
                for key, module in metadata.items()                                                                  \
                for submodule in module.get(SUBMODULES_KEY, [])                                                      \
                for element in submodule.get(ELEMENTS_KEY, [])                                                       \
                for conflicting in element.get(CONFLICTS_KEY, [])                                                    \
                if isinstance(conflicting, dict) and not getElementByRequiresReference(conflicting, metadata)]


def checkDependenciesValid(metadata):
    return checkInternalRequiresValid(metadata) and  \
           checkExternalRequiresValid(metadata) and  \
           checkInternalConflictsValid(metadata) and \
           checkExternalConflictsValid(metadata)


def checkAsn1FilesValid(metadata):
    return not [logError(metadataPath(module)+":"+element.get(NAME_KEY), ASN1FILES_ERROR_MESSAGE, asn1File) \
                for key, module in metadata.items()                               \
                for submodule in module.get(SUBMODULES_KEY, [])                   \
                for element in submodule.get(ELEMENTS_KEY, [])                    \
                for asn1File in element.get(ASN1FILES_KEY, [])                    \
                if asn1File not in os.listdir(module.get(METADATA_PATH_KEY))]

    
def validateMetadata(library_path=LIBRARY_PATH, build_path=BUILD_PATH):
    
    metadata = loadLibrary(library_path)

    print("\nVerifying metadata's asn.1 files")
    if checkAsn1FilesValid(metadata):
        print(VALIDATION_PASSED_MESSAGE)
    else:
        print(VALIDATION_FAILED_MESSAGE)
        return ERROR_CODE

    print("\nVeryfing metadata's module dependencies")
    if checkDependenciesValid(metadata):
        print(VALIDATION_PASSED_MESSAGE)
    else:
        print(VALIDATION_FAILED_MESSAGE)
        return ERROR_CODE

    print("\nGenerating and building C files")
    try:
        if(os.path.isdir(build_path)):
            shutil.rmtree(build_path)

        os.makedirs(build_path)
    except OSError:
        print("Error when creating directory: %s" % (build_path))
        return ERROR_CODE
    if checkCodeValid(metadata, build_path):
        print(VALIDATION_PASSED_MESSAGE)
    else:
        print(VALIDATION_FAILED_MESSAGE)
        return ERROR_CODE

    return SUCCESS_CODE

if __name__ == "__main__":
    sys.exit(validateMetadata())
