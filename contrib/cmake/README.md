# FindAsn1scc CMake Module
by Jamie Smith

This module should make using asn1scc in CMake projects very simple. After adding the cmake file someplace on your CMAKE_MODULE_PATH, you can just use:
```cmake
find_package(Asn1scc)
```

Then you can create asn.1 libraries like this:
```cmake
add_asn1_library(foo_serialization
    LIB_TYPE STATIC
    PROTOCOL foo.asn
    GEN_DIR ${CMAKE_CURRENT_SOURCE_DIR}/generated_c
    RULES uPER)
```

and the module will do all the heavy lifting.

## Docs
**This module defines the following variables:**
- `Asn1scc_EXECUTABLE` - path to asn1scc executable
- `Asn1scc_COMMAND` - command needed to run asn1scc (can be a list if mono is being used)
- `Asn1scc_VERSION` - Numeric version of asn1scc
- `Asn1scc_FOUND` - whether or not asn1scc was found

**It also defines the following functions:**

### add_asn1_library
Creates a library from an asn.1 protocol.  The library target will be generated with appropriate interface includes and definitions so that all you need to do is link to it to use the generated code. 

Usage: 
```
add_asn1_library(<target name>
    PROTOCOL <.asn and .acn files...>
    RULES <one or more of: uPER, XER, ACN>
    ASN1_OPTIONS <options for asn1scc>
    GEN_DIR <directory in source or binary dir where C files will be generated>
    [WORD_SIZE <bytes>]
    [FP_WORD_SIZE <bytes>]
    [LIB_TYPE <STATIC, OBJECT or SHARED>])
```

This function is safe to call even if asn1scc was not found: If asn1scc is not found and GEN_DIR contains up-to-date C files, then those files will be used as-is.  If you want to disable this behavior, simply set REQUIRED in your find_package(Asn1scc) call so that an error is produced if asn1scc is not found.

The WORD_SIZE and FP_WORD_SIZE options can be used to add the `--word-size` and `--fp-word-size options`  to asn1scc.  They will take care of adding the needed C compile options for these arguments.  

NOTE: all contents of GEN_DIR will be deleted when ASN.1 generation is performed.

### wrap_asn1_sources
Lower-level function to create rules to build asn.1 source files.  You would use this if you need to compile asn.1 source files into an existing target.

Usage:
```
wrap_asn1_sources(<out var>
    PROTOCOL <.asn and .acn files...>
    RULES <one or more of: uPER, XER, ACN>
    ASN1_OPTIONS <options for asn1scc>
    GEN_DIR <directory in source or binary dir where C files will be generated>)
```

OUT_VAR will be populated with the names of .c files to attach to a target.

This function can be used even if asn1scc was not found: If asn1scc is not found and GEN_DIR contains up-to-date C files, then those files will be used as-is.  If you want to disable this behavior, simply set REQUIRED in your find_package(Asn1scc) call so that an error is produced if asn1scc is not found.

NOTE: all contents of GEN_DIR will be deleted when ASN.1 generation is performed.

