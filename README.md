[![CircleCI](https://dl.circleci.com/status-badge/img/circleci/NWDXtobZpVSQ5ErUz9CgXB/3deZmhdEfAoLGYUJtiCX4c/tree/master.svg?style=svg&circle-token=69c83a7973425a3ab92fb7e2d7580bcb292a508f)](https://dl.circleci.com/status-badge/redirect/circleci/NWDXtobZpVSQ5ErUz9CgXB/3deZmhdEfAoLGYUJtiCX4c/tree/master)

Executive summary
=================

This is the source code of the ASN1SCC compiler - an ASN.1 compiler that
targets **C**, **Ada**, **Scala**, **Python** and **Rust** while placing
specific emphasis on embedded systems.

ASN1SCC is the ASN.1 compiler of the **European Space Agency** and is used
in space missions to support binary encoding needs in satellite systems
flight and ground software.

An overview of the ASN.1 language support scope is available
[on this page](https://taste.tuxfamily.org/wiki/index.php?title=Technical_topic:_ASN1SCC_-_ESA%27s_ASN.1_Compiler_for_safety-critical_embedded_platforms)

What makes **ASN1SCC** unique is that in addition to supporting the standard
ASN.1 uPER compact binary encoding rules, it offers the possibility to
describe **custom binary encoding rules** with a simple textual notation
(ACN — ASN.1 Custom Encoding Rules), in order to communicate with equipment
that comes with legacy data formats. See
[this page](https://taste.tuxfamily.org/wiki/index.php?title=Technical_topic:_ASN.1_-_An_introduction_to_ACN)
for an overview and
[this page](https://taste.tuxfamily.org/wiki/index.php?title=Technical_topic:_Hints_to_model_complex_packet_encodings_with_ASN.1_and_ACN)
for concrete examples as used in space systems.

To know more you can also read
[this conference paper about ASN1SCC (PDF)](http://web1.see.asso.fr/erts2012/Site/0P2RUC89/7C-4.pdf),
or a [blog post with hands-on examples](https://www.thanassis.space/asn1.html).

Supported target languages and encodings
========================================

| Language | uPER | ACN | XER | BER |
|----------|:----:|:---:|:---:|:---:|
| C        | ✅   | ✅  | ✅  | ✅  |
| Ada      | ✅   | ✅  | ✅  | —   |
| Rust     | ✅   | ✅  | ✅  | —   |
| Scala    | ✅   | ✅  | —   | —   |
| Python   | ✅   | ✅  | ✅  | —   |

The C and Ada backends are the most mature and full-featured. The Rust
backend supports uPER, ACN and XER. The Scala and Python backends support
uPER, ACN and (for Python) XER.

Compilation
===========

## Install the Java JRE

This is a compile-time only dependency, required to execute ANTLR
(the parser generator used internally).

## Install .NET SDK (version 10.0)

Install the [.NET 10.0](https://dotnet.microsoft.com/download/dotnet/10.0) SDK.
Add the NuGet package source (in case it is missing):

    dotnet nuget add source "https://api.nuget.org/v3/index.json" --name "NuGet"

Then execute...

    dotnet build "asn1scc.sln"

...and the compiler will be built.

On Linux (Debian) the build can be done with:

    $ make -f Makefile.debian

(all dependencies will be installed automatically).

Under Windows, you can also open `asn1scc.sln` and build the `asn1scc`
project (right-click / build).

## Install language-specific dependencies

To **compile and run the generated code**, each backend needs its own
toolchain. The compiler itself (asn1scc) does not require these at build
time — they are only needed when you want to compile the *output* of
asn1scc.

### C

A C compiler is required. On Linux:

```bash
sudo apt-get install gcc
```

### Ada

An Ada compiler (GNAT) is required. On Linux:

```bash
sudo apt-get install gnat gprbuild
```

### Scala

Install `sbt` (the Scala Build Tool). On Linux:

```bash
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo tee /etc/apt/trusted.gpg.d/sbt.asc
sudo apt-get update
sudo apt-get install sbt
```

Windows: <https://www.scala-sbt.org/download/>

### Rust

Install the Rust toolchain via `rustup`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Python

Python 3 is required (Python 3.8 or later). On Linux:

```bash
sudo apt-get install python3 python3-pip
pip3 install pytest
```

## Run the tests

    cd v4Tests
    make

Note that in order to run the tests you need GCC, GNAT (Ada compiler),
Rust (cargo), Python 3 with pytest, and sbt — depending on which backends
you want to exercise. The tests will process hundreds of ASN.1 grammars,
generate source code for each target language, compile it, run it, and
check the coverage results.

To run tests for a specific backend only:

    cd v4Tests
    make cTests         # C backend only
    make adaTests       # Ada backend only
    make rustTests      # Rust backend only
    make pythonTests    # Python backend only
    make scalaTests     # Scala backend only

You can also run individual test cases with the Python test runner:

    cd v4Tests
    python3 scripts/runTests.py -l Rust -s
    python3 scripts/runTests.py -l c -t test-cases/acn/01-INTEGER/001.asn1

Continuous integration and Docker image
=======================================

ASN1SCC is set up to use CircleCI for continuous integration. Upon every
commit or merge request, [we instruct CircleCI](.circleci/config.yml) to:

- create on the fly [a Docker image](Dockerfile) based on Microsoft's .NET image
- [build ASN1SCC](circleci-build.sh) with the new code inside that image
- then run all the tests and check the coverage results

In addition, a runtime docker image can be built with the following command,
which can then be used instead of installing ASN1SCC on the host:

    DOCKER_BUILDKIT=1 docker build -t asn1scc-runtime -f Dockerfile.runtime .

The [asn1-docker.sh](asn1-docker.sh) bash script wraps the `docker run ...`
call into an easy-to-use compiler command. For example, assuming your
ASN.1 files are in `/tmp/myasnfiles/`:

```bash
$ pwd
/tmp/myasnfiles

$ cat sample.asn
MY-MODULE DEFINITIONS AUTOMATIC TAGS ::= BEGIN
Message ::= SEQUENCE {
    msgId INTEGER,
    myflag INTEGER,
    value REAL,
    szDescription OCTET STRING (SIZE(10)),
    isReady BOOLEAN
}
END

$ /opt/asn1scc/asn1-docker.sh -c -uPER sample.asn
$ ls
asn1crt.c  asn1crt_encoding.c  asn1crt_encoding.h  asn1crt_encoding_uper.c
asn1crt_encoding_uper.h  asn1crt.h  sample.asn  sample.c  sample.h
```

Usage
=====

The compiler has many features. You can see some simple usage examples in a
[blog post](https://www.thanassis.space/asn1.html) and check out the
official [TASTE project site](https://taste.tools).

All the examples below use the following ASN.1 grammar:

```asn1
MY-MODULE DEFINITIONS AUTOMATIC TAGS ::= BEGIN
Message ::= SEQUENCE {
    msgId INTEGER,
    myflag INTEGER,
    value REAL,
    szDescription OCTET STRING (SIZE(10)),
    isReady BOOLEAN
}
END
```

C backend
---------

### Code generation

```bash
$ asn1scc -c -uPER sample.asn
```

This produces `sample.h`, `sample.c` and the runtime library files
(`asn1crt.c`, `asn1crt.h`, etc.) in the current directory.

### Encoding a message

```c
#include <stdio.h>
#include "sample.h"

int main(void)
{
    Message testMessage = {
        .msgId  = 1,
        .myflag = 2,
        .value  = 3.14,
        .szDescription = { .arr = "HelloWorld" },
        .isReady = true
    };

    unsigned char encodedBuffer[Message_REQUIRED_BYTES_FOR_ENCODING];
    BitStream encodedMessage;
    int errCode;

    BitStream_Init(&encodedMessage, encodedBuffer,
                   Message_REQUIRED_BYTES_FOR_ENCODING);

    if (!Message_Encode(&testMessage, &encodedMessage, &errCode, true))
    {
        printf("Encoding failed with error code %d\n", errCode);
    }
    else
    {
        int encodedSize = BitStream_GetLength(&encodedMessage);
        for (int i = 0; i < encodedSize; ++i)
            printf("%02x ", encodedBuffer[i]);
        printf("(%d bytes)\n", encodedSize);
    }
}
```

### Compiling and running

```bash
$ gcc -o sample_test *.c
$ ./sample_test
01 01 01 02 09 80 cd 19 1e b8 51 eb 85 1f 48 65 6c 6c 6f 57 6f 72 6c 64 80 (25 bytes)
```

Ada backend
-----------

### Code generation

```bash
$ asn1scc -Ada -uPER -typePrefix T -o out_dir sample.asn
```

This produces `my_module.ads`, `my_module.adb`, the Ada runtime library
files (`adaasn1rtl.adb`, `adaasn1rtl.ads`, etc.), an `asn1_x86.gpr`
project file, and a `Makefile` in `out_dir/`.

### Encoding a message

```ada
with adaasn1rtl; use adaasn1rtl;
with adaasn1rtl.encoding; use adaasn1rtl.encoding;
with My_Module; use My_Module;

procedure Encode_Example is
    TestMessage : TMessage := (msgId => 1, myflag => 2, value => 3.14,
                               szDescription => (Data => "HelloWorld"),
                               isReady => True);
    EncodedBuffer : aliased adaasn1rtl.Byte_Buffer(1 .. TMessage_REQUIRED_BYTES_FOR_ENCODING);
    EncodedMessage : adaasn1rtl.encoding.Bitstream;
    ErrCode : asn1SccErrorCode;
    Result : asn1SccBoolean;
begin
    adaasn1rtl.encoding.Bitstream_Init(EncodedMessage, EncodedBuffer);
    Result := TMessage_Encode(TestMessage, EncodedMessage, ErrCode);
    --  Result = True means encoding succeeded
end Encode_Example;
```

### Compiling and running

```bash
$ cd out_dir
$ gprbuild -gnat2012 -P asn1_x86.gpr mainprogram.adb
$ ./mainprogram
```

Rust backend
------------

### Code generation

```bash
$ asn1scc -Rust -uPER -atc -typePrefix ASN1SCC_ -o out_dir sample.asn
```

This produces a Cargo project in `out_dir/`:

```
out_dir/
├── Cargo.toml              # Project manifest
├── mainprogram.rs          # Entry point for automatic test cases
├── sampleDef.rs            # Type definitions (structs, enums)
├── sample.rs               # Encode/decode function implementations
├── test_case_001.rs        # Individual test case
├── test_auto_tcsDef.rs     # Auto test cases (types)
├── test_auto_tcs.rs        # Auto test cases (bodies)
├── testsuiteDef.rs         # Test suite (types)
├── testsuite.rs            # Test suite (body)
└── asn1rust/               # Runtime library crate
    ├── Cargo.toml
    └── src/
        ├── lib.rs           # Public API
        ├── uper.rs           # uPER encode/decode primitives
        ├── acn.rs            # ACN encode/decode primitives
        ├── xer.rs            # XER encode/decode primitives
        └── ber.rs            # BER encode/decode primitives
```

### Encoding a message

```rust
use asn1rust::*;
use crate::sampleDef::*;

fn main() {
    let test_message = ASN1SCC_Message {
        msg_id: 1,
        myflag: 2,
        value: 3.14,
        sz_description: ASN1SCC_Message_szDescription { arr: *b"HelloWorld" },
        is_ready: true,
    };

    let mut buffer = [0u8; 256];
    let mut bitstream = BitStream::new(&mut buffer);
    let mut err_code: i32 = 0;

    if ASN1SCC_Message_Encode(&test_message, &mut bitstream, &mut err_code, true) {
        let encoded_size = bitstream.get_length();
        for i in 0..encoded_size {
            print!("{:02x} ", buffer[i]);
        }
        println!("({} bytes)", encoded_size);
    } else {
        println!("Encoding failed with error code {}", err_code);
    }
}
```

### Compiling and running

```bash
$ cd out_dir
$ cargo run
```

For ACN encoding with a custom encoding specification file:

```bash
$ asn1scc -Rust -ACN -atc -typePrefix ASN1SCC_ -o out_dir sample.asn sample.acn
```

For XER encoding:

```bash
$ asn1scc -Rust -xer -atc -typePrefix ASN1SCC_ -o out_dir sample.asn
```

Scala backend
-------------

### Code generation

```bash
$ asn1scc -Scala -uPER -atc -typePrefix T -o out_dir sample.asn
```

This produces an sbt project in `out_dir/` with Scala source files
in `src/main/scala/asn1src/` and the Scala runtime library in
`src/main/scala/asn1scala/`.

### Encoding a message

```scala
import asn1src.*
import asn1scala.*

val testMessage = TMessage(
    msgId = 1,
    myflag = 2,
    value = 3.14,
    szDescription = TMessage_szDescription(Vector.tabulate(10)(i => (i % 256).toByte)),
    isReady = true
)

val buffer = new Array[Byte](TMessage EncodeConstants.REQUIRED_BYTES_FOR_ENCODING)
val codec = UPER(buffer)
val result = TMessage_Encode(testMessage, codec, true)
//  result is Either[ErrorCode, Int]
```

### Compiling and running

```bash
$ cd out_dir
$ sbt run
```

Python backend
--------------

### Code generation

```bash
$ asn1scc -python -uPER -atc -typePrefix T -o out_dir sample.asn
```

This produces a Python package in `out_dir/asn1pylib/` containing:
- `asn1python/` — the runtime library (bitstream, codecs, types)
- `asn1src/` — generated type definitions, encoders/decoders and test cases

### Encoding a message

```python
from asn1pylib.asn1src.MY_MODULE import TMessage, TMessage_szDescription
from asn1pylib.asn1python import UPEREncoder

test_message = TMessage(
    msgId=1,
    myflag=2,
    value=3.14,
    szDescription=TMessage_szDescription([72, 101, 108, 108, 111, 87, 111, 114, 108, 100]),
    isReady=True
)

encoder = UPEREncoder.of_size(TMessage.EncodeConstants.REQUIRED_BYTES_FOR_ENCODING)
test_message.encode_uper(encoder, True)
encoded_bytes = encoder.get_bitstream_buffer()
print(" ".join(f"{b:02x}" for b in encoded_bytes))
```

### Running the tests

```bash
$ cd out_dir/asn1pylib
$ pytest
```

Command-line options
====================

### Language selection

| Option | Short form | Description |
|--------|-----------|-------------|
| `--c-lang` | `-c` | Generate code for C/C++ |
| `--ada-lang` | `-Ada` | Generate code for Ada/SPARK |
| `--scala-lang` | `-Scala` | Generate code for Scala |
| `--rust-lang` | `-Rust` | Generate code for Rust |
| `--python-lang` | `-python` | Generate code for Python (experimental) |

You must select at least one target language.

### Encoding selection

| Option | Short form | Description |
|--------|-----------|-------------|
| `--uper-enc` | `-uPER` | Unaligned Packed Encoding Rules |
| `--xer-enc` | `-XER` | XML Encoding Rules |
| `--acn-enc` | `-ACN` | ASN.1 Custom Encoding Rules (requires an ACN file) |

You can select multiple encodings in a single invocation. All backends
support uPER and ACN. XER is supported by C, Ada, Rust and Python. BER is
supported only by C.

### Code generation options

| Option | Short form | Description | Backends |
|--------|-----------|-------------|----------|
| `--auto-test-cases` | `-atc` | Generate automatic test cases that round-trip-encode and decode every type | All |
| `--equal-func` | `-equal` | Generate equality-testing functions (automatically enabled by `-atc`) | All |
| `--type-prefix <prefix>` | `-typePrefix` | Add a prefix to all generated data type names | C, Ada |
| `--field-prefix <prefix>` | `-fp` | Add a prefix to component/alternative field names. Use `AUTO` to prefix only names that conflict with language keywords | All |
| `--rename-policy <int>` | `-renamePolicy` | Policy for renaming enumerated values (0 = Ada-style, 1 = C-style, 2 = C-style with underscore, 3 = lower-case). Default: 0 for Ada/Python, 1 for C/Scala/Rust | All |
| `--enable-efficient-enumerations <uint>` | `-eee` | Use binary search instead of switch-case for enumerated types with at least `<uint>` enumerants | C only |
| `--slim` | `-slim` | **Slim mode**: generate narrow integer/real types based on ASN.1 range constraints (e.g. `INTEGER (0..255)` → `uint8_t` instead of the default 64-bit type) | All |
| `--init-globals` | `-ig` | Generate `const` global variables containing default-initialized values; init procedures copy from these globals instead of field-by-field init | C, Python, Rust, Scala (not Ada) |
| `--streaming-mode` | `-sm` | Streaming mode support | All |
| `--handle-empty-sequences` | `-es` | Add a dummy integer member to empty SEQUENCE structures (needed for C compliance) | All |
| `--log-execution-time` | `-let` | Enable detailed logging of execution time | All |
| `--acn-deferred` | `-acnDeferred` | ACN deferred patching: separate functions for reference types with ACN parameters | C, Ada only |

### ICD (Interface Control Document) options

| Option | Short form | Description | Backends |
|--------|-----------|-------------|----------|
| `--icd-uper <file>` | `-icdUper` | Produce an ICD HTML file describing the uPER encoding | All |
| `--icd-acn <file>` | `-icdAcn` | Produce an ICD HTML file describing the ACN encoding | C, Ada, Rust, Python |
| `--icd-raw <file>` | `-icdRaw` | Serialize the ACN ICD model to JSON | C, Ada, Rust, Python |
| `--icd-pdus <list>` | `-icdPdus` | Comma-separated list of PDU type assignments to limit generation | All |
| `--detect-pdus` | `-dpdus` | Auto-detect PDUs in the ASN.1 grammar | All |

### Output and debug options

| Option | Short form | Description |
|--------|-----------|-------------|
| `--out <dir>` | `-o` | Output directory (default: current directory) |
| `--xml-ast <file>` | `-x` | Dump the internal AST to an XML file |
| `--custom-stg <file>` | `-customStg` | Use a custom StringTemplate file for code generation |
| `--custom-stg-ast-version <int>` | `-customStgAstVersion` | AST version for custom STG (1 = original, 4 = with referenced types) |
| `--include-func <func>` | `-if` | Include a function from the RTL (C only) |
| `--mapping-functions-module <name>` | `-mfm` | Name of the Ada module or C header file with mapping function definitions |
| `--word-size <int>` | `-wordSize` | Size of `asn1SccSint`/`asn1SccUint` in bytes (4 or 8). C only |
| `--fp-word-size <int>` | `-fpWordSize` | Size of REAL type (4 or 8 bytes). C only |
| `--target <profile>` | `-t` | Ada target profile: `x86`, `stm32`, `msp430`, or `allboards`. Ada only |
| `--generate-test-grammar` | `-gtc` | Generate a sample ASN.1 grammar for testing (experimental) |
| `--print-template-info` | `-printTemplateInfo` | Add line-number comments to generated code indicating the source template |
| `--debug-asn1 <string>` | `-asn1` | Print all ASN.1 grammars in a single module/file (debugging) |
| `--version` | `-v` | Display version information |
| `--help` | `-help` | Display help |

### Notes on slim mode (`-slim`)

In **slim mode**, the compiler narrows integer and real types to the
smallest native type that fits the ASN.1 range constraints. For example,
`INTEGER (0..255)` generates `uint8_t` (C), `Interfaces.Unsigned_8` (Ada),
`u8` (Rust), etc. Without slim mode, all integers use the full-width
`asn1SccSInt`/`asn1SccUint` types (64-bit by default).

Slim mode is supported by all backends and can be combined with any
encoding.

### Notes on init-globals (`-ig`)

With `-ig`, the compiler generates a `const` global variable for each
type assignment containing the default-initialized value. The init
procedure then copies from this global instead of doing field-by-field
initialization. This is primarily a C optimization but also works for
Python, Rust and Scala (all of which use procedure-style initialization).
Ada uses function-style initialization, so `-ig` has no effect there.

### Notes on `-acnDeferred`

The ACN deferred mode (`-acnDeferred` or `--acn-deferred`) generates
separate encode/decode functions for reference types that have ACN
parameters, enabling more modular code. This feature is supported only
by the C and Ada backends.

Credits
=======

Project supervisor at the European Space Agency: Maxime Perrotin (maxime.perrotin@esa.int)

Main project developer: George Mamais (gmamais@gmail.com)

Check <https://lamdasoft.eu/asn1scc/> if you need commercial support.

Major contributor: Thanassis Tsiodras (ttsiodras@gmail.com)

The Scala backend was developed by:
* Filip Schramka (Ateleris)
* Ivo Nussbaumer (Ateleris)
* Mario Bucev (EPFL)
* Simon Felix (Ateleris)

The Python backend was developed by:
* Julia Hartmann (Ateleris)
* Luca Schafroth (Ateleris)
* Manuel Stutz (Ateleris)

The Rust backend was developed by:
* Maxime Perrotin (European Space Agency)
