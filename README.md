[![Build and Test Status of ASN1SCC on Circle CI](https://circleci.com/gh/ttsiodras/asn1scc.svg?&style=shield&circle-token=fcc32f415742887faa6ad69826b1cf25426df086)](https://circleci.com/gh/ttsiodras/asn1scc/tree/master)

*For the impatient: if you already know what ASN.1 and ASN1SCC is, and
just want to run the ASN1SCC compiler:*

    docker pull ttsiodras/asn1scc
    docker run -it ttsiodras/asn1scc

*...and follow the instructions shown.*

Executive summary
=================

This is the source code of the ASN1SCC compiler - an ASN.1 compiler that
targets C and Ada, while placing specific emphasis on embedded systems.
You can read a comprehensive paper about it
[here (PDF)](http://web1.see.asso.fr/erts2012/Site/0P2RUC89/7C-4.pdf),
or a blog post with hands-on examples
[here](https://www.thanassis.space/asn1.html).
Suffice to say, if you are developing for embedded systems, it will probably
interest you.

Compilation
===========

## Ιnstall the Java JRE

First, install the Java JRE. This is a compile-time only dependency,
required to execute ANTLR.

## Install .NET SDK (version 7.0)

Install the [.NET 7.0](https://dotnet.microsoft.com/download/dotnet/7.0) sdk.
Add NuGet package source (in case it is missing) by executing:
    
    dotnet nuget add source "https://api.nuget.org/v3/index.json" --name "NuGet"
    
Then execute...

    dotnet build "asn1scc.sln"

...and the compiler will be built.

On Linux (debian) the build can be done with the following command:

    $ make -f Makefile.debian

(all dependencies will be installed automatically)

Under Windows, you can also open open `asn1scc.sln` and build the `asn1scc` project (right-click/build)

## Run the tests - if you want to:

    cd v4Tests
    make

Note that in order to run the tests you need both GCC and GNAT.
The tests will process hundreds of ASN.1 grammars, generate C and
Ada source code, compile it, run it, and check the coverage results.

Continuous integration and Docker image
=======================================

ASN1SCC is setup to use CircleCI for continuous integration. Upon every
commit or merge request, [we instruct CircleCI](.circleci/config.yml) to...

- create on the fly [a Docker image](Dockerfile) based on Microsoft's .NET image
- [build ASN1SCC](circleci-build.sh) with the new code inside that image
- then run all the tests and check the coverage results.

In addition, a runtime docker image can be build with the following command
which can then be used instead of installing ASN1SCC on the host (or any of
the dependencies or build tools).

    DOCKER_BUILDKIT=1 docker build -t asn1scc-runtime -f Dockerfile.runtime .

...and your Docker will build an "asn1scc-runtime" Docker image. This image can be
used as if the ASN1SCC is installed on the host system. The [asn1-docker.sh](asn1-docker.sh)
bash script can be used to wrap the `docker run ...` call into a easy to use compiler command.
For example, let's assume your ASN.1 files are in a folder as `/tmp/myasnfiles/`. You can use 
this script file like calling `asn1.exe` file as if it is on your host system. The ASN1SCC will
be executed inside a docker container and the generated files will appear in the folder
where the script was called. Assuming that the ASN.1 file is named `sample.asn`, here is a sample
call of the script (`asn1-docker.sh` script is inside `/opt/asn1scc` folder and the docker image
named `asn1scc-runtime` is already built.)

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
asn1crt.c  asn1crt_encoding.c  asn1crt_encoding.h  asn1crt_encoding_uper.c  asn1crt_encoding_uper.h  asn1crt.h  sample.asn  sample.c  sample.h
```

As can be seen above, the host does not have the ASN1SCC installation; 
only the asn1scc-runtime docker image.

Usage
=====
The compiler has many features and you can see some simple usage examples in a [blog post](https://www.thanassis.space/asn1.html).
You can also check out the official [TASTE project site](https://taste.tools).

This is an example of use, assuming you have created the ASN.1 sample given above. If you have installed the compiler binary instead of the Docker image, you may generate the code with this command:

```bash
$ asn1scc -c -uPER sample.asn
```

We will write a simple C function that creates a variable of type "Message", then encode it and print the resulting binary data:

```c
$ cat sample_test.c
#include <stdio.h>
#include "sample.h"

int main(void)
{
    Message testMessage = {   // create a dummy message for the test
        .msgId  = 1,
        .myflag = 2,
        .value  = 3.14,
        .szDescription = {
             .arr = "HelloWorld"
        },
        .isReady = true
    };

    // Create a buffer to hold the encoded data.
    // The (maximum) size is computed by the compiler and set
    // in a macro defined in sample.h
    unsigned char encodedBuffer[Message_REQUIRED_BYTES_FOR_ENCODING];

    // The encoder needs a data structure for the serialization
    BitStream encodedMessage;

    // The Encoder may fail and update an error code
    int errCode;

    // Initialization associates the buffer to the bit stream
    BitStream_Init (&encodedMessage,
                    encodedBuffer,
                    Message_REQUIRED_BYTES_FOR_ENCODING);
    
    // Encode the message using uPER encoding rule
    if (!Message_Encode(&testMessage,
                        &encodedMessage,
                        &errCode,
                        true))
    {
        // Error codes are defined as macros in sample.h
        printf("Encoding failed with error code %d\n", errCode);       
    }
    else  // Everything went fine, print the message as a suite of hex numbers
    {
        int encodedSize = BitStream_GetLength(&encodedMessage);
        for (int i=0; i<encodedSize; ++i)
        {
            printf("%02x ", encodedBuffer[i]);
        }
        printf("(%d bytes)\n", encodedSize);
    }
}
```

Then compile it and run it:
```bash
$ gcc -o sample_test *.c
$ ./sample_test
01 01 01 02 09 80 cd 19 1e b8 51 eb 85 1f 48 65 6c 6c 6f 57 6f 72 6c 64 80 (25 bytes)
```

Credits
=======
George Mamais (gmamais@gmail.com), Thanassis Tsiodras (ttsiodras@gmail.com)
