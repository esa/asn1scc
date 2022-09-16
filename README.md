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

## Install .NET SDK (version 6.0)

Install the [.NET 6.0](https://dotnet.microsoft.com/download/dotnet/6.0) sdk.
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
acn.c  asn1crt.c  asn1crt.h  real.c  sample.asn  sample.c  sample.h
```

As can be seen above, the host does not have the ASN1SCC installation; 
only the asn1scc-runtime docker image.

Usage
=====

The compiler has many features - documented in
[Chapter 10 of the TASTE manual](http://download.tuxfamily.org/taste/snapshots/doc/taste-documentation-current.pdf),
and you can see some simple usage examples in a related
[blog post](https://www.thanassis.space/asn1.html).

You can also check out the official [TASTE project site](https://taste.tools).

Credits
=======
George Mamais (gmamais@gmail.com), Thanassis Tsiodras (ttsiodras@gmail.com)
