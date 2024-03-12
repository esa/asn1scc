CROSS-TESTS
===

This directory contains framework and test-cases for cross-tests of the ASN1SCC compiler.
These check for any errors in communication between languages.
Specifically, tests here implement a scenario in which an ASN.1
structure encoded with a language is still decodable with a different one.

Usage and Implementation
===
Framework is written as a [SCons](www.scons.org) build,
with the target being a report about passed/failed test cases,
and the source being ASN.1 structures located in directory test-cases.

To launch tests simply type in `scons`, keep in mind that SCons will by default use lazy approach
and will build only the necessary targets. All test-cases belong in appropriate subdirectories.

The framework internally uses `gcc` and `gnat` for compiling asn1scc output.
It will try to build the compiler if it does not exist.
