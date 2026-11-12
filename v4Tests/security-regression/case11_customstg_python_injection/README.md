# Case 11 – python.stg: code injection through string values and file names (ESACERT #75543)

`StgVarious/python.stg` is the custom STG (`-customStg python.stg::out.py`) that renders the ASN.1 model as a
Python module. OpenGEODE (and other TASTE tools) run it with the template installed next to the compiler and then
`exec()` the result. The template placed ASN.1 string values (`Print_StringValue`), the input file path
(`FileXml`, `TypeGeneric`, as `r"..."`), names (affected by `-typePrefix`) and DEFAULT values (`.EDQ`, which escapes
quotes but not backslashes) inside Python string literals without escaping. A value such as

    evilValue MyStr ::= "x""+str(__import__(""os"").system(""id""))+""y"

or a file named `a"+str(__import__("os").system("id"))+"b.asn` made the consumer execute arbitrary code. Values with
control characters (LF, NUL) produced a file that did not compile.

The compiler cannot escape for the template (custom STGs may emit any language), so the template chooses:
`$x; format="python"$` (`ST/ST.fs`, `StringFormatRenderer`) renders the body of a Python string literal that evaluates
back to the original text (quotes, backslash, control and non-ASCII characters escaped). `python.stg` now uses it for
every attribute inside a string literal, and no longer uses raw strings for file names. Third-party custom templates
that emit Python must do the same.

`reproduce_issue.sh` generates the module with OpenGEODE's options, from a directory and a file whose names carry
Python code (created in a temporary Linux directory: such names cannot exist on Windows file systems), executes it
with `check.py` and checks that:

- no injected code ran (no `PROOF*` file), for the string value, the directory and file name, and `-typePrefix`;
- the file path, the string values (backslash, both quotes, LF, CR, HT, NUL, non-ASCII) and a DEFAULT value whose
  text ends a string with a backslash read back unchanged.

Run it with `ASN1SCC=<compiler> ./reproduce_issue.sh` (the template next to the compiler is used; override with
`PYTHON_STG=<path>`). With the template of a compiler without the fix the injection leg reports the `PROOF_DIR`,
`PROOF_FILE` and `PROOF_VALUE` files and the special-character leg a file that does not compile.
