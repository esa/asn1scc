The **`IA5String`** type (International Alphabet number 5) is one of the fundamental character string types of ASN.1 and is generally equivalent to the ASCII alphabet.

Assigning an abstract value to an ASN.1 type, including `IA5String`, follows these general rules:

1.  The value reference name must start with a lowercase letter (for example, `valuereference`).
2.  It is followed by the type to which the value belongs, starting with an uppercase letter (for example, `IA5String`).
3.  The assignment symbol **`::=`** is used.
4.  The actual value (`Value`) comes last.

**Note:** Abstract values defined in an ASN.1 module are **never encoded** for transmission; they only improve readability in subtype constraints or define default values.

### IA5String Value Syntax

Character string types such as `IA5String` are typically enclosed in double quotes (`"`).

**1. Basic String Assignment:**

A basic value assignment for `IA5String` looks like this:

```asn1
string IA5String ::= "string including ""double quotes"""
-- represents <<string including "double quotes">>
```
If the string contains a double quote, it must appear doubled within the value.

**2. Assignment With Non-Graphic Characters:**

`IA5String` supports a special notation that allows you to reference non-graphic (control) characters by using braces `{}` in a `CharacterStringList` form:

*   This notation lets you mix quoted string fragments with character references.
*   Non-graphic characters can be referenced either by named values (imported from the standard **`ASN1-CHARACTER-MODULE`**) or through the **`Tuple`** production.
*   The `Tuple` production applies only to `IA5String` and identifies characters by their table column and row (for example, `{TableColumn, TableRow}`).

**Example** using an imported character reference:
```asn1
ExampleIA5String DEFINITIONS ::=
BEGIN
IMPORTS cr FROM ASN1-CHARACTER-MODULE {joint-iso-itu-t
asn1(1) specification(0) modules(0) iso10646(0)} ;
two-lines IA5String ::= { "First line", cr,
"Second line" }
END
```
In this example the `two-lines` value includes a carriage-return (CR) character.
