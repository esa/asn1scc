# ACN Save-Position Internals

## Behaviour at a Glance

- Fields marked with `save-position` request that the encoder remember the current bitstream location so post-processing hooks can patch bytes after child encoding finishes.
- The backend stores these bookmarks inside the generated `_extension_function_positions` struct (one per enclosing sequence) and threads the chosen variable name through every inlined encoder.
- During decoding, the same bookmark gives post-decode validators access to the original bit ranges, mirroring the encoder side for symmetry.
- The optimisation that inlines child encoders into `CHOICE` parents relies on deterministic variable names; the updated implementation ensures declaration and usage stay in sync.

## Code Hotspots

- `createAcnChild` (`BackendAst/DAstConstruction.fs:37`) computes the determinant metadata, derives the local position variable name, and calls `handleSavePosition` with the chosen identifier.
- `handleSavePosition` (`BackendAst/DAstACN.fs:90`) wraps the user-supplied `FuncBody`, prepending a `sequence_save_bitstream` invocation from the active language macros when `savePosition` is true.
- `handleAlignmentForAcnTypes` (`BackendAst/DAstACN.fs:151`) runs after `handleSavePosition`, so alignment logic never interleaves with the bookmark write.
- `getDeterminantTypeDefinitionBodyWithinSeq` (`BackendAst/DAstACN.fs:30`) computes the struct type used to store saved positions, ensuring the struct matches the determinant's data kind.

```fsharp
let handleSavePosition funcBody savePosition cName lvName _ lm codec =
    match savePosition with
    | false -> funcBody
    | true  ->
        fun state err prms nesting scope ->
            let content, nextState = funcBody state err prms nesting scope
            let stmt = lm.acn.sequence_save_bitstream lvName cName codec
            let wrapped =
                match content with
                | Some body -> Some { body with funcBody = sprintf "%s\n%s" stmt body.funcBody }
                | None      -> Some { funcBody = stmt; errCodes = []; localVariables = []; bValIsUnReferenced = true; bBsIsUnReferenced = false; resultExpr = None; auxiliaries = []; icdResult = None }
            wrapped, nextState
```

## Interaction with the AST

- `Asn1AcnAst.AcnChild.Type.savePosition` flags the fields; the flag comes from ACN parsing and is preserved through `DAstConstruction`.
- `AcnInsertedFieldDependencies` can redirect determinants; when that happens the `createAcnChild` helper fetches the de-aliased dependency before computing the position struct.
- The generated struct name derives from the parent sequence identifiers (`getAcnDeterminantName`), so renaming ASN.1 types propagates automatically.
- The call graph stored in `State.functionCalls` ensures any encoder that depends on bookmarks is still emitted, even if the user narrows output to specific PDUs.

## Naming / Refactor Notes

- The flag parameter `savePosition` would read clearer as `requiresBookmark`, matching the intent seen in documentation and macro names.
- `lvName` could be renamed `bookmarkVarName` throughout `DAstConstruction.createAcnChild` to document that it refers to the struct instance, not the determinant itself.
- Consider lifting the bookmark wrapping into a dedicated helper module to keep `DAstACN` focused on codec construction; this would also help unit-test the bookmark logic in isolation.
