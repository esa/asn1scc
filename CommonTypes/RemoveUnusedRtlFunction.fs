module RemoveUnusedRtlFunction


let findFunctionNames (headerContents: string) : string list =
    let pattern = @"\b[a-zA-Z_][a-zA-Z0-9_]*\s+[a-zA-Z_][a-zA-Z0-9_]*\s*\([^\)]*\)\s*;"
    let regex = System.Text.RegularExpressions.Regex(pattern)
    let matches = regex.Matches(headerContents)
    
    [ for matchResult in matches do
        let declaration = matchResult.Value
        // Split the declaration at whitespace and take the second to last element,
        // which should be the function name (assuming no return type modifiers like pointers)
        let parts = declaration.Split([|' '; '\t'; '\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries)
        if parts.Length > 1 then yield parts.[parts.Length - 2] ]


