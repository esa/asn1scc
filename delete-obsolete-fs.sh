#!/usr/bin/env bash
# Delete tracked obsolete .fs files; warn for untracked ones.

# Ensure we are inside a Git repo
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Error: not inside a Git repository." >&2
  exit 1
fi

# List of obsolete files (one per line)
read -r -d '' FILES <<'EOF'
./asn1scc/AssemblyInfo.fs
./BackendAst/BackendAst.fs
./BackendAst/BackendAstConstruct.fs
./BackendAst/BAst.fs
./BackendAst/BastAddAcnInsertFields.fs
./BackendAst/BAstConstruction.fs
./BackendAst/BAstFold.fs
./BackendAst/CAst.fs
./BackendAst/CAstAcnEncodingClasses.fs
./BackendAst/CAstConstruction.fs
./BackendAst/CAstFold.fs
./BackendAst/Constraints.fs
./BackendAst/ConstraintsMapping.fs
./BackendAst/createDefinitions.fs
./BackendAst/DAstValidate.fs
./BackendAst/print_debug.fs
./BackendAst/uPER.fs
./BackendAst/validate.fs
./BackendAst/variable.fs
./CommonTypes/isvalid_if.stg.fs
./FrontEndAst/AcnType2.fs
./FrontEndAst/AcnTypes.fs
./FrontEndAst/AssemblyInfo.fs
./FrontEndAst/GenericFold2.fs
./FrontEndAst/Library1.fs
./FrontEndAst/LoadAcnInfo.fs
./FrontEndAst/TargetLanguageStgMacros.fs
./StgAda/AssemblyInfo.fs
./StgAda/isvalid_a_impl.stg.fs
./StgC/AbstractMacrosImp.fs
./StgC/AssemblyInfo.fs
./StgC/isvalid_c_impl.stg.fs
./StgScala/AssemblyInfo.fs
./StgVarious/AssemblyInfo.fs
./StgVarious/Library1.fs
EOF

deleted=0
skipped=0

# Process each file line-by-line
while IFS= read -r f; do
  # Skip empty lines (just in case)
  [[ -z "$f" ]] && continue

  if git ls-files --error-unmatch "$f" >/dev/null 2>&1; then
    # File is tracked: remove it from repo (and working tree if present)
    if git rm "$f"; then
      echo "Deleted:   $f"
      ((deleted++))
    else
      echo "Error deleting: $f" >&2
    fi
  else
    echo "Warning:  Not tracked by Git (skipped): $f"
    ((skipped++))
  fi
done <<< "$FILES"

echo
echo "Summary: deleted=$deleted, skipped_not_tracked=$skipped"
echo "Reminder: review changes with 'git status' and commit when ready."
