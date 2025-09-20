#!/usr/bin/env bash

# Fail immediately on errors, unset variables, or failed pipelines.
set -euo pipefail

# Default to dry-run mode so the script only reports potential deletions.
DRY_RUN=${DRY_RUN:-1}

# Ensure we are operating inside a Git repository before continuing.
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Error: This script must be run inside a Git repository." >&2
  exit 1
fi

# Resolve the absolute path to the repository root and work from there for consistency.
REPO_ROOT=$(git rev-parse --show-toplevel)
cd "$REPO_ROOT"

# Let the user know whether the script will actually remove files or just report them.
if [[ "$DRY_RUN" == "1" ]]; then
  echo "Running in dry-run mode. Set DRY_RUN=0 to allow git rm deletions."
else
  echo "Dry-run disabled. Tracked files that are not referenced will be removed via git rm."
fi

declare -a PROJECTS=()
while IFS= read -r project; do
  PROJECTS+=("$project")
done < <(find . -type f -name '*.fsproj' -print | sort)

# Abort early if the repository does not contain any F# project files.
if [[ ${#PROJECTS[@]} -eq 0 ]]; then
  echo "No .fsproj files found under $REPO_ROOT."
  exit 0
fi

OVERALL_TRACKED=0
OVERALL_REMOVED=0

for PROJECT_PATH in "${PROJECTS[@]}"; do
  PROJECT_DIR=$(dirname "$PROJECT_PATH")
  PROJECT_DIR_CLEAN=${PROJECT_DIR#./}
  PROJECT_DISPLAY=${PROJECT_PATH#./}
  echo "\n=== Inspecting project: $PROJECT_DISPLAY ==="

  # Extract the list of .fs files explicitly referenced by the project using Python for reliable XML parsing.
  mapfile -t REFERENCED_FILES < <(python3 - <<'PY' "$PROJECT_PATH"
import sys
from pathlib import Path
import xml.etree.ElementTree as ET

project_path = Path(sys.argv[1]).resolve()
project_dir = project_path.parent.resolve()

try:
    tree = ET.parse(project_path)
except ET.ParseError as exc:
    sys.stderr.write(f"Warning: Failed to parse {project_path}: {exc}\n")
    sys.exit(0)

wildcards = set('*?[')
referenced = set()

def has_wildcard(text: str) -> bool:
    return any(ch in text for ch in wildcards)

for node in tree.iter():
    tag = node.tag.split('}', 1)[-1]
    if tag != 'Compile':
        continue
    for attr in ('Include', 'Update'):
        value = node.get(attr)
        if not value:
            continue
        normalized = value.strip().replace('\\', '/')
        if not normalized or '$(' in normalized:
            continue
        candidate = project_dir / normalized
        paths = []
        if has_wildcard(normalized):
            paths = list(project_dir.glob(normalized))
        else:
            paths = [candidate]
        if not paths:
            paths = [candidate]
        for resolved in paths:
            try:
                relative = resolved.resolve(strict=False).relative_to(project_dir)
            except ValueError:
                # Skip files that are outside the project directory tree.
                continue
            referenced.add(relative.as_posix())

for entry in sorted(referenced):
    print(entry)
PY
  )

  declare -A REFERENCED_MAP=()
  for ITEM in "${REFERENCED_FILES[@]}"; do
    [[ -z "$ITEM" ]] && continue
    REFERENCED_MAP["$ITEM"]=1
  done

  # Scan the project directory for every .fs file that physically exists on disk.
  declare -a OBSOLETE_FILES=()
  TOTAL_FS_FILES=0
  while IFS= read -r -d '' RELATIVE_TO_PROJECT; do
    ((TOTAL_FS_FILES++))
    RELATIVE_TO_PROJECT=${RELATIVE_TO_PROJECT#./}
    if [[ -n ${REFERENCED_MAP[$RELATIVE_TO_PROJECT]:-} ]]; then
      continue
    fi

    if [[ -n "$PROJECT_DIR_CLEAN" ]]; then
      ROOT_REL="$PROJECT_DIR_CLEAN/$RELATIVE_TO_PROJECT"
    else
      ROOT_REL="$RELATIVE_TO_PROJECT"
    fi
    ROOT_REL=${ROOT_REL#./}
    OBSOLETE_FILES+=("$ROOT_REL")
  done < <(find "$PROJECT_DIR" -type f -name '*.fs' -printf '%P\0')

  if [[ ${#REFERENCED_FILES[@]} -eq 0 ]]; then
    echo "No referenced .fs files were found in the project file (possible SDK-style defaults)."
  fi

  if [[ ${#OBSOLETE_FILES[@]} -eq 0 ]]; then
    echo "No obsolete .fs files detected in $PROJECT_DISPLAY."
    echo "Referenced entries: ${#REFERENCED_FILES[@]} | Files on disk: $TOTAL_FS_FILES"
    continue
  fi

  echo "Potentially obsolete files:"
  declare -a TRACKED_TO_REMOVE=()
  declare -a SKIPPED_UNTRACKED=()

  for ROOT_REL in "${OBSOLETE_FILES[@]}"; do
    if git -C "$REPO_ROOT" ls-files --error-unmatch -- "$ROOT_REL" >/dev/null 2>&1; then
      TRACKED_TO_REMOVE+=("$ROOT_REL")
      echo "  tracked: $ROOT_REL"
    else
      SKIPPED_UNTRACKED+=("$ROOT_REL")
      echo "  untracked (skipped): $ROOT_REL"
    fi
  done

  if [[ ${#TRACKED_TO_REMOVE[@]} -gt 0 ]]; then
    OVERALL_TRACKED=$((OVERALL_TRACKED + ${#TRACKED_TO_REMOVE[@]}))
    if [[ "$DRY_RUN" == "1" ]]; then
      for FILE in "${TRACKED_TO_REMOVE[@]}"; do
        echo "  [dry-run] git rm -- $FILE"
      done
    else
      for FILE in "${TRACKED_TO_REMOVE[@]}"; do
        git -C "$REPO_ROOT" rm -- "$FILE"
        OVERALL_REMOVED=$((OVERALL_REMOVED + 1))
      done
    fi
  fi

  if [[ ${#SKIPPED_UNTRACKED[@]} -gt 0 ]]; then
    echo "Skipped ${#SKIPPED_UNTRACKED[@]} untracked file(s)."
  fi

  echo "Referenced entries: ${#REFERENCED_FILES[@]} | Files on disk: $TOTAL_FS_FILES | Tracked obsolete: ${#TRACKED_TO_REMOVE[@]}"
done

if [[ "$DRY_RUN" == "1" ]]; then
  echo "\nDry-run summary: $OVERALL_TRACKED tracked file(s) would be removed."
else
  echo "\nRemoval summary: $OVERALL_REMOVED tracked file(s) were removed."
fi
