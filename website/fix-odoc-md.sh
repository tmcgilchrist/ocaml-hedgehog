#!/usr/bin/env bash
# Post-process odoc-generated markdown for Astro/Starlight compatibility.
# Adapted from davesnx/parseff.
#
# Usage: fix-odoc-md.sh <file.md>

set -euo pipefail

file="$1"

if [ ! -f "$file" ]; then
  echo "Error: $file not found" >&2
  exit 1
fi

# Work on a temp file to avoid partial writes
tmp="${file}.tmp"

sed \
  -e 's/\\\=/=/g' \
  -e 's/\\\_/_/g' \
  -e 's/\\->/\->/g' \
  -e 's|\.\./\.\./||g' \
  "$file" > "$tmp"

mv "$tmp" "$file"
