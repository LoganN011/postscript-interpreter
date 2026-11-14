#!/usr/bin/env bash
# ==============================================================================
# Batch PostScript to SVG Renderer
#
# Usage:
#   ./render_all_svg.sh <input_folder> [output_folder]
#
# Examples:
#   ./render_all_svg.sh test/working_graphics ./svg_output
#   ./render_all_svg.sh ../Examples ./all_svgs
# ==============================================================================

set -e

# Resolve script directory (where postscript-interpreter project lives)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Verify arguments
if [ -z "$1" ]; then
  echo "Usage: $0 <input_folder> [output_folder]"
  echo ""
  echo "Arguments:"
  echo "  <input_folder>   Directory containing .ps files (searched recursively)"
  echo "  [output_folder]  Destination directory for .svg files (default: <input_folder>/svg_output)"
  exit 1
fi

INPUT_DIR="$(cd "$1" 2>/dev/null && pwd || echo "$1")"
if [ ! -d "$INPUT_DIR" ]; then
  echo "Error: Input directory '$1' does not exist."
  exit 1
fi

if [ -n "$2" ]; then
  OUTPUT_DIR="$2"
else
  OUTPUT_DIR="${INPUT_DIR}/svg_output"
fi

mkdir -p "$OUTPUT_DIR"
OUTPUT_DIR="$(cd "$OUTPUT_DIR" && pwd)"

# Locate compiled postscript-interpreter executable
INTERP_BIN=$(find "$SCRIPT_DIR/dist-newstyle" -type f -name "postscript-interpreter" -executable 2>/dev/null | head -n 1)

if [ -z "$INTERP_BIN" ] || [ ! -x "$INTERP_BIN" ]; then
  echo "Interpreter binary not found. Building with cabal..."
  (cd "$SCRIPT_DIR" && cabal build exe:postscript-interpreter)
  INTERP_BIN=$(find "$SCRIPT_DIR/dist-newstyle" -type f -name "postscript-interpreter" -executable 2>/dev/null | head -n 1)
fi

if [ -z "$INTERP_BIN" ] || [ ! -x "$INTERP_BIN" ]; then
  echo "Error: Could not locate or build postscript-interpreter executable."
  exit 1
fi

echo "=========================================================="
echo " PostScript to SVG Batch Renderer"
echo "=========================================================="
echo " Interpreter : $INTERP_BIN"
echo " Input Dir   : $INPUT_DIR"
echo " Output Dir  : $OUTPUT_DIR"
echo "=========================================================="

# Discover all .ps files
mapfile -t PS_FILES < <(find "$INPUT_DIR" -type f \( -name "*.ps" -o -name "*.PS" \) | sort)
TOTAL=${#PS_FILES[@]}

if [ "$TOTAL" -eq 0 ]; then
  echo "No .ps files found in '$INPUT_DIR'."
  exit 0
fi

echo "Found $TOTAL .ps file(s) to render."
echo ""

SUCCESS_COUNT=0
FAIL_COUNT=0
INDEX=1

for ps_file in "${PS_FILES[@]}"; do
  rel_name="$(basename "$ps_file")"
  base_name="${rel_name%.*}"
  out_base="${OUTPUT_DIR}/${base_name}"
  out_file="${out_base}.svg"

  printf "[%3d/%3d] Rendering %-35s ... " "$INDEX" "$TOTAL" "$rel_name"

  # Run interpreter with -o output argument
  if "$INTERP_BIN" "$ps_file" -o "$out_file" 2>/dev/null; then
    # Check if multi-page files were produced
    pages_found=$(ls -1 "${out_base}"_*.svg 2>/dev/null || true)
    if [ -n "$pages_found" ]; then
      num_pages=$(echo "$pages_found" | wc -l)
      echo "OK ($num_pages pages -> ${base_name}_1.svg .. ${base_name}_${num_pages}.svg)"
      SUCCESS_COUNT=$((SUCCESS_COUNT + num_pages))
    elif [ -s "$out_file" ] && grep -q "<svg" "$out_file"; then
      echo "OK (1 page -> ${base_name}.svg)"
      SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    else
      echo "EMPTY/NO GRAPHICS"
      rm -f "$out_file"
      FAIL_COUNT=$((FAIL_COUNT + 1))
    fi
  else
    echo "FAILED"
    rm -f "$out_file" "${out_base}"_*.svg 2>/dev/null || true
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi

  INDEX=$((INDEX + 1))
done

echo ""
echo "=========================================================="
echo " Rendering Complete!"
echo " Total files processed : $TOTAL"
echo " Successfully created  : $SUCCESS_COUNT SVG(s)"
if [ "$FAIL_COUNT" -gt 0 ]; then
  echo " Skipped / Failed      : $FAIL_COUNT"
fi
echo " Output location       : $OUTPUT_DIR"
echo "=========================================================="
