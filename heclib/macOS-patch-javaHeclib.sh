#!/bin/bash
set -euo pipefail

# Load env variables
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Paths
OUTPUT_DIR="$SCRIPT_DIR/Output"
LIB_DIR="$OUTPUT_DIR/Patched"
GFORTRAN="$GFORTRAN_PATH"

echo "Copying libjavaHeclib.dylib to $LIB_DIR..."
mkdir $LIB_DIR
cp "$OUTPUT_DIR/libjavaHeclib.dylib" "$LIB_DIR/"

echo "Copying dependent dylibs from $GFORTRAN..."
cp "$GFORTRAN/libgfortran.5.dylib" "$LIB_DIR/"
cp "$GFORTRAN/libquadmath.0.dylib" "$LIB_DIR/"
cp "$GFORTRAN/libgcc_s.1.1.dylib" "$LIB_DIR/"

echo "Patching libjavaHeclib.dylib..."
install_name_tool -id "libjavaHeclib.dylib" "$LIB_DIR/libjavaHeclib.dylib"
install_name_tool -change "$GFORTRAN/libgfortran.5.dylib" "@loader_path/libgfortran.5.dylib" "$LIB_DIR/libjavaHeclib.dylib"

echo "Patching libgfortran.5.dylib..."
install_name_tool -id "libgfortran.5.dylib" "$LIB_DIR/libgfortran.5.dylib"
install_name_tool \
  -change "@rpath/libquadmath.0.dylib" "@loader_path/libquadmath.0.dylib" \
  -change "@rpath/libgcc_s.1.1.dylib" "@loader_path/libgcc_s.1.1.dylib" \
  "$LIB_DIR/libgfortran.5.dylib"

echo "Patching libquadmath.0.dylib..."
install_name_tool -id "@rpath/libquadmath.0.dylib" "$LIB_DIR/libquadmath.0.dylib"

echo "Patching libgcc_s.1.1.dylib..."
install_name_tool -id "@rpath/libgcc_s.1.1.dylib" "$LIB_DIR/libgcc_s.1.1.dylib"

echo "Done. New linkage:"
otool -L "$LIB_DIR/libjavaHeclib.dylib"