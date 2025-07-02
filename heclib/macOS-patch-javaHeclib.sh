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
install_name_tool -id "@rpath/libjavaHeclib.dylib" "$LIB_DIR/libjavaHeclib.dylib"
install_name_tool -change "$GFORTRAN/libgfortran.5.dylib" "@rpath/libgfortran.dylib" "$LIB_DIR/libjavaHeclib.dylib"
install_name_tool -add_rpath "@loader_path" "$LIB_DIR/libjavaHeclib.dylib"

echo "Patching libgfortran.5.dylib..."
install_name_tool -id "@rpath/libgfortran.5.dylib" "$LIB_DIR/libgfortran.5.dylib"

echo "Patching libquadmath.0.dylib..."
install_name_tool -id "@rpath/libquadmath.0.dylib" "$LIB_DIR/libquadmath.0.dylib"

echo "Patching libgcc_s.1.1.dylib..."
install_name_tool -id "@rpath/libgcc_s.1.1.dylib" "$LIB_DIR/libgcc_s.1.1.dylib"

echo "Done. New linkage:"
otool -L "$LIB_DIR/libjavaHeclib.dylib"
otool -L "$LIB_DIR/libgfortran.5.dylib"
otool -L "$LIB_DIR/libquadmath.0.dylib"
otool -L "$LIB_DIR/libgcc_s.1.1.dylib"

echo "Zipping javaHeclib..."
# Create a folder named javaHeclib and move patched libs into it
ZIP_ROOT="$LIB_DIR/javaHeclib"
mkdir -p "$ZIP_ROOT"
cp "$LIB_DIR/libjavaHeclib.dylib" "$ZIP_ROOT"
cp "$LIB_DIR/libgfortran.5.dylib" "$ZIP_ROOT"
cp "$LIB_DIR/libquadmath.0.dylib" "$ZIP_ROOT"
cp "$LIB_DIR/libgcc_s.1.1.dylib" "$ZIP_ROOT"
# Create the zip with javaHeclib as the top-level folder
cd "$LIB_DIR"
zip -r "javaHeclib.zip" "javaHeclib"

# Show contents for confirmation
pwd && ls -l