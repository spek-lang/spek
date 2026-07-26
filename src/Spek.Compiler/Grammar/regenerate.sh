#!/usr/bin/env bash
# Regenerate the ANTLR4 C# parser/lexer/visitor from the .g4 grammar files.
# Requires: Java 11+ on PATH and antlr4-complete.jar in the same directory.
#
# Usage:
#   cd src/Spek.Compiler/Grammar
#   ./regenerate.sh
#
# On Windows without Java on PATH, point JAVA to a JRE, e.g.:
#   JAVA="$LOCALAPPDATA/JetBrains/Toolbox/bin/jre/bin/java.exe" ./regenerate.sh

JAVA="${JAVA:-java}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAR="$SCRIPT_DIR/antlr4-complete.jar"

if [ ! -f "$JAR" ]; then
  echo "Downloading ANTLR4 4.13.1 complete JAR..."
  curl -L -o "$JAR" "https://www.antlr.org/download/antlr-4.13.1-complete.jar"
fi

# Run from the Grammar directory with bare filenames so the generated
# headers carry relative paths, not this machine's absolute ones (the
# Generated/*.cs files are checked in).
cd "$SCRIPT_DIR"
"$JAVA" -jar "$JAR" \
  -Dlanguage=CSharp \
  -package "Spek.Compiler.Grammar" \
  -visitor \
  -no-listener \
  -o Generated \
  SpekLexer.g4 \
  SpekParser.g4

echo "Done. Generated files are in Grammar/Generated/"
