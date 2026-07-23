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

"$JAVA" -jar "$JAR" \
  -Dlanguage=CSharp \
  -package "Spek.Compiler.Grammar" \
  -visitor \
  -no-listener \
  -o "$SCRIPT_DIR/Generated" \
  "$SCRIPT_DIR/SpekLexer.g4" \
  "$SCRIPT_DIR/SpekParser.g4"

echo "Done. Generated files are in Grammar/Generated/"
