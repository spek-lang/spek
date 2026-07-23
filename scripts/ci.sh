#!/usr/bin/env bash
#
# Local mirror of .github/workflows/ci.yml — the correctness gate (build + test).
# Run it before pushing; CI runs the same steps. Requires the .NET 10 SDK.
#
#   ./scripts/ci.sh
#
set -euo pipefail
cd "$(dirname "$0")/.."   # repo root

echo "==> dotnet --version"
dotnet --version

echo "==> restore"
dotnet restore src/Spek.slnx

echo "==> build"
dotnet build src/Spek.slnx --no-restore --nologo

echo "==> test"
dotnet test src/Spek.slnx --no-build --nologo --verbosity normal

echo "==> samples"
for p in samples/*/*.csproj; do
  dotnet build "$p" --nologo -v q
done

echo "✓ CI passed"
