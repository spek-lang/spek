#!/usr/bin/env bash
# One-command demo runner: ./demos/run.sh fleet [--readings N] [--devices D] [--poison-every K]
set -euo pipefail
cd "$(dirname "$0")"

demo="${1:-}"
if [ -z "$demo" ] || [ ! -d "$demo" ]; then
  echo "usage: ./demos/run.sh <demo> [args...]" >&2
  echo "demos: $(ls -d */ 2>/dev/null | tr -d '/' | tr '\n' ' ')" >&2
  exit 1
fi
shift

# The benchmark suite runs Release (BenchmarkDotNet requires it).
if [ "$demo" = "benchmarks" ]; then
  exec dotnet run -c Release --project benchmarks/Demo.Benchmarks -- "$@"
fi

# The throughput demo measures the runtime at full tilt — always Release.
if [ "$demo" = "throughput" ]; then
  exec dotnet run -c Release --project throughput/Throughput.Harness -- "$@"
fi

harness=$(ls -d "$demo"/*.Harness 2>/dev/null | head -1)
exec dotnet run --project "$harness" -- "$@"
